library(shiny)
library(shinyjs)
# Load Libraries
library(magick)
library(EBImage)
library(pdftools)
library(collapse)
library(svDialogs)
library(stringr)

# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel("Upload Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("scanned_pdf", "Select .pdf file that holds scans of forms:",
                multiple = FALSE,
                accept = c(".pdf")),
    
      # Input: Enter an integer ----
      numericInput(
                inputId = "num_questions", 
                label = "Enter number of questions on the exam, must be 120 or less:",
                value = 10, 
                min = 1, 
                max = 120, 
                step = 1
            ),

          # Input: Enter an integer ----
      numericInput(
                inputId = "num_versions", 
                label = "Enter the number of test versions:",
                value = 1, 
                min = 1, 
                max = 10, 
                step = 1
            ),
    
          # Input: Select a file ----
      fileInput("answer_key", "Select .csv file that holds answer key for exam:",
                multiple = FALSE,
                accept = c(".csv")),

      # Horizontal line ----
      tags$hr(),
      

      # Button
      actionButton("processData", "Process Data")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("updates"),
      textOutput("fail_list"),
      shinyjs::hidden(downloadButton("downloadData", "Download Processed Student Answers")),
      shinyjs::hidden(downloadButton("downloadScores", "Download Processed Student Scores")),
      shinyjs::hidden(downloadButton("downloadAlerts", "Download Alerts/Irregularities in Student Responses"))

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
    # input$scanned_pdf will be NULL initially. 
    # Require file and number of questions
    observeEvent(input$processData, {
    req(input$scanned_pdf)
    req(input$num_questions)
    req(input$answer_key)
    req(input$num_versions)

    shinyjs::html(id = "updates", html = "Starting...", add = TRUE)
    shinyjs::html(id = "updates", html = "<br>", add = TRUE)

    # Set Parameters for UO Score Answer Form
    num_ident_fields <- 5 # Assumes page number, Last Name, First Name, UO Student ID, and Form Version Number is Captured 
    length_SID <- 9 # Length of UO Student ID
    length_form_number <- 2 # Maximum number of digits in form version number
    length_question_option <- 5 # Maximum Number of MC Question Options
    length_alphabet <- 26 # Number of Options for Last Name and First Name Letter Entries
    length_last_name <- 15 # Maximum length of last name entry
    length_first_name <- 10 # Maximum length of first name entry

    # Number of exams to be graded. Assumes one scanned form per page in pdf file
    pdf_path <- input$scanned_pdf$datapath
    num_forms <- pdf_info(pdf_path)$pages
    # Number of questions on exam
    num_questions <- as.numeric(input$num_questions)

    # Threshold to detect blanks - must be between 0 and 1. 
    # Lower numbers will detect less blanks, but may assign a random answer when none is given.
    # Higher numbers will detect more blanks, but may miss more legitimate answers
    blank_threshold <- 0.10

    # Threshold to detect multiple answers
    # Lower numbers will detect less multiple answers
    # Higher numbers will detect more multiple answers
    mult_answer_threshold <- 0.05

    # List to hold forms that were not processed for various reasons
    fail_list <- list() 

    # Initialize storage matrix
    num_rows <- num_ident_fields + num_questions; 
    store_mat <- matrix(nrow=num_rows,ncol=0)

    # Define function that will identify marks
    mark_identify <- function(row_start,row_end,col_start,col_end,study_image,threshold){
    threshold_image_check = study_image[row_start:row_end,col_start:col_end];
    num_pixels = (dim(threshold_image_check)[1])*(dim(threshold_image_check)[2]);
    percent_filled = sum(threshold_image_check)/num_pixels;
    return(percent_filled);
    }

    form_count <- 0 # counter to count the number of forms processed. Useful if processing a subset of main file. 
    successful_form_count <- 0 # counter to count the number of forms sucessfully processed.

    # --- BEGIN PROCESSING ---
    for (page_num in 1:num_forms) {

    form_count <- form_count + 1
    
    image_pdftools <- pdf_convert(pdf_path, pages=page_num, filenames = "temp.png", dpi = 600)
    
    print_update <- paste("Processing for", page_num, "of", num_forms, "\n")
    print(print_update)
    shinyjs::html(id = "updates", html = print_update, add = TRUE)
    
    # Convert to magick image
    image <- image_read(image_pdftools)
    
    # Convert to grayscale (simplify the image for processing)
    gray_image <- image_convert(image, colorspace = 'gray')
    
    # Move from magick package to EBImage for remainder of analysis
    gray_image_EBImage <- as_EBImage(gray_image)
    
    # Calibrate Position of Left-Hand-Side Row Guides on UO Score
    
    # Pick left and right column indicators that you think will contain all the guides
    col_min <- 51
    col_max <- 250
    W <- col_max-(col_min-1)
    H <- dim(gray_image_EBImage)[2]
    
    # Determine a threshold to identify guide marks
    sorted_image <- sort(c(gray_image_EBImage[1:W,1:H]))
    max_diff_loc <- which.max(sorted_image[2:(W*H)]-sorted_image[1:((W*H)-1)])
    threshold <- mean(sorted_image[max_diff_loc:(max_diff_loc+1)])
    
    # Check col_min and col_max to make sure they contain all guides
    check_col_min <- sum(as.numeric((gray_image_EBImage[col_min,1:H]<threshold)))
    if (check_col_min>25) {
        print_update <- paste("col_min may not contain all guides for form", page_num)
        fail_list = rbind(fail_list,print_update)
        fail_list = rbind(fail_list,"")
        next
    }
    
    check_col_max <- sum(as.numeric((gray_image_EBImage[col_max,1:H]<threshold)))
    if (check_col_max>25) {
        print_update <- paste("col_max may not contain all guides, or may include other parts of form, for form", page_num)
        fail_list = rbind(fail_list,print_update)
        fail_list = rbind(fail_list,"")
        next
    }
    
    # Identify possible position of guides
    all_dark <- which(gray_image_EBImage[col_min:col_max,1:H]<threshold)
    all_dark_coord <- matrix(NA, length(all_dark), 2)
    all_dark_coord[,1] <- (all_dark %/% W) + 1*((all_dark %% W)>0)
    all_dark_coord[,2] <- ((all_dark %% W)*((all_dark %% W)>0) + W*((all_dark %% W)==0)) + (col_min-1)
    
    all_dark_row_counts <- as.data.frame(table(all_dark_coord[,1]))
    
    all_dark_row_counts <- all_dark_row_counts[all_dark_row_counts[,2]>=70,]
    
    all_dark_coord = all_dark_coord[(all_dark_coord[,1] %in% all_dark_row_counts[,1]),]
    
    unique_row = unique(all_dark_coord[,1])
    
    if (length(unique_row)==0) {
        print_update <- paste("Wrong number of guides detected in form", page_num)
        fail_list = rbind(fail_list,print_update)
        fail_list = rbind(fail_list,"")
        next
    }

    guide_mat_temp <- matrix(NA, length(unique(all_dark_coord[,1])), 4)
    
    num_guides=1
    
    for (iter in 1:length(unique_row)) {
        
        sub_mat = all_dark_coord[(all_dark_coord[,1]==unique_row[iter]),]
        
        guide_mat_temp[iter,1] = unique_row[iter]
        guide_mat_temp[iter,2] = min(sub_mat[,2])
        guide_mat_temp[iter,3] = max(sub_mat[,2])
        
        if (iter>1) {
        if ((unique_row[iter]-unique_row[iter-1])>5) {
            num_guides = num_guides+1
        }
        }
        
        guide_mat_temp[iter,4] = num_guides
        
    }
    
    if (num_guides != 61) {
        print_update <- paste("Wrong number of guides detected in form", page_num)
        fail_list = rbind(fail_list,print_update)
        fail_list = rbind(fail_list,"")
        break
    }
    
    guide_mat = matrix(NA,num_guides,4)
    
    for (iter in 1:num_guides) {
        sub_mat = as.data.frame(guide_mat_temp[(guide_mat_temp[,4]==iter),1:3])
        guide_mat[iter,1] = min(sub_mat[,1])
        guide_mat[iter,2] = max(sub_mat[,1])
        guide_mat[iter,3] = fmode(sub_mat[,2])
        guide_mat[iter,4] = fmode(sub_mat[,3])
    }
    
    #  Mapping of Guides and Columns to Objects of Interest
    
    # Last Name: Guides #9-34; First Name: Guides #9-34; SID Number: Guides #15-24; 
    # Form Number: Guides #30-39; Questions in blocks of 20: Guides #42-61
    
    # Column guides - Column #1 is column before first "Last Name" column.
    # There are 46 total columns. 
    
    # Columns 1, 17 are blank; Columns 2-16 are Last Name; Columns 18-27 are First Name
    # Column 29 is Middle Initial; Columns 31-39 is SID; Columns 33-34 are Form Version Number
    # Columns 2-6 are Questions 1-20; Columns 10-14 are Questions 21-40; Columns 18-22 are Questions 41-60
    # Columns 26-30 are Questions 61-80; Columns 34-38 are Questions 81-100; Columns 42-46 are Questions 101-120
    
    # Capture Student ID Number
    SID_number <- rep(NA,length_SID);
    guides_temp <- c(15:24) # Relevant Row Guides
    cols_temp <- c(31:39) # Relevant Columns
    
    for (i in 1:length_SID){
        
        rank_mat = matrix(data=NA,nrow=10,ncol=1)
        
        for (j in 0:9) {
        left_m <- guide_mat[guides_temp[j+1],3]
        right_m <- guide_mat[guides_temp[j+1],4]
        top_m <- guide_mat[guides_temp[j+1],1]
        bottom_m <- guide_mat[guides_temp[j+1],2]
        offset <- cols_temp[i]*100 + 105
        rank_mat[(j+1),] <- mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
        }
        
        if (any((max(rank_mat)-rank_mat)>blank_threshold)==TRUE) {
        SID_number[i] <- (which.min(rank_mat[,1])-1);
        }

    }
    
    SID_number <- SID_number[!is.na(SID_number)]
    SID_number_onenum = paste(SID_number, collapse="")
    
    # Capture Form Version Number
    Form_number <- rep(NA,length_form_number)
    guides_temp <- c(30:39) # Relevant Row Guides
    cols_temp <- c(33:34) # Relevant Columns
    
    for (i in 1:length_form_number) {
        
        rank_mat = matrix(data=NA,nrow=10,ncol=1)
        
        for (j in 0:9){
        left_m <- guide_mat[guides_temp[j+1],3]
        right_m <- guide_mat[guides_temp[j+1],4]
        top_m <- guide_mat[guides_temp[j+1],1]
        bottom_m <- guide_mat[guides_temp[j+1],2]
        offset <- cols_temp[i]*100 + 105
        rank_mat[(j+1),] <- mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
        }
        
        if (any((max(rank_mat)-rank_mat)>blank_threshold)==TRUE) {
        Form_number[i] <- (which.min(rank_mat[,1])-1);
        }

    }
    
    Form_number <- Form_number[!is.na(Form_number)]  
    Form_number_onenum <- paste(Form_number,collapse="")

    # Capture Student Last Name
    Last_Name = rep(NA,length_last_name);
    guides_temp = c(9:34) # Relevant Row Guides
    cols_temp = c(2:16)
    
    for (i in 1:length_last_name){
        
        rank_mat = matrix(data=NA,nrow=length_alphabet,ncol=1)
        
        for (j in 1:length_alphabet) {
        left_m <- guide_mat[guides_temp[j],3]
        right_m <- guide_mat[guides_temp[j],4]
        top_m <- guide_mat[guides_temp[j],1]
        bottom_m <- guide_mat[guides_temp[j],2]
        offset <- cols_temp[i]*100 + 105
        rank_mat[j,] <- mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
        }
        
        if ((min(rank_mat[-which.min(rank_mat[,1]),1]))-min(rank_mat[,1])>0.05) {
        Last_Name[i] <- LETTERS[which.min(rank_mat[,1])];
        }
        
    }
    
    Last_Name <- Last_Name[!is.na(Last_Name)]
    Last_Name_str <- paste(Last_Name, collapse="")
    
    # Capture Student First Name
    First_Name <- rep(NA,length_first_name);
    guides_temp <- c(9:34) # Relevant Row Guides
    cols_temp <- c(18:27)
    
    for (i in 1:length_first_name){
        
        rank_mat = matrix(data=NA,nrow=length_alphabet,ncol=1)
        
        for (j in 1:length_alphabet) {
        left_m <- guide_mat[guides_temp[j],3]
        right_m <- guide_mat[guides_temp[j],4]
        top_m <- guide_mat[guides_temp[j],1]
        bottom_m <- guide_mat[guides_temp[j],2]
        offset <- cols_temp[i]*100 + 105
        rank_mat[j,] <- mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
        }
        
        if ((min(rank_mat[-which.min(rank_mat[,1]),1]))-min(rank_mat[,1])>0.05) {
        First_Name[i] <- LETTERS[which.min(rank_mat[,1])];
        }
        
    }
    
    First_Name <- First_Name[!is.na(First_Name)]
    First_Name_str <- paste(First_Name, collapse="")
    
    # Capture Student's Answers to Multiple Choice Questions
    
    Answers <- rep(NA,num_questions)
    guides_temp <- c(42:61) # Relevant Row Guides
    
    for (i in 1:num_questions) {
        
        if (i<=20){
        cols_temp <- c(2:6)
        top_m <- guide_mat[guides_temp[i],1]
        bottom_m <- guide_mat[guides_temp[i],2]
        left_m <- guide_mat[guides_temp[i],3]
        right_m <- guide_mat[guides_temp[i],4]
        }
        
        else if (i<=40) {
        cols_temp <- c(10:14)
        top_m <- guide_mat[guides_temp[(i-20)],1]
        bottom_m <- guide_mat[guides_temp[(i-20)],2]
        left_m <- guide_mat[guides_temp[(i-20)],3]
        right_m <- guide_mat[guides_temp[(i-20)],4]
        }
        
        else if (i<=60) {
        cols_temp <- c(18:22)
        top_m <- guide_mat[guides_temp[(i-40)],1]
        bottom_m <- guide_mat[guides_temp[(i-40)],2]
        left_m <- guide_mat[guides_temp[(i-40)],3]
        right_m <- guide_mat[guides_temp[(i-40)],4]
        }
        
        else if (i<=80) {
        cols_temp <- c(26:30)
        top_m <- guide_mat[guides_temp[(i-60)],1]
        bottom_m <- guide_mat[guides_temp[(i-60)],2]
        left_m <- guide_mat[guides_temp[(i-60)],3]
        right_m <- guide_mat[guides_temp[(i-60)],4]
        }
        
        else if (i<=100) {
        cols_temp <- c(34:38)
        top_m <- guide_mat[guides_temp[(i-80)],1]
        bottom_m <- guide_mat[guides_temp[(i-80)],2]
        left_m <- guide_mat[guides_temp[(i-80)],3]
        right_m <- guide_mat[guides_temp[(i-80)],4]
        }
        
        else if (i<=120) {
        cols_temp <- c(42:46)
        top_m <- guide_mat[guides_temp[(i-100)],1]
        bottom_m <- guide_mat[guides_temp[(i-100)],2]
        left_m <- guide_mat[guides_temp[(i-100)],3]
        right_m <- guide_mat[guides_temp[(i-100)],4]
        }
        
        rank_mat <- matrix(data=NA,nrow=length_question_option,ncol=1)
        
        for (j in 1:length_question_option){
        offset <- cols_temp[j]*100 + 105
        rank_mat[j,] <- mark_identify((left_m+offset),(right_m+offset),top_m,bottom_m,gray_image_EBImage)
        }
        
        if (any((max(rank_mat)-rank_mat)>blank_threshold)==FALSE) {
        Answers[i]="BLANK"
        } else {
        Answer_temp = LETTERS[which((rank_mat-min(rank_mat))<mult_answer_threshold)]
        Answers[i] = paste(Answer_temp, collapse = ",")
        }
        
    }
    
    store_vec <- as.matrix(c(form_count,Last_Name_str, First_Name_str, SID_number_onenum, Form_number_onenum, Answers))
    store_mat <- cbind(store_mat, store_vec)
    
    successful_form_count = successful_form_count+1

    }
    # --- END PROCESSING ---
    print_update <- paste("Processing completed.")
    print(print_update)
    shinyjs::html(id = "updates", html = print_update, add = TRUE)
    shinyjs::html(id = "updates", html = "<br>", add = TRUE)
    # output$updates <- renderPrint(
    #     print_update
    # )
    file.remove("temp.png")

    store_vec_labels <- as.matrix(c("Page_Number","Last_Name", "First_Name", "Student_ID", "Form Number", (1:1:num_questions)))
    store_mat <- t(store_mat)
    store_mat <- as.data.frame(store_mat);
    names(store_mat) <- t(store_vec_labels)

    # Downloadable csv of dataset ----
    temp_file <- tempfile(fileext = ".csv")
    write.csv(store_mat, temp_file, row.names = FALSE)
    # Store the path to the saved file (e.g., in a reactive value)
    # for later reading or downloading
    session$userData$saved_file_path <- temp_file

    output$downloadData <- downloadHandler(
    filename = function() {
        paste("StudentAnswers", Sys.Date(), ".csv", sep = "")
        },
    content = function(file) {
        file.copy(session$userData$saved_file_path, file)
        }
    )

    # --- BEGIN GRADING ---
    shinyjs::html(id = "updates", html = "Computing scores...", add = TRUE)
    num_versions <- as.numeric(input$num_versions)
    if (num_versions>1) {
        version_numbers = t((1:1:num_versions))
    }

    Answer_Key_path <- input$answer_key$datapath
    Answer_Key = read.csv(Answer_Key_path) # Assumes first column holds questions numbers and each subsequent column is solutions for a particular form
    Answer_Key = as.data.frame(Answer_Key[,2:(num_versions+1)])

    Student_Answers_path = session$userData$saved_file_path
    Student_Answers = read.csv(Student_Answers_path) # Output file from UO_Score_OMR.R



blank_list <- list()
multiple_list <- list()

if (num_versions>1) {
  wrong_form_list = list()
  no_form_list = list()
}

# Initialize storage matrix
store_grades <- matrix(nrow=dim(Student_Answers)[1],ncol=4)

for (i in 1:dim(Student_Answers)[1]) {
  
  answers_i = t(Student_Answers[i,6:(dim(Student_Answers)[2])])
  page_number_i = Student_Answers[i,1]
  last_name_i = Student_Answers[i,2]
  first_name_i  = Student_Answers[i,3]
  sid_i = Student_Answers[i,4]

  if ("BLANK" %in% answers_i) {
    blank_answers <- which(answers_i=="BLANK")
    print_answers <- paste(blank_answers,collapse=",")
    print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
    print_update1 = paste0("Blank answers found for: ",print_name," (page number ", page_number_i,").")
    print_update2 = paste("Question",print_answers)
    blank_list = rbind(blank_list,print_update1)
    blank_list = rbind(blank_list,print_update2)
    blank_list = rbind(blank_list,"")
  }
  
  if (TRUE %in% (str_length(answers_i)>1)) {
    mult_answers <- which(str_length(answers_i)>1 & (answers_i!="BLANK"))
    if (length(mult_answers)>0) {
      print_answers <- paste(mult_answers,collapse=",")
      print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
      print_update1 = paste0("Multiple answers found for: ", print_name, " (page number ", page_number_i, ").")
      print_update2 = paste("Question",print_answers)
      multiple_list = rbind(multiple_list,print_update1)
      multiple_list = rbind(multiple_list,print_update2)
      multiple_list = rbind(multiple_list,"")
    }
  }
  
  if (num_versions==1) {
    key_form_i = as.data.frame(Answer_Key[,1])
  }

  if (num_versions>1) {
    form_i = Student_Answers[i,5]
    
    if (is.na(form_i)==FALSE) {
      key_form_i = as.data.frame(Answer_Key[,form_i])
    }
    
    if (is.na(form_i)) {
      key_form_alli = as.data.frame(Answer_Key)
      alt_scores = matrix(0,num_versions,2)
      
      for (j in 1:num_versions) {
        
        scores_alli = sum(answers_i==key_form_alli[,j])
        alt_scores[j,] = c(scores_alli,j)
        
      }
      
      max_score_i = max(alt_scores[,1])
      max_form_i = version_numbers[which.max(alt_scores[,1])]
      print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
      print_update1 = paste0("No version selected by ", print_name, " (page number ", page_number_i, ").")
      print_update2 = paste0("Maximum score was ",max_score_i," with version number ",max_form_i,".")
      no_form_list = rbind(no_form_list,print_update1)
      no_form_list = rbind(no_form_list,print_update2)
      no_form_list = rbind(no_form_list,"")
      
      score_i = max_score_i
      
      store_grades_i <- as.matrix(c(last_name_i, sid_i, score_i, page_number_i))
      store_grades[i,] <- t(store_grades_i)
      
      next
      
    }
    
  }
    
  score_i = sum(answers_i==key_form_i)
  
  if (num_versions>1) {
    
    if ((score_i/(dim(answers_i)[1])) < 0.5) {
        
      key_form_noti = as.data.frame(Answer_Key[,version_numbers!=form_i])
        
      alt_scores = matrix(0,(num_versions-1),2)
      alt_scores
      alt_scores[,2] = t(version_numbers[version_numbers!=form_i])
      alt_scores
        
      for (j in 1:(num_versions-1)) {
          
        scores_noti = sum(answers_i==key_form_noti[,j])
        alt_scores[j,1] = c(scores_noti)
          
      }
        
      if (max(alt_scores[,1])>score_i) {
        max_score_i = max(alt_scores[,1])
        max_form_i = alt_scores[which.max(alt_scores[,1]),2]
        print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
        print_update1 = paste0("Probable wrong version selected by ",print_name," (Page Number ",page_number_i,").")
        print_update2 = paste0("Score with selected version number ",form_i," was ",score_i,"."," Score with version number ",max_form_i," was ",max_score_i,".")
        wrong_form_list = rbind(wrong_form_list,print_update1)
        wrong_form_list = rbind(wrong_form_list,print_update2)
        wrong_form_list = rbind(wrong_form_list,"")
        
        score_i = max_score_i
        
      }
        
    }
      
  }
  
  store_grades_i <- as.matrix(c(last_name_i, sid_i, score_i, page_number_i))
  store_grades[i,] <- t(store_grades_i)
    
}

store_grade_labels <- as.matrix(c("Student", "SIS User ID", "Score", "Page Number"))
store_grades <- as.data.frame(store_grades);
names(store_grades) <- store_grade_labels

    # Downloadable csv of dataset ----
    output$downloadScores <- downloadHandler(
    filename = function() {
        paste("StudentScores", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
        write.csv(store_grades, file, row.names = FALSE)
        }
    )

if (num_versions==1) {
  alerts_list <- rbind(blank_list,multiple_list)
}

if (num_versions>1) {
  alerts_list = rbind(blank_list,multiple_list,no_form_list,wrong_form_list)
}

    output$downloadAlerts <- downloadHandler(
    filename = function() {
        paste("StudentAlerts", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
        write.csv(unlist(alerts_list), file, row.names = FALSE)
        }
    )

    # --- DOWNLOADS ---
    shinyjs::show("downloadData")
    shinyjs::show("downloadScores")
    shinyjs::show("downloadAlerts")


    if (length(fail_list)!=0){
    output$fail_list <- renderText(
        unlist(fail_list)
    )
    }
    })

}

# Create Shiny app ----
shinyApp(ui, server)