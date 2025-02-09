# R script to identify and record marks on the UO Score Answer form

# Clear Environment and Console
rm(list = ls())
cat(rep("\n", 100))

# Load Libraries
library(magick)
library(EBImage)
library(pdftools)
library(collapse)

# Set Parameters for UO Score Answer Form
num_ident_fields <- 4 # Assumes Last Name, First Name, UO Student ID, and Form Version Number is Captured 
length_SID <- 9 # Length of UO Student ID
length_form_number <- 2 # Maximum number of digits in form version number
length_question_option <- 5 # Maximum Number of MC Question Options
length_alphabet <- 26 # Number of Options for Last Name and First Name Letter Entries
length_last_name <- 15 # Maximum length of last name entry
length_first_name <- 10 # Maximum length of first name entry

# Set Exam Specific Parameters
input_file_name_path <- "./Input_UO_Score/Midterm 1 Scantrons.pdf" # Location of single pdf file holding scanned forms to be processed. One page per form
output_image_path <- "./Output_UO_Score/individual_form_scans" # Location of directory to hold scanned image of each processed form
output_file_path <- "./Output_UO_Score/Recorded_Scantron_Output.csv"
num_forms <- pdf_info(input_file_name_path)$pages # Number of exams to be graded. Assumes one scanned form per page in pdf file
num_questions <- 50 # Number of questions on the exam, must be 120 or less
blank_threshold <- 0.10  # Threshold to detect blanks - must be between 0 and 1. 
                        # Lower numbers will detect less blanks, but may assign a random answer when none is given.
                        # Higher numbers will detect more blanks, but may miss more legitimate answers
mult_answer_threshold <- 0.05  # Threshold to detect multiple answers
                              # Lower numbers will detect less multiple answers
                              # Higher numbers will detect more multiple answers
fail_file_path <- "./Output_UO_Score/Forms_Not_Processed.txt" # Name of file to output forms that were not processed for various reasons

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

for (page_num in 1:num_forms) { 

  form_count <- form_count + 1
  
  image_pdftools <- pdf_convert(input_file_name_path, pages=page_num, filenames = "temp.png", dpi = 600)
  
  cat(rep("\n", 100))
  print_update <- paste("Processing for", page_num, "of", num_forms)
  print(print_update)
  
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
    write(print_update,file=fail_file_path,append=TRUE)
    next
  }
  
  check_col_max <- sum(as.numeric((gray_image_EBImage[col_max,1:H]<threshold)))
  if (check_col_max>25) {
    print_update <- paste("col_max may not contain all guides, or may include other parts of form, for form", page_num)
    write(print_update,file=fail_file_path,append=TRUE)
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
    write(print_update,file=fail_file_path,append=TRUE)
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
  
  store_vec <- as.matrix(c(Last_Name_str, First_Name_str, SID_number_onenum, Form_number_onenum, Answers))
  store_mat <- cbind(store_mat, store_vec)
  
  output_file <- file.path(output_image_path, paste0(Last_Name_str, "_", First_Name_str, "_", SID_number_onenum, ".png"))
  
  image_write(image, output_file)
  
 # file.remove("temp.png")
  
  successful_form_count = successful_form_count+1

}

file.remove("temp.png")

store_vec_labels <- as.matrix(c("Last_Name", "First_Name", "Student_ID", "Form Number", (1:1:num_questions)))
store_mat <- t(store_mat)
store_mat <- as.data.frame(store_mat);
names(store_mat) <- t(store_vec_labels)

write.csv(store_mat, output_file_path, row.names = FALSE)

print_update1 <- paste("Processing Complete.", form_count, "forms attempted to be processed.")
print_update2 <- paste("There were", form_count-successful_form_count, "forms unable to be processed.")
cat(rep("\n", 100))
print(print_update1)
print(print_update2)