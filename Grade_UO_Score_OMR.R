# Grade Exams Based on Output File from UO_Score_OMR.R

# Clear Environment and Console
rm(list = ls())
cat(rep("\n", 100))

# Load Libraries
library(stringr)

# Set number of versions. 
# Assumption is that versions are indicated by ascending integers starting at 1, 
# and this order is maintained in the solution key file. 
num_versions=2

if (num_versions>1) {
  version_numbers = t((1:1:num_versions))
}

Answer_Key = read.csv("./Input_Grade_UO_Score/Midterm_1_Key.csv") # Assumes first column holds questions numbers and each subsequent column is solutions for a particular form
Answer_Key = as.data.frame(Answer_Key[,2:(num_versions+1)])
Student_Answers = read.csv("./Input_Grade_UO_Score/Recorded_Scantron_Output.csv") # Output file from UO_Score_OMR.R

output_scores <- "./Output_Grade_UO_Score/Scores.csv"
answer_issues <- "./Output_Grade_UO_Score/Answer_Issues.txt" # Name of file to output forms with missing answers, or multiple answers for one question

if (num_versions>1) {
  wrong_form <- "./Output_Grade_UO_Score/Probable_Wrong_Form.txt" # Name of file to output students who likely recorded the wrong form
  no_form <- "./Output_Grade_UO_Score/No_Form_Selected.txt" # Name of file to to output students who did not record an exam version
}

# Initialize storage matrix
store_grades <- matrix(nrow=dim(Student_Answers)[1],ncol=3)

for (i in 1:dim(Student_Answers)[1]) {
  
  answers_i = t(Student_Answers[i,5:(dim(Student_Answers)[2])])
  last_name_i = Student_Answers[i,1]
  first_name_i  = Student_Answers[i,2]
  sid_i = Student_Answers[i,3]

  if ("BLANK" %in% answers_i) {
    blank_answers <- which(answers_i=="BLANK")
    print_answers <- paste(blank_answers,collapse=",")
    print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
    print_update = paste("Blank answers found for:",print_name, "- Question",print_answers)
    write(print_update,file=answer_issues,append=TRUE)
    write("",file=answer_issues,append=TRUE)
  }
  
  if (TRUE %in% (str_length(answers_i)>1)) {
    mult_answers <- which(str_length(answers_i)>1 & (answers_i!="BLANK"))
    if (length(mult_answers)>0) {
      print_answers <- paste(mult_answers,collapse=",")
      print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i)
      print_update = paste("Multiple answers found for:",print_name, "- Question",print_answers)
      write(print_update,file=answer_issues,append=TRUE)
      write("",file=answer_issues,append=TRUE)
    }
  }
  
  if (num_versions==1) {
    key_form_i = as.data.frame(Answer_Key[,1])
  }

  if (num_versions>1) {
    form_i = Student_Answers[i,4]
    
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
      print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i,".")
      print_update = paste0("No version selected by ",print_name," Maximum score was ",max_score_i," with version number ",max_form_i,".")
      write(print_update,file=no_form,append=TRUE)
      write("",file=no_form,append=TRUE)
      
      score_i = max_score_i
      
      store_grades_i <- as.matrix(c(last_name_i, sid_i, score_i))
      store_grades[i,] <- t(store_grades_i)
      
      next
      
    }
    
  }
    
  score_i = sum(answers_i==key_form_i)
  
  if (num_versions>1) {
    
    if ((score_i/(dim(answers_i)[1])) < 0.5) {
        
      key_form_noti = as.data.frame(Answer_Key[,version_numbers!=form_i])
        
      alt_scores = matrix(0,(num_versions-1),2)
      alt_scores[,2] = t(version_numbers[version_numbers!=form_i])
        
      for (j in 1:(num_versions-1)) {
          
        scores_noti = sum(answers_i==key_form_noti[,j])
        alt_scores[j,1] = c(scores_noti)
          
      }
        
      if (max(alt_scores[,1])>score_i) {
        max_score_i = max(alt_scores[,1])
        max_form_i = alt_scores[which.max(alt_scores[,1]),2]
        print_name <- paste0(last_name_i,", ",first_name_i,", ",sid_i,".")
        print_update = paste0("Probable wrong version selected by ",print_name," Score with selected version number ",form_i," was ",score_i,"."," Score with version number ",max_form_i," was ",max_score_i,".")
        write(print_update,file=wrong_form,append=TRUE)
        write("",file=wrong_form,append=TRUE)
      }
        
    }
      
  }
  
  store_grades_i <- as.matrix(c(last_name_i, sid_i, score_i))
  store_grades[i,] <- t(store_grades_i)
    
}

store_grade_labels <- as.matrix(c("Student", "SIS User ID", "Score"))
store_grades <- as.data.frame(store_grades);
names(store_grades) <- store_grade_labels

write.csv(store_grades, output_scores, row.names = FALSE)