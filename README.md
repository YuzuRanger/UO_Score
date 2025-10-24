# UO_Score
UO_Score_OMR.R: R script to identify and record answers from images of the UO Score Answer Form

Grade_UO_Score_OMR.R: R script to produce scores and identify problematic student responses using output from UO_Score_OMR.R

> [!TIP]
> If you want to use the UO Score Application with a GUI, [go to DesktioDeployR UO Score instead](https://github.com/YuzuRanger/DesktopDeployR/tree/ScoreApp).

## Setting Up in VS Code
ℹ️ Follow these steps: [R in Visual Studio Code](https://code.visualstudio.com/docs/languages/r)  
When choosing mirrors, you can choose US(OR) which is the Oregon State University Open Source Lab!

📚 In your VS Code R Terminal, install the necessary libraries:
```
install.packages("magick")
```
```
install.packages("BiocManager")
BiocManager::install("EBImage")
```
```
install.packages("pdftools")
```
```
install.packages("collapse")
```
```
install.packages("svDialogs")
```

## User Guide
### Step 1: UO_Score_OMR.R
R Script to Process UO Score Answer Form  
The R script UO_Score_OMR.R identifies and records entries on the front page of the “UO Score Answer Form” custom bubble form. The script will identify:  
1)	First Name
2)	Last Name
3)	UO ID Number
4)	Form Version Number
5)	Answers to multiple choice questions from question 1 to a maximum of 120
  
The script will produce:
1)	A single .csv ﬁle holding all student responses.
2)	A single .txt ﬁle identifying forms that could not be processed (this ﬁle is not produced if there are no such ﬁles).
3)	If desired, a single .png ﬁle for each student holding an image of their scanned UO Score Answer Form, with ﬁlename given by the student’s name and UO ID number. Users will be prompted to decide whether these ﬁles should be created.
   
To use the script, follow these steps:
1)	Create a single .pdf ﬁle holding the front page of all the UO Score Answer Forms that you wish to process. Each page of this ﬁle should be a separate form (one sided, not including the back page).
    - The script was tested with scans produced by a Xerox AltaLink C8155 copier, using the default settings.
2)	Run the script in its entirety according to your environment.
    - In RStudio, select all text and click “Run.” 
    - In Visual Studio Code, click “▶Run Source” or use Ctrl+Shift+S.


### Step 2: Grade_UO_Score_OMR.R
R Script to Grade UO Score Answer Form Based on Output from UO_Score_OMR.R  
  
The R script Grade_UO_Score_OMR.R produces scores and identiﬁes potential student entry issues on the UO Score Answer Form. This script uses output produced in a ﬁrst step by the separate R script UO_Score_OMR.R. The script will produce the following output ﬁles:  
1)	A single .csv ﬁle holding, for each student: 1) last name, 2) UO ID number, 3) exam score, and 4) the page number from the master ﬁle holding all scanned exams.
2)	A single .txt ﬁle reporting irregular student responses, including:
    - Questions where a student entered no answer
    - Questions where a student entered multiple answers.
    - The identity of students who did not enter a form number.
    - The identity of students who potentially entered the wrong form number.
  
To use the script, follow these steps:
1)	Run the UO_Score_OMR.R script to produce a .csv ﬁle holding the recorded student responses.
2)	Create a .csv ﬁle holding the answer key for the different versions of the exam. The ﬁrst column should hold question numbers beginning from 1. Each subsequent column should hold solutions based on different exam versions. It is assumed that exam versions are indicated by students on the UO Score Answer Form in Box 5 (“TEST”) with integers starting at 1, and the columns of this ﬁle hold the solutions for different versions in numerical order.  
That is, solutions for exam version “1” should be in column 2, solutions for exam version “2” should be in column 3, etc. If there is only one version of the exam the solutions for this exam should be in column 2. In this case, any student entries on the UO Score answer form in box 5 are ignored.  
Be sure to include a header on your answer key, such as A1: *Question Number* B1: *Form 1* C1: *Form 2* in your spreadsheet.
3)	Run the script Grade_UO_Score_OMR.R according to your environment.
    - In RStudio, select all text and click “Run.” 
    - In Visual Studio Code, click “▶Run Source” or use Ctrl+Shift+S.
