{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fswiss\fcharset0 Arial;}}
{\*\generator Msftedit 5.41.21.2509;}\viewkind4\uc1\pard\f0\fs20 ##Getting and Cleaning data -Course project\par
\par
Function 'run_analysis' is created to compute the mean for different subject and different activities.\par
This function read the X,Y and subject files in both train and test directory.\par
After reading the files,it combines the train and test dataset into single data set using rbind() function.\par
Columns containing mean or standard deviaiton datas alone are filtered in X data set\par
This function also changes the default header(V1,V2..) into descriptive names using colnames() function.\par
Values in activities data set are changed to descriptive names with help of "feature" file\par
X,Y and Subject data sets are combined into a single dataset using cbind() function\par
Aggregate() function is used to compute mean for different subject and activities in X data set\par
Output of aggregate() function is stored in tidy_data file in the working directory using write.table function with row.names=FALSE\par
\par
\par
\par
\par
}
 