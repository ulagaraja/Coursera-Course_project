run_analysis<-function()
{
  #Reading X,Y and subject files from Test and Train folders
  #combining Test and Train data set into single data set(Seperately for x,y and Subject) using rbind()
  x_test<-read.table("./UCI HAR Dataset/test/x_test.txt",quote = "")
  x_train<-read.table("./UCI HAR Dataset/train/x_train.txt",quote = "")
  x<-rbind(x_test,x_train)
  
  y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",quote = "")
  y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",quote = "")
  y<-rbind(y_test,y_train)
  
  subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",quote = "")
  subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",quote = "")
  subject<-rbind(subject_test,subject_train)
  #Reading features.txt file 
  features<-read.table("./UCI HAR Dataset/features.txt")
  
  #Filtering the columns having mean or STD in X
  indices<-grep("mean|std",features$V2)
  x<-x[,indices]  
  #Changing Variable names into descriptive names in X
  colnames(x)<-features[indices,2]
  
  #Changing values in Y into Descriptive Activity names
  activity<-c("walking","Walking_upstairs","walking_downstairs","sitting","standing","laying")
  for(i in 1:6)
  {
    index<-grep(i,y$V1);
    y[index,1]<-activity[i]
  }
  
  #Changing the header names from V1,V to descriptive names in Y and subject
  colnames(y)<-"Activity"
  colnames(subject)<-"Subject"
  
  #combining X,Y and subject into one single data set
  x<-cbind(subject,y,x)
  
  #Using aggregate function to calculate mean for different subject and activities
  tidy_data<-aggregate(x,by=list(Subject,Activity),FUN = mean)
  tidy_data$Subject<-NULL
  
  #Changing the header names into descriptive names in tidy data received from aggregate function
  tidy_data$Activity<-NULL
  colnames(tidy_data)[1]<-"Subject"
  colnames(tidy_data)[2]<-"Activity"
  
  #Writing the tidy data set into current working directory
  write.table(tidy_data,file="./tidy_data.txt",row.names = FALSE)
  
}