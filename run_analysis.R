#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)

# READ DATA X
DataTrain.x <- read.table("./data/train/X_train.txt")
DataTest.x <- read.table("./data/test/X_test.txt")

# READ DATA Y
DataTrain.y <- unlist(read.table("./data/train/y_train.txt"))
DataTest.y <- unlist(read.table("./data/test/y_test.txt"))

# READ DATA SUBJECT
DataTrain.subject <- unlist(read.table("./data/train/subject_train.txt"))
DataTest.subject <- unlist(read.table("./data/test/subject_test.txt"))

# READ DATA ATIVITY LABELS
activity_labels <- read.table("./data/activity_labels.txt")
colnames(activity_labels) <- c("ActivityCode","ActivityName")

# READ DATA  FEATURES
features <- read.table("./data/features.txt")

# MERGE TRAIN AND TEST DATASET
DataMerged <- rbind(DataTrain.x,DataTest.x)

# EXTRACT ONLY MEAN AND STD
Datatmp <- features
Datatmp$IsMean <- grepl("mean()",Datatmp[,2])
Datatmp$IsStd <- grepl("std()",Datatmp[,2])

DataMeanStd <- DataMerged[,Datatmp$IsMean | Datatmp$IsStd]

# DESCRIPTIVE ACTIVITY NAME
DataMeanStd$ActivityCode <- c(DataTrain.y,DataTest.y)
colnames(activity_labels) <- c("ActivityCode","ActivityName")
DataMeanStd$ActivityName <- activity_labels[DataMeanStd$ActivityCode,2]

#APPROPIATELY VARIABLE NAME 
colnames(DataMeanStd)<-Datatmp[Datatmp$IsMean | Datatmp$IsStd,2]

#TIDY DATASET WITH AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND SUBJECT
DataMerged$Subject <- c(DataTrain.subject,DataTest.subject)
DataMerged$Activity <- c(DataTrain.y,DataTest.y)
DataTidy <- do.call(rbind,by(DataMerged, DataMerged$Subject,FUN=function(subtab) {
  do.call(rbind,by(subtab, subtab$Activity, FUN=function(subsubtab) {
    nc <- ncol(subsubtab)
    c(
      apply(subsubtab[,1:(nc-2)], FUN=function(x) { mean(as.numeric(x)) }, MARGIN=2),
      subsubtab[1,(nc-1):nc]
    )
  }))
}))
write.csv(DataTidy, "UCI_tidy.csv", row.names=FALSE)
