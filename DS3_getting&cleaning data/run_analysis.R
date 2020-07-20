library(dplyr)
file <- "JHU_DS3_Final.zip"
if (!file.exists(file)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, file, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(file) 
}

# read train data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
#read test data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
#variable description
features <- read.table("./UCI HAR Dataset/features.txt")
#activities
activity_label<- read.table("./UCI HAR Dataset/activity_labels.txt")
#1.Merge data
X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
Subject_total <- rbind(Subject_train, Subject_test)
#2. Extracts only the measurements on the mean and standard deviation for each measurement
select_var <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
X <- X[,select_var[,1]]
#3.Uses descriptive activity names to name the activities in the data set
colnames(Y) <- "activity"
Y$activitylabel <- factor(Y$activity, labels = as.character(activity_label[,2]))
activitylabel <- Y[,-1]
# 4. Appropriately labels the data set with descriptive variable names.
colnames(X) <- features[select_var[,1],2]
#5.From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
colnames(Subject_total) <- "subject"
total <- cbind(X, activitylabel, Subject_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
#export summary dataset:
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
