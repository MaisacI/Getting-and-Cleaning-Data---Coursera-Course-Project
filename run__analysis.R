library(dplyr)

##Getting the data##

#Downloading zip file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

# Unzipping file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

data <- file.path("./data" , "UCI HAR Dataset")

##Reading the data##

#Training data
trainSubjects <- read.table(file.path(data, "train", "subject_train.txt"))
trainValues <- read.table(file.path(data, "train", "X_train.txt"))
trainActivity <- read.table(file.path(data, "train", "y_train.txt"))

#Test data
testSubjects <- read.table(file.path(data, "test", "subject_test.txt"))
testValues <- read.table(file.path(data, "test", "X_test.txt"))
testActivity <- read.table(file.path(data, "test", "y_test.txt"))

#Features
features <- read.table(file.path(data, "features.txt"), as.is = TRUE)
  
#Activity labels
activity <- read.table(file.path(data, "activity_labels.txt"))
colnames(activity) <- c("activityId", "activityLabel")


##Merge training and test data##
mergedata <- rbind(cbind(trainSubjects, trainValues, trainActivity), cbind(testSubjects, testValues, testActivity))

##Assign column names##
colnames(mergedata) <- c("subject", features[, 2], "activity")


##Extraction of mean and standard deviation for each measurement

columnsToKeep <- grepl("subject|activity|mean|std", colnames(mergedata))
mergedata <- mergedata[, columnsToKeep]


##Use descriptive activity names in naming activities in the data set with named factor levels##
mergedata$activity <- factor(mergedata$activity, levels = activity[, 1], labels = activity[, 2])


##Proper labelling
mergedatacols <- colnames(mergedata)
mergedatacols <- gsub("[\\(\\)-]", "", mergedatacols) # remove special characters
mergedatacols <- gsub("^f", "frequencyDomain", mergedatacols) # expand abbreviations
mergedatacols <- gsub("Gyro", "Gyroscope", mergedatacols)
mergedatacols <- gsub("std", "StandardDeviation", mergedatacols)
mergedatacols <- gsub("^t", "timeDomain", mergedatacols)
mergedatacols <- gsub("Freq", "Frequency", mergedatacols)
mergedatacols <- gsub("mean", "Mean", mergedatacols)
mergedatacols <- gsub("Acc", "Accelerometer", mergedatacols)
mergedatacols <- gsub("BodyBody", "Body", mergedatacols)
colnames(mergedata) <- mergedatacols


##Create a second, independent tidy data set with the average of each variable for each activity and each subject##
library(plyr);
mergedata2<-aggregate(. ~subject + activity, mergedata, mean)
mergedata2<-mergedata2[order(mergedata2$subject,mergedata2$activity),]
write.table(mergedata2, file = "tidydata.txt",row.name=FALSE)
