# Set Working directory
setwd("C:/Users/vince/GitRepo/GitHub/getting-and-cleaning-data")
# possible packages needed
install.packages("R.utils")
library(R.utils)
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("quantmod")
library(quantmod)
install.packages("stringr")
library(stringr)
install.packages("qdap")
library(qdap)
install.packages("plyr")
library(plyr)

# 1. Merges the training and the test sets to create one data set.

# Download File
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "getdata_projectfiles_UCI_HAR_Dataset.zip")
download.file(url, f)

# unzip file install R.utils package to get to unzip command and load the library before using
unzip(f, overwrite = TRUE)
fPath <- file.path(getwd(), "UCI HAR Dataset")
files <- list.files(fPath, recursive = TRUE)
# ignore "inertial signals"
filesFinal <- files[ !grepl("Inertial Signals",files) ]
filesFinal # print list of files to screen

# load data
d_Test_Subject   <- read.table(file.path(fPath, "test" , "subject_test.txt"), header = FALSE)
d_Test_Activity  <- read.table(file.path(fPath, "test" , "Y_test.txt" ), header = FALSE)
d_Test_Features  <- read.table(file.path(fPath, "test" , "X_test.txt" ), header = FALSE)

d_Train_Subject  <- read.table(file.path(fPath, "train", "subject_train.txt"), header = FALSE)
d_Train_Activity <- read.table(file.path(fPath, "train", "Y_train.txt"), header = FALSE)
d_Train_Features <- read.table(file.path(fPath, "train", "X_train.txt"), header = FALSE)

# merge datasets
dfSubject  <- rbind(d_Train_Subject, d_Test_Subject)
dfActivity <- rbind(d_Train_Activity, d_Test_Activity)
dfFeatures <- rbind(d_Train_Features, d_Test_Features)

#set names to vars
names(dfSubject)  <-c("subject")
names(dfActivity) <- c("activity")
FeaturesNames     <- read.table(file.path(fPath, "features.txt"),head=FALSE)

#remove bad characters
FeaturesNames$V2 <- gsub("(", "", fixed = TRUE, FeaturesNames$V2)
FeaturesNames$V2 <- gsub(")", "", fixed = TRUE, FeaturesNames$V2)
FeaturesNames$V2 <- gsub("-", "", fixed = TRUE, FeaturesNames$V2)
FeaturesNames$V2 <- gsub(",", "", fixed = TRUE, FeaturesNames$V2)
FeaturesNames$V2 <- gsub(" ", "", fixed = TRUE, FeaturesNames$V2)

# make columns lower case
names(dfFeatures) <- tolower(FeaturesNames$V2)

# merge datasets
dfFinal   <- cbind(dfSubject, dfActivity, dfFeatures)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# get only the columns that match mean or std, make sure to set to lower case
SubFeats <- tolower(FeaturesNames$V2[grep("mean|std", FeaturesNames$V2)])

# add the subject and activity columns also
setColNames <- c(as.character(SubFeats), "subject", "activity" )

# get the subset of data based on selected columns
almostTidyData <- subset(dfFinal, select=setColNames)

# 3. Uses descriptive activity names to name the activities in the data set
ativities <- read.table(file.path(fPath, "activity_labels.txt"),header = FALSE)

# replace the activities integer values with their corresponding string values
almostTidyData$activity <- factor(almostTidyData$activity);
almostTidyData$activity <- factor(almostTidyData$activity,labels=as.character(ativities$V2))

## 4. Appropriately labels the data set with descriptive variable names.
names(almostTidyData)<-gsub("^t", "time", names(almostTidyData))
names(almostTidyData)<-gsub("^f", "frequency", names(almostTidyData))
names(almostTidyData)<-gsub("acc", "accelerometer", names(almostTidyData))
names(almostTidyData)<-gsub("gyro", "gyroscope", names(almostTidyData))
names(almostTidyData)<-gsub("mag", "magnitude", names(almostTidyData))
names(almostTidyData)<-gsub("bodybody", "body", names(almostTidyData))

str(almostTidyData)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
groupColumns = c("subject","activity")
dataColumns = colnames(almostTidyData[,1:79])
dataColumns
tidyDf = ddply(almostTidyData, groupColumns, function(x) colMeans(x[dataColumns]))
head(tidyDf)

write.table(tidyDf, file = "tidydata.txt",row.name=FALSE)


