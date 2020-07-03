# Getting and Cleaning Data Course Project

# You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set. √
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. √
# 3. Uses descriptive activity names to name the activities in the data set √
# 4. Appropriately labels the data set with descriptive variable names. √
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#     of each variable for each activity and each subject. √

getwd()
setwd("/Users/noemiglarner/Documents/MD-PhD CRIB/Education/Statistics/
      Data Science Specialization (coursera.org)/Module 3 - Getting and Cleaning Data/data/UCI HAR Dataset/")

# Read in data
activity <- read.table("activity_labels.txt")
colnames(activity) <- c("activityID", "activityLabel")

features <- read.table("features.txt", as.is = TRUE)

# Test subjects
subject_test <- read.table("test/subject_test.txt")
testx <- read.table("test/X_test.txt")
testy <- read.table("test/y_test.txt")

str(subject_test)
# 'data.frame':	2946 obs. of  1 variable:
# $ X2: int  2 2 2 2 2 2 2 2 2 2 ...

# Train subjects
subject_train <- read.table("train/subject_train.txt")
trainx <- read.table("train/X_train.txt")
trainy <- read.table("train/y_train.txt")

str(subject_train)
# 'data.frame':	7351 obs. of  1 variable:
# $ X1: int  1 1 1 1 1 1 1 1 1 1 ...

# 1 Merges the training and the test sets to create one data set
test_data <- cbind(subject_test, testy, testx)
test_data$group <- c("Test")
train_data <- cbind(subject_train, trainy, trainx)
train_data$group <- c("Train")
df <- rbind(test_data, train_data)
# Add column names
colnames(df) <- c("subject", "activity", features[,2], "group")

# 2 Extracts only the measurements on the mean and standard deviation for each measurement
keep_columns <- grepl("subject|activity|mean|std|group", colnames(df))
df2 <- df[, keep_columns]

# 3 Uses descriptive activity names to name the activities in the data set
df2$activity <- factor(df2$activity, levels = activity[, 1], labels = activity[, 2])

# 4 Appropriately labels the data set with descriptive variable names (from features_info)
column_names <- colnames(df2)
column_names <- gsub("[\\(\\)-]", "", column_names)
column_names <- gsub("subject", "Subject", column_names)
column_names <- gsub("activity", "Activity", column_names)
column_names <- gsub("group", "Group", column_names)
column_names <- gsub("^t", "Time", column_names)
column_names <- gsub("^f", "Frequency", column_names)
column_names <- gsub("Gyro", "Gyroscope", column_names)
column_names <- gsub("Acc", "Accelerometer", column_names)
column_names <- gsub("Mag", "Magnitude", column_names)
column_names <- gsub("mean", "Mean", column_names)
column_names <- gsub("std", "StandardDeviation", column_names)
column_names <- gsub("BodyBody", "Body", column_names)

colnames(df2) <- column_names
df2

# 5 From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
library(dplyr)

tidy_data <- df2 %>%
  group_by(Subject, Activity, Group) %>% 
  summarise_each(funs(mean))

# Output file
write.csv(tidy_data, file = "tidy_data.csv")
write.table(tidy_data, file = "tidy_data.txt")