library(dplyr)

# Step 1
# Merge the training and test sets to create one data set
###############################################################################
setwd("C:/Users/yin.you/Documents/R Coursera/getdata_projectfiles_UCI HAR Dataset (1)/UCI HAR Dataset")
features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
x_test <- read.table("test/X_test.txt", col.names = features$functions)
y_test <- read.table("test/y_test.txt", col.names = "code")
subject_train <- read.table("train/subject_train.txt", col.names = "subject")
x_train <- read.table("train/X_train.txt", col.names = features$functions)
y_train <- read.table("train/y_train.txt", col.names = "code")
# create 'x' data set
x_data <- rbind(x_train, x_test)

# create 'y' data set
y_data <- rbind(y_train, y_test)

# create 'subject' data set
subject_data <- rbind(subject_train, subject_test)

Merged_Data <- cbind(subject_data, y_data , x_data)
# Step 2
# Extract only the measurements on the mean and standard deviation for each measurement
###############################################################################

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# Step 3
# Use descriptive activity names to name the activities in the data set
###############################################################################

TidyData$code <- activities[TidyData$code, 2]

# Step 4
# Appropriately label the data set with descriptive variable names
###############################################################################

# correct column name
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

# Step 5
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################

FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(FinalData, "FinalData.txt", row.name=FALSE)



