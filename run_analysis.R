library(dplyr)

# download and unzip file 
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
if (!file.exists(zipFile)) {
        download.file(zipUrl, zipFile, mode = "wb")
}
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(zipFile)
}

# read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features and activity lables
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Merge the training and  test sets in one data set

humanActivity <- rbind(
        cbind(trainingSubjects, trainingValues, trainingActivity),
        cbind(testSubjects, testValues, testActivity)
)

# remove unnecessary data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Extract only the measurements on the mean and standard deviation for each measurement

# Keep select columns of data set 
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]


# Use descriptive activity names to name the activities in the data set and 
# replace activity values with named factor levels

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols


# Create a second, independent tidy set with the average of each
#          variable for each activity and each subject

# group by subject and activity and summarize using mean
humanActivityMeans <- humanActivity %>% 
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

# output to  "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)