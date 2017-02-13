library(plyr)

# 0. Downloading dataset
if(!file.exists("./project")){dir.create("./project")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./project/Dataset.zip")

# Unzip dataSet to /project directory
unzip(zipfile="./project/Dataset.zip",exdir="./project")


# 1. Merging the training and the test sets to create one data set:

# 1.1 Reading files

# 1.1.1 Reading trainings tables:
x_train <- read.table("./project/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./project/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./project/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading testing tables:
x_test <- read.table("./project/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./project/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./project/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('./project/UCI HAR Dataset/features.txt')

# 1.1.4 Reading activity labels:
activityLabels = read.table('./project/UCI HAR Dataset/activity_labels.txt')

# 1.2 Assigning column names:
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# 1.3 Merging all data in one set:
mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
set_all <- rbind(mrg_train, mrg_test)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 2.1 Reading column names:
colNames <- colnames(set_all)

# 2.2 Create vector for defining ID, mean and standard deviation:
mean_stdev <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
)

# 2.3 Making nessesary subset from set_all:
sub_mean_stdev <- set_all[ , mean_stdev == TRUE]

# 3. Using descriptive activity names to name the activities in the data set:
activity_names <- merge(sub_mean_stdev, activityLabels,
                              by='activityId',
                              all.x=TRUE)

# 4. Appropriately labeling the data set with descriptive variable names.
# This step was made in previos steps =) See 1.3, 2.2, 2.3.

# 5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject:

# 5.1 Making second tidy data set 
TidySet2 <- aggregate(. ~subjectId + activityId, activity_names, mean)
TidySet2 <- TidySet2[order(TidySet2$subjectId, TidySet2$activityId),]

# 5.2 Writing second tidy data set in txt file
write.table(TidySet2, "TidySet2.txt", row.name=FALSE)