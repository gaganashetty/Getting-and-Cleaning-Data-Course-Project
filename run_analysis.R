library(dplyr)

filename <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
#Dataset downloaded and extracted under the folder called UCI HAR Dataset

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Assigning all data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
#The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.

activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
#List of activities performed when the corresponding measurements were taken and its codes (labels)

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
#contains test data of 9/30 volunteer test subjects being observed

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
#contains recorded features test data

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
#contains test data of activities’code labels

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
#contains train data of 21/30 volunteer subjects being observed

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
#contains recorded features train data

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
#contains train data of activities’code labels



#Step 1: Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)
Merged_Data

#X is created by merging x_train and x_test using rbind() function
#Y is created by merging y_train and y_test using rbind() function
#Subject is created by merging subject_train and subject_test using rbind() function
#Merged_Data is created by merging Subject, Y and X using cbind() function

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
TidyData
#TidyData is created by subsetting Merged_Data, selecting only columns: subject, code and the measurements on the mean and standard deviation (std) for each measurement

#Step 3: Uses descriptive activity names to name the activities in the data set.
TidyData$code <- activities[TidyData$code, 2]
#Entire numbers in code column of the TidyData replaced with corresponding activity taken from second column of the activities variable

#Step 4: Appropriately labels the data set with descriptive variable names.

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

#code column in TidyData renamed into activities
#All Acc in column’s name replaced by Accelerometer
#All Gyro in column’s name replaced by Gyroscope
#All BodyBody in column’s name replaced by Body
#All Mag in column’s name replaced by Magnitude
#All start with character f in column’s name replaced by Frequency
#All start with character t in column’s name replaced by Time

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

str(FinalData)

FinalData

#FinalData is created by sumarizing TidyData taking the means of each variable for each activity and each subject, after groupped by subject and activity.
