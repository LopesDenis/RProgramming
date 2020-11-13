# Script - Getting and Cleaning Data - Course Project 
# Author: Denis Lopes


# ## Steps
# 1. Merge Training and Test sets
# 2. Extract measuements
# 3. Use Descriptive activity
# 4. Adjust Labels
# 5. Create from step 4, dataSet with Avarage

# Download file and extract
library(data.table)
library(reshape2)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
              , file.path("./data/dataSet.zip"))
unzip(zipfile = "./data/dataSet.zip", exdir = "./data")


# load dataSet
activityLabels <- fread(file.path("./data/UCI HAR Dataset/activity_labels.txt")
                            , col.names = c("classLabels", "activityName"))

features <- fread(file.path("./data/UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureName"))

# find wanted features and cleaning
featuresWanted <-  grep("(mean|std)\\(", features[, featureName])
measurements <- features[featuresWanted, featureName ]
measurements <- gsub('[()]', '', measurements)


#load train dataSets 
train <- fread(file.path("./data/UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = F]
setnames(train, colnames(train), measurements)

trainActivities <- fread(file.path("./data/UCI HAR Dataset/train/y_train.txt")
                         , col.names = c("activity"))

trainSubjects <- fread(file.path("./data/UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("subjectNum"))

train <- cbind(trainSubjects, trainActivities, train)


#load test dataSets 
test <- fread(file.path("./data/UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = F]
setnames(test, colnames(test), measurements)

testActivities <- fread(file.path("./data/UCI HAR Dataset/test/y_test.txt")
                         , col.names = c("activity"))

testSubjects <- fread(file.path("./data/UCI HAR Dataset/test/subject_test.txt")
                       , col.names = c("subjectNum"))

test <- cbind(testSubjects, testActivities, test)


#merge datasets
combined <- rbind(train, test)


# convert activity and SubjectNum in Factor
combined[["activity"]] <- factor(combined[, activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])


combined[["subjectNum"]] <- factor(combined[["subjectNum"]] )


# organize data , melt and them aggregate with mean
combined <- reshape2::melt(data = combined, id = c("subjectNum", "activity"))
combined <- reshape2::dcast(data = combined, subjectNum + activity ~ variable, fun.aggregate = mean)

# order
combined <- combined[order(combined$activity),]


# save data
data.table::fwrite(x = combined, file = "tidyData.txt", quote = FALSE)

