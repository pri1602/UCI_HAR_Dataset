# For this project, I used the UCI-HAR dataset. The tasks are to i) to merge two 
#datasets, to name the variables, to add a column of activity codes, to replace
#the activity codes by activity labels and finally to aggregate the variables 
#by subjectIDs and activity groups. 
setwd("~/R/Coursera/Getting and Cleaning Data/Course Project")
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#how to open zipped files
#Helpful link: http://www.r-bloggers.com/extracting-datasets-from-excel-files-in-a-zipped-folder/
#Create a temporary directory & a temporary file location.Download,unzip,create
#file path, read
td <- tempdir()
tf <- tempfile(tmpdir=td, fileext=".zip") 
# Download the fileurl to the temporary file location and unzip the file.
download.file(fileurl, tf)
#fname will contain list of all file names in the temporary directory 'td')
fname <- unzip(tf, exdir=td, overwrite=TRUE, list=TRUE)
head(fname)
#Create a file path to the temp directory.Within that directory, direct it to 
# the file you want to open. First, I import the 'X_Test' file, using fname$Name[15].
#Since its a .txt file, use read.table to import the file named 'test_data'
fpath_test <- file.path(td, fname$Name[17])
test_data <- read.table(fpath_test)
head(test_data)
#Now import the 'X_train' data.
fpath_train <- file.path(td,fname$Name[31])
train_data <- read.table(fpath_train)
head(train_data)

#STEP 1 Merge train_data and test_data
library(dplyr)
merged <- full_join(test_data, train_data)
head(merged)

# STEP 2 Select mean and standard dev variables.Now import the 'features' variable
#which essentially #contains the variable names.
fpath_varnames <- file.path(td,fname$Name[2])
varnames <- read.table(fpath_varnames)
#We select the column with only the names, i.e. 'V2' 
varnames <- select(varnames, V2)
# We want to transpose the row values in 'varnames' to column-names.t() will help us.
transp_var <- t(varnames)
#This gives us a matrix of the names. But we need a vector of names so that
#we can bind it to the merged dataset. 
variables <- as.vector(transp_var)
#Now, we must add this row of 'variables' vector as the headerof the 'merged' dataset.
# We have to replace the variable names v1, V2 etc. 
names(merged) <- variables
head(merged)
#Now we have to select all the variables which have 'mean' and 'sd'. 
find_vars <- grep('mean|std',names(merged))
#We subset 'merged' by indexing. We need all the columns in 'merged' and only
#those variables that correspond to 'find_vars'.
data <- merged[,find_vars]
head(data)

#STEP3 We have to add a variable of activity codes. This is in the 
#ytest and train tables. Then we have to replace the activity codes with activity
#labels.
#Import ytest and ytrain
fpath_ytest <- file.path(td, fname$Name[18])
ytest <- read.table(fpath_ytest)
fpath_ytrain <- file.path(td, fname$Name[32])
ytrain <- read.table(fpath_ytrain)
#If we can append ytest to ytrain, it will give us the activity codes of every
#observation in the dataset.(the functions merge and full_join duplicates values.
#we need to append ytest to ytrain. Make sure ytest comes before ytrain because 
#the values in the merged dataset are in that order. 
#'activity'gives us a vector of 10299 observations that corresponds to every observation in 'merged.'
activity <- append(ytest$V1,ytrain$V1)
#Merge the 'activity' vector with the 'data' dataset. 
activity_data <- cbind(activity, data)
str(activity_data)
#We import the table with activity labels
fpath_activity <- file.path(td, fname$Name[1])
activity_labels <- read.table(fpath_activity)
activity_labels
#We replace the activity codes with activity labels.
activity_data$activity <- gsub(1, "WALKING", activity_data$activity)
activity_data$activity <- gsub(2, "WALKING_UPSTAIRS", activity_data$activity)
activity_data$activity <- gsub(3, "WALKING_DOWNSTAIRS", activity_data$activity)
activity_data$activity <- gsub(4, "SITTING", activity_data$activity)
activity_data$activity <- gsub(5, "STANDING", activity_data$activity)
activity_data$activity <- gsub(6, "LAYING", activity_data$activity)
View(activity_data)
#STEP4 Replace variable names with descriptive variable names. I have already 
#replaced it with varriable names in Step 2.So I just clean it up a little by replacing
#- with _ and removing (). 
names(activity_data) <- gsub("-", "_", names(activity_data))
names(activity_data) <- gsub("[()]", "", names(activity_data))
names(activity_data)
#STEP5 To aggregate each variable by subjectID and activity.
#I had to first add the subjectID variable to the dataset. 
#Import subject IDs for test. 
fpath_subjectID_test <- file.path(td, fname$Name[16])
subjectID_test <- read.table(fpath_subjectID_test)
head(subjectID_test)
#Now import subjectIDs for train.
fpath_subjectID_train <- file.path(td,fname$Name[30])
subjectID_train <- read.table(fpath_subjectID_train)
head(subjectID_train)
#Append subject IDs of test with subjectIDs of train. 
subjectID <- append(subjectID_test$V1, subjectID_train$V1)
#Merge the 'subjectID' vector with the Step 4 'activity_data' dataset. 
g <- cbind(subjectID, activity_data)
#Aggregate by subjectID and activity
x <- aggregate(g[,-(1:2)], list(subjectID,activity),mean)
#Replace activity codes by labels. 
x$Group.2 <- gsub(1, "WALKING", x$Group.2)
x$Group.2 <- gsub(2, "WALKING_UPSTAIRS", x$Group.2)
x$Group.2 <- gsub(3, "WALKING_DOWNSTAIRS", x$Group.2)
x$Group.2 <- gsub(4, "SITTING", x$Group.2)
x$Group.2 <- gsub(5, "STANDING", x$Group.2)
x$Group.2 <- gsub(6, "LAYING",x$Group.2)
#Replacing the names of the variables with new names
names(x) <- gsub("mean", "groupMean", names(x))
names(x) <- gsub("std", "groupStd", names(x))
names(x)[names(x)=="Group.1"] <- "subjectID"
names(x)[names(x)=="Group.2"] <- "activity"
View(x)
write.table(x, file="har_data.txt", row.names=F)

