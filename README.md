# UCI_HAR_Dataset
Coursera_Getting and Cleaning Data_CourseProject
#The purpose of this Course Project is to create a tidy dataset from the Human Activity Recognition Using Smartphones Dataset. For #this analysis, I used the following files out of all the files from the original HAR dataset. 

#- 'README.txt'
#- 'features_info.txt': Shows information about the variables used on the feature vector. 
#- 'features.txt': List of all features. This file gives the variable labels. 
#- 'train/X_train.txt': This is the training dataset. 
#- 'train/y_train.txt': Activity codes for each variable in the training dataset.
#- 'train/subject_train.txt': SubjectID of the subject who performed the activity in the training dataset.  
#- 'test/X_test.txt': This is the test dataset. 
#- 'test/y_test.txt': Activity codes for each variable in the test dataset.
#- 'test/subject_test.txt': SubjectID of the subject who performed the activity in the test dataset. 
#- 'activity_labels.txt': Links the class labels with their activity name.

#Unzip the UCI-HAR folder. 
setwd("~/R/Coursera/Getting and Cleaning Data/Course Project")
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
td <- tempdir()
tf <- tempfile(tmpdir=td, fileext=".zip") 
download.file(fileurl, tf)
fname <- unzip(tf, exdir=td, overwrite=TRUE, list=TRUE)
head(fname)

#Import the 'X_test'file, save as 'test_data.'Also import the 'X_train' files and save as 'train_data.'
fpath_test <- file.path(td, fname$Name[17])
test_data <- read.table(fpath_test)
str(test_data)
fpath_train <- file.path(td,fname$Name[31])
train_data <- read.table(fpath_train)
str(train_data)

#STEP 1: Merged 'test_data' and 'train data' to create the 'merged' dataset using the function full_join from dplyr tools.
library(dplyr)
merged <- full_join(test_data, train_data)
str(merged)

#STEP 2: Extract measurements on the mean and standard deviation for each measurement. 
#1. It will be easier to do this if I have the variable names for each column. I import the variable names from the 'features.txt' file. However, the names are in one single column. 
#2. I use function t() to transpose the column values to row-names. Transpose gives a matrix of names, so I first convert it into #a vector of names using function as.vector.
#3. I use the names() function to replace the header of the 'merged' dataset with the new variable names. 
#4. I use grep to find those variables with mean or standard deviation ('mean|sd') in their names.  I then subset the 'merged' dataset by the indices thrown out by the grep function to derive a smaller dataset with only the mean and standard deviation measurements. 
     #Importing the features file and selecting only that variable which gives the variable names in one column.  
fpath_varnames <- file.path(td,fname$Name[2])
varnames <- read.table(fpath_varnames)
varnames <- select(varnames, V2)
    #Transposing and converting resulting matrix into a vector. 
transp_var <- t(varnames)
variables <- as.vector(transp_var)
   #Replacing the names of 'merged' with the values in the vector 'variables.'
names(merged) <- variables
head(merged)
   # Finding variable names with 'mean' and 'std'. Then subsetting 'merged' by resulting indices.
find_vars <- grep('mean|std',names(merged))
data <- merged[,find_vars]
head(data)

#STEP 3: Use descriptive activity names to describe the activities in the dataset.
#1. First I need the activity codes for each observation. So, I imported the 'y_test.txt' file and 'y_train.txt' file. 
#2. Appended the values in y_train to the values in y_test. The resulting 'activity' vector gives the activity codes of each row in #'data.'
#3. Merged the 'activity' vector with the 'data' dataset. So now, every observation has an activity code. 
#4. Imported the 'activity_labels.txt' file to see which code corresponds to what label. 
#5. Used the 'gsub' function to replace the activity codes with activity labels. 
    #Importing activity codes in 'ytest.txt'
fpath_ytest <- file.path(td, fname$Name[18])
ytest <- read.table(fpath_ytest)
    #Importing activity codes in 'ytrain.txt'
fpath_ytrain <- file.path(td, fname$Name[32])
ytrain <- read.table(fpath_ytrain)
    #Appending 'ytrain' values to 'ytest' values. 
activity <- append(ytest$V1,ytrain$V1)
    #Merge the 'activity' vector to the 'data' dataset. 
activity_data <- cbind(activity, data)
str(activity_data)
   #Import activity labels from 'activity_labels.txt'
fpath_activity <- file.path(td, fname$Name[1])
activity_labels <- read.table(fpath_activity)
activity_labels
   #Replace codes in the 'activity' variable with activity labels.
activity_data$activity <- gsub(1, "WALKING", activity_data$activity)
activity_data$activity <- gsub(2, "WALKING_UPSTAIRS", activity_data$activity)
activity_data$activity <- gsub(3, "WALKING_DOWNSTAIRS", activity_data$activity)
activity_data$activity <- gsub(4, "SITTING", activity_data$activity)
activity_data$activity <- gsub(5, "STANDING", activity_data$activity)
activity_data$activity <- gsub(6, "LAYING", activity_data$activity)
View(activity_data)

#STEP 4: Appropriately labels the data set with descriptive variable names.
The variable names were added in Step 2. I cleaned up the names by replacing the - with _. 
names(activity_data) <- gsub("-", "_", names(activity_data))

#STEP5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#1. This step requires the SubjectIDs for each observation. I import the subjectIDs of the 'test' and 'train' datasets. 
#2. Appended the values of subjectIDs for the train data to subjectIDs for the test data. This gives a vector of values with the #subjectID for every observation in the dataset. 
#3. Merge the subjectID vector with the 'activity_data.'
#4. Used the 'aggregate' function to group the dataset by subject ID and activity, and took the mean for each variable for each group.
#5. Since the 'aggregate' function converts the 'activity' variable to a factor, I replace the factor codes with activity labels for 
#the final tidy dataset. 
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
  #Use 'aggregate' group by 'subjectID' and 'activity' and take the mean of each variable(except 'subjectID' and 'activity'). 
x <- aggregate(g[,-(1:2)], list(subjectID,activity),mean)
   #Replace activity codes by activity labels. 
x$Group.2 <- gsub(1, "WALKING", x$Group.2)
x$Group.2 <- gsub(2, "WALKING_UPSTAIRS", x$Group.2)
x$Group.2 <- gsub(3, "WALKING_DOWNSTAIRS", x$Group.2)
x$Group.2 <- gsub(4, "SITTING", x$Group.2)
x$Group.2 <- gsub(5, "STANDING", x$Group.2)
x$Group.2 <- gsub(6, "LAYING",x$Group.2)
   #x is the required tidy dataset. 
View(x)








