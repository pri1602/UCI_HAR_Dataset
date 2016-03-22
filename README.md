## UCI_HAR_Dataset
## Coursera_Getting and Cleaning Data_CourseProject
The purpose of this Course Project is to create a tidy dataset from the Human Activity Recognition Using Smartphones Dataset.The dataset is available at [UCI_HAR_Dataset](https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
###### From the UCI-HAR folder, read the 'X_test.txt' and 'X_train.txt' files. 
```
setwd("~/R/Coursera/Getting and Cleaning Data/Course Project")
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
td <- tempdir()
tf <- tempfile(tmpdir=td, fileext=".zip") 
download.file(fileurl, tf)
fname <- unzip(tf, exdir=td, overwrite=TRUE, list=TRUE)
head(fname)
fpath_test <- file.path(td, fname$Name[17])
test_data <- read.table(fpath_test)
str(test_data)
fpath_train <- file.path(td,fname$Name[31])
train_data <- read.table(fpath_train)
str(train_data)
```
######STEP 1: Merged 'test_data' and 'train data' to create one dataset.
```
library(dplyr)
merged <- full_join(test_data, train_data)
str(merged)
```
######STEP 2: Extract measurements on the mean and standard deviation for each measurement. 
It will be easier to do this if I have the variable names for each column. I import the variable names from the 'features.txt' file.I use function t() to transpose the column values to row-names. Transpose gives a matrix of names, so I first convert it into #a vector of names using function as.vector.I use the names() function to replace the header of the 'merged' dataset with the new variable names. I use grep to find those variables with mean or standard deviation ('mean|sd') in their names.  I then subset the 'merged' dataset by the indices thrown out by the grep function to derive a smaller dataset with only the mean and standard deviation measurements.
```
fpath_varnames <- file.path(td,fname$Name[2])
varnames <- read.table(fpath_varnames)
varnames <- select(varnames, V2)
transp_var <- t(varnames)
variables <- as.vector(transp_var)
names(merged) <- variables
head(merged)
find_vars <- grep('mean|std',names(merged))
data <- merged[,find_vars]
head(data)
```
######STEP 3: Use descriptive activity names to describe the activities in the dataset.
First I need the activity codes for each observation. So, I imported the 'y_test.txt' file and 'y_train.txt' file. Appended the values in y_train to the values in y_test. The resulting 'activity' vector gives the activity codes of each row in 'data.' Then I merged the 'activity' vector with the 'data' dataset. So now, every observation has an activity code. Then I imported the 'activity_labels.txt' file to see which code corresponds to what label. Finally,I used the 'gsub' function to replace the activity codes with activity labels. 
 ```
fpath_ytest <- file.path(td, fname$Name[18])
ytest <- read.table(fpath_ytest)
fpath_ytrain <- file.path(td, fname$Name[32])
ytrain <- read.table(fpath_ytrain)
activity <- append(ytest$V1,ytrain$V1)
activity_data <- cbind(activity, data)
str(activity_data)
fpath_activity <- file.path(td, fname$Name[1])
activity_labels <- read.table(fpath_activity)
activity_labels
activity_data$activity <- gsub(1, "WALKING", activity_data$activity)
activity_data$activity <- gsub(2, "WALKING_UPSTAIRS", activity_data$activity)
activity_data$activity <- gsub(3, "WALKING_DOWNSTAIRS", activity_data$activity)
activity_data$activity <- gsub(4, "SITTING", activity_data$activity)
activity_data$activity <- gsub(5, "STANDING", activity_data$activity)
activity_data$activity <- gsub(6, "LAYING", activity_data$activity)
View(activity_data)
```
######STEP 4: Appropriately labels the data set with descriptive variable names.
The variable names were added in Step 2. I cleaned up the names by replacing the - with _.
```
names(activity_data) <- gsub("-", "_", names(activity_data))
```
######STEP5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
This step requires the SubjectIDs for each observation. I import the subjectIDs of the 'test' and 'train' datasets.Then appended the values of subjectIDs for the train data to subjectIDs for the test data. This gives a vector of values with the subjectID for every observation in the dataset. Then merge the subjectID vector with the 'activity_data.' Used the 'aggregate' function to group the dataset by subject ID and activity, and took the mean for each variable for each group.Since the 'aggregate' function converts the 'activity' variable to a factor, I replace the factor codes with activity labels for the final tidy dataset.
```
fpath_subjectID_test <- file.path(td, fname$Name[16])
subjectID_test <- read.table(fpath_subjectID_test)
head(subjectID_test)
fpath_subjectID_train <- file.path(td,fname$Name[30])
subjectID_train <- read.table(fpath_subjectID_train)
head(subjectID_train)
subjectID <- append(subjectID_test$V1, subjectID_train$V1)
g <- cbind(subjectID, activity_data)
x <- aggregate(g[,-(1:2)], list(subjectID,activity),mean)
x$Group.2 <- gsub(1, "WALKING", x$Group.2)
x$Group.2 <- gsub(2, "WALKING_UPSTAIRS", x$Group.2)
x$Group.2 <- gsub(3, "WALKING_DOWNSTAIRS", x$Group.2)
x$Group.2 <- gsub(4, "SITTING", x$Group.2)
x$Group.2 <- gsub(5, "STANDING", x$Group.2)
x$Group.2 <- gsub(6, "LAYING",x$Group.2)
View(x)
```







