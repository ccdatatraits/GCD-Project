---
title: "Getting &amp; Cleaning Data - Course Project"
author: "Sohail Munir Khan"
date: "21 June 2015"
output: html_document
---

### Code Structure
run_analysis.R is broken into various top level functions
```{r}
######################## Start Analysis ########################
datasetDir <- fetchAndUnzipData()
dataEnvironment <- mergeDatasets(datasetDir)
dataEnvironment <- extractDataset(dataEnvironment)
dataEnvironment <- nameDescriptiveActivities(dataEnvironment)
dataEnvironment <- labelDataset(dataEnvironment)
tidyDataset <- createTidyDataset(dataEnvironment)
####################### Finish Analysis ########################
```

* datasetDir <- fetchAndUnzipData()
    + Creates data directory and creates it if needed
    + Checks that the original ZIP in placed in the directory or not, 
    otherwise retrieves it from URL provided
    + Outputs dateDownloaded for the ZIP, if file was retrieved
    + Checks whether data has been unzipped or not, otherwise unzip the
    downloaded data
    + Returns directory location to where files were retrieved and unzipped

```{r}
# Fetches the required source data far the project if needed
fetchAndUnzipData <- function() {
    # Create data/ directory if it doesn't exist
    dataDir <- "./data/"
    if(!file.exists(dataDir)) {dir.create(dataDir)}
    # Relative directory where unzipped data should be available
    datasetDir <- paste(dataDir, "UCI HAR Dataset/", sep = "")
    # Relative path to zip file, if downloaded or available
    dataset <- paste(dataDir, "Dataset.zip", sep = "")
    
    # Fetch zip file if needed
    if (!file.exists(dataset)) {
        sprintf("Fetching Data: %s\n", dataset)
        fileURL <- paste("https://d396qusza40orc.cloudfront.net/getdata%2F", 
                         "projectfiles%2FUCI%20HAR%20Dataset.zip", sep = "")
        download.file(fileURL, destfile = dataset, method = "curl")
        # Print date if data downloaded
        dateDownloaded <- date()
        dateDownloaded
    }
    # Else print data (Dataset.zip) exists
    else print("Data already exist")
    
    # Check whether data has been unzipped therefore Samsung data is available
    if (!file.exists(datasetDir)) {
        print("Unzipping Data")
        unzip(dataset, exdir = dataDir)
    }
    # Else print data has been unzipped
    else print("Data already unzipped")
    # Returned location to dataset
    datasetDir
}
```
  
* dataEnvironment <- mergeDatasets(datasetDir)
    + Uses the dataset directory (datasetDir) to fetch each file in both 
    test and train folder
    + Makes sure that relative path for each file for test has a corresponding 
    train file. Otherwise the program stops
    + Creates a MERGED object for each combination of test and train data
        + e.g. X_test.txt and X_train.txt is merged into an object: X_MERGED
    + Fetches raw activityLabels & features data from files in 
    'data/UCI HAR Dataset/' folder
    + All of these objects are placed into a new dataEnvironment which is 
    created only for the purpose of data that will be used in the 
    following steps
    + All subsequent calls pass this new environment to get and store 
    data objects

```{r}
# Step1: Merges the training and the test sets to create one data set.
mergeDatasets <- function(directory) {
    
    # Called by mergeData to combine test and train for a particular variable
    produceMergedData <- function(testFileLocation, trainFileLocation) {
        testDF <- read.table(testFileLocation, as.is = TRUE)
        trainDF <- read.table(trainFileLocation, as.is = TRUE)
        rbind(testDF, trainDF)
    }
    
    # Merge each test file with train file, while also checking that both file
    # exist. 
    mergeData <- function(testFile, trainFile, dataEnvironment) {
        # Relative location to testFile
        testFileLocation <- paste(directory, "test/", testFile, sep = "")
        # Relative location to trainFile
        trainFileLocation <- paste(directory, "train/", trainFile, sep = "")
        
        # Compare that filenames for both test and train only differ by 
        # test or train. E.g. Inertial\ Signals/total_acc_z_test.txt should
        # have a matching train file: Inertial\ Signals/total_acc_z_train.txt
        # We breakdown this relative path by removing the word 'test' or 'train'
        # Then confirm that remaining tokens(strsplit) are common for both
        testFullPathIgnoringName <- unlist(strsplit(testFileLocation, "test"))
        trainFullPathIgnoringName <- 
            unlist(strsplit(trainFileLocation, "train"))
        # If we find there is a problem between test or train, in terms of any
        # file not being common names, we break the program (stop function)
        if(!all(testFullPathIgnoringName == trainFullPathIgnoringName)) {
            stop("Filenames or filepaths for test & train data don't match")
        }
        # For each pair of test and train, we create a new string with the 
        # matching name. E.g. Inertial\ Signals/total_acc_z_MERGED for the 
        # example provided above in comments
        mergedName <- gsub("test", "MERGED", 
                            tail(unlist(strsplit(testFileLocation, "/")), 1))
        # Remove the trailing .txt from common MERGED name
        mergedName <- gsub(".txt", "", mergedName)
        # Merge the actual two files
        mergedData <- produceMergedData(testFileLocation, trainFileLocation)
        # Store this mergedData associated with mergedName in our data env
        assign(mergedName, mergedData, dataEnvironment)
    }
    # Location of test and train dir relative to the Samsung directory
    testDir <- paste(directory, "test/", sep = "")
    trainDir <- paste(directory, "train/", sep = "")
    # recursively list test and train folder files
    testFiles <- list.files(testDir, recursive = TRUE)
    trainFiles <- list.files(trainDir, recursive = TRUE)
    
    # activityLabel and features file have to be read as well.
    # Filenames set here
    activityLabelsFile <- paste(directory, "activity_labels.txt", sep = "")
    featuresFile <- paste(directory, "features.txt", sep = "")
    
    # Actual reading of the file and created DataFrames
    activityLabelsDF <- read.table(activityLabelsFile, as.is = TRUE)
    featuresDF <- read.table(featuresFile, as.is = TRUE)
    
    # Create a new environment to store all relevant information
    dataEnvironment <- new.env()
    # Match each recursively found test file with equivalent train file
    for (i in seq_along(testFiles)) {
        print(paste("Merging:", testFiles[i],"&",trainFiles[i]))
        mergeData(testFiles[i], trainFiles[i], dataEnvironment)
    }
    
    # Assign activityLabels and features RAW files into data env
    assign("activityLabelsRAW", activityLabelsDF, dataEnvironment)
    assign("featuresRAW", featuresDF, dataEnvironment)
    dataEnvironment
}
```
  
* dataEnvironment <- extractDataset(dataEnvironment)
    + Raw activityLables & features first reduntant columns are removed
    + features data is filtered to only get colnames that have either mean or
    std (standard deviation). E.g. 530 fBodyBodyGyroMag-std()
    + X data is filtered using the features found above - allRelevantXData
    + allRelevantData is created by column binding allRelevantXData, subject, y
    + features columns are renamed to remove "-", "(", ")" and camel casing mean
    & std columns
    + Leav the data, featuresUsed and activityLabels for later use and remove 
    everything else

```{r}
# Step2: Extracts only the measurements on the X and 
#        standard deviation for each measurement. 
extractDataset <- function(dataEnvironment) {
    # Store all dataEnvironment objects for now.
    # These will be dropped by the end of this function
    tBodyAccX <- get("body_acc_x_MERGED", dataEnvironment)
    tBodyAccY <- get("body_acc_y_MERGED", dataEnvironment)
    tBodyAccZ <- get("body_acc_z_MERGED", dataEnvironment)
    tBodyGyroX <- get("body_gyro_x_MERGED", dataEnvironment)
    tBodyGyroY <- get("body_gyro_y_MERGED", dataEnvironment)
    tBodyGyroZ <- get("body_gyro_z_MERGED", dataEnvironment)
    totalAccX <- get("total_acc_x_MERGED", dataEnvironment)
    totalAccY <- get("total_acc_y_MERGED", dataEnvironment)
    totalAccZ <- get("total_acc_z_MERGED", dataEnvironment)
    
    # Our important files X, subject and y (merged with both test and train)
    subject <- get("subject_MERGED", dataEnvironment)
    X <- get("X_MERGED", dataEnvironment)
    y <- get("y_MERGED", dataEnvironment)
    
    # Retrieve activityLabels and features variables from data env
    activityLabels <- get("activityLabelsRAW", dataEnvironment)
    features <- get("featuresRAW", dataEnvironment)
    
    # Only second column is data, first one is indexing with the row numbers
    activityLabels <- activityLabels[,2]
    
    # Only second column is data, first one is indexing with the row numbers
    features <- features[,2]
    
    # Store the current dataEnvironment list
    dataEnvironmentCurrentList <- ls(envir = dataEnvironment)
    
    # Before merging data together, fix the column names for subject and y
    names(subject) <- "Subject"
    names(y) <- "Activity"
    
    # Only get the features that contain either "mean" or "std" in colnames
    relevantFeatures <- sapply(features, function(c){grepl("mean|std", c)})
    
    # Subset only those variables that we require after this point
    allRelevantXData <- X[, relevantFeatures]
    
    # Combine all relevant data together: Subet of X variables, subject and y
    allRelevantData <- cbind(allRelevantXData, subject, y)
    
    # At the moment relevantFeatures is a logical vector, create actual features
    # used from it
    featuresUsed <- names(relevantFeatures)[relevantFeatures]
    
    # Sanitise the column names so it doesn't have "-", "(", ")" while also
    # renaming "mean" to "Mean" and "std" to "Std" for better column names
    featuresUsed <- sapply(featuresUsed, function(colname){
        gsub("\\-", "", gsub("\\(", "", gsub("\\)", "", 
        gsub("mean", "Mean", gsub("std", "Std", colname)))))
    })
    
    # Assign all data to the data env that will be used afterwards
    assign("allRelevantData", allRelevantData, dataEnvironment)
    assign("featuresUsed", featuresUsed, dataEnvironment)
    assign("activityLabels", activityLabels, dataEnvironment)
    
    # Remove all older environment list so we don't overpopulate environment
    # This will get rid of all Inertial Signals subfolder merged data
    rm(list = dataEnvironmentCurrentList, envir = dataEnvironment)
    
    # Return the same environment but this time with mean and SD objects
    dataEnvironment
}
```
  
* dataEnvironment <- nameDescriptiveActivities(dataEnvironment)
    + Fetch "allRelevantData" & "activityLabels" from data env
    + Transform all activities from number 1:6 to corresponding value from
    **activity_labels.txt** fetched data
        1. WALKING
        2. WALKING_UPSTAIRS
        3. WALKING_DOWNSTAIRS
        4. SITTING
        5. STANDING
        6. LAYING
    + Set a new data env obj "dataWithActivityLabels" and discard the rest
    + Note 'featuresUsed' is still available to name the variables appropriately


```{r}
# Step3: Uses descriptive activity names to name the activities in the data set
nameDescriptiveActivities <- function(dataEnvironment) {
    
    # Retrieve only "allRelevantData" and "activityLabel" that we need here
    allRelevantData <- get("allRelevantData", dataEnvironment)
    activityLabels <- get("activityLabels", dataEnvironment)
    
    # Rename activities from numbering 1:6 to factors with labels from
    # "activityLabels" that we retreived from the "activity_labels.txt" file
    dataWithActivityLabels <- transform(allRelevantData, 
        Activity = factor(allRelevantData$Activity, labels = activityLabels))
    
    # Since "dataWithActivityLabels" has been created, there is no need for 
    # data that built this object here
    rm(allRelevantData, activityLabels, envir = dataEnvironment)
    
    # Assign the new dataframe that has correct labelling for activities
    assign("dataWithActivityLabels", dataWithActivityLabels, dataEnvironment)
    # Return the same environment but with descriptive activity names
    dataEnvironment
}
```
  
* dataEnvironment <- labelDataset(dataEnvironment)
    + Retrived dataframe (with labels) & featuredUsed
    + Set correct column names including "Subject" & "Activity" at the end since
    we column binded in this order: X (transformed), subject (called "Subject" 
    now), y (called "Activity" now)
    + Set "fullData" to contain our final transformed data (just before 
    summarising based on Subject and Activity - next step would do that)

```{r}
# Step4: Appropriately labels the data set with descriptive variable names.
labelDataset <- function(dataEnvironment) {
    
    # Retrieved our DF and features so we can combine them together
    dataWithActivityLabels <- get("dataWithActivityLabels", dataEnvironment)
    featuresUsed <- get("featuresUsed", dataEnvironment)
    
    # Create our fullData obj and set its colnames to the featursUsed obj
    # Also the last two variables were column binded using subject and y 
    # (Column names for them set to "Subject" and "Activity" respectively)
    fullData <- dataWithActivityLabels
    colnames(fullData) <- c(featuresUsed, "Subject", "Activity")
    
    # Removed unneeded objects from our data environment
    rm(dataWithActivityLabels, featuresUsed, envir = dataEnvironment)
    
    # Assign the all imporant fullData to our data env
    assign("fullData", fullData, dataEnvironment)
    # Return the same environment 
    # but this time each object's variable appropriately labelled
    dataEnvironment
}
```
  
* tidyDataset <- createTidyDataset(dataEnvironment)
    + Retrieve "fullData" that is what we were after just before summarising
    + Using **dplyr** library, group by "Subject" & "Activity"
    + Using summarise_each, we will be able to call mean on all variable we have
    + Note this would be taking a mean of the mean and the std column we have in
    our "fullData" object
    + Arrange based on "Subject" and alphabetically "Activity" provding us with 
    a wide-format tidy dataset we needed
    + Write this tide dataset to the file with the filename "tidyDataset.txt"
    ( Note you can easily read this file using read.table(..., as.is = True) 
    function)

```{r}
# Step5: From the data set in step 4, creates a second, independent tidy 
#        data set with the average of each variable for each activity and 
#        each subject.
createTidyDataset <- function(dataEnvironment) {
    
    # Retrieve the full data that contains full data that will be grouped now
    fullData <- get("fullData", dataEnvironment)
    # Using dplyr group_by and summarise each column, create the data
    library(dplyr)
    dataTbl <- tbl_df(fullData)
    # Going to group by first Subject then Activity
    by_subject_and_activity <- group_by(dataTbl, Subject, Activity)
    # Using summarise_each we can summarise all our columns that have not been
    # grouped. This will take care of our summary we require from full data
    tidyDataset <- summarise_each(by_subject_and_activity, funs(mean))
    # Arrange tidy dataset by Subject (1:10) then alphebetically by Activity
    tidyDataset <- arrange(tidyDataset, Subject, as.character(Activity))
    # Write to "tidyDataset.txt" file & return our tidy dataset
    write.table(tidyDataset, file = "tidyDataset.txt", row.names = FALSE)
    tidyDataset
}
```
  
All parts of our run_analysis.R file have been referenced here with explanation
