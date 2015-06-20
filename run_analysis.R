## Main project script to get, clean and produce tidy data for the project

# Fetches the required source data far the project if needed
fetchAndUnzipData <- function() {
    dataDir <- "./data/"
    if(!file.exists(dataDir)) {dir.create(dataDir)}
    datasetDir <- paste(dataDir, "UCI HAR Dataset/", sep = "")
    dataset <- paste(dataDir, "Dataset.zip", sep = "")
    if (!file.exists(dataset)) {
        sprintf("Fetching Data: %s\n", dataset)
        fileURL <- paste("https://d396qusza40orc.cloudfront.net/getdata%2F", 
                         "projectfiles%2FUCI%20HAR%20Dataset.zip", sep = "")
        download.file(fileURL, destfile = dataset, method = "curl")
        dateDownloaded <- date()
        dateDownloaded
    }
    else print("Data already exist")
    if (!file.exists(datasetDir)) {
        print("Unzipping Data")
        unzip(dataset, exdir = dataDir)
    }
    else print("Data already unzipped")
    datasetDir
}

# Step1: Merges the training and the test sets to create one data set.
mergeDatasets <- function(directory) {
    produceMergedData <- function(testFileLocation, trainFileLocation) {
        testDF <- read.table(testFileLocation, as.is = TRUE)
        trainDF <- read.table(trainFileLocation, as.is = TRUE)
        rbind(testDF, trainDF)
    }
    
    mergeData <- function(testFile, trainFile, pr) {
        testFileLocation <- paste(directory, "test/", testFile, sep = "")
        trainFileLocation <- paste(directory, "train/", trainFile, sep = "")
        testFullPathIgnoringName <- unlist(strsplit(testFileLocation, "test"))
        trainFullPathIgnoringName <- unlist(strsplit(trainFileLocation, "train"))
        if(!all(testFullPathIgnoringName == trainFullPathIgnoringName)) {
            stop("Filenames or filepaths for test & train data don't match")
        }
        mergedName <- gsub("test", "MERGED", 
                            tail(unlist(strsplit(testFileLocation, "/")), 1))
        mergedName <- gsub(".txt", "", mergedName)
        assign(mergedName, 
               produceMergedData(testFileLocation, trainFileLocation), env)
    }
    testDir <- paste(directory, "test/", sep = "")
    trainDir <- paste(directory, "train/", sep = "")
    testFiles <- list.files(testDir, recursive = TRUE)
    trainFiles <- list.files(trainDir, recursive = TRUE)
    
    env <- new.env()
    for (i in seq_along(testFiles)) {
        print(paste("Merging:", testFiles[i],"&",trainFiles[i]))
        mergeData(testFiles[i], trainFiles[i], env)
    }
    env
}

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
    subject <- get("subject_MERGED", dataEnvironment)
    totalAccX <- get("total_acc_x_MERGED", dataEnvironment)
    totalAccY <- get("total_acc_y_MERGED", dataEnvironment)
    totalAccZ <- get("total_acc_z_MERGED", dataEnvironment)
    X <- get("X_MERGED", dataEnvironment)
    y <- get("y_MERGED", dataEnvironment)
    
    # Store the current dataEnvironment list
    dataEnvironmentCurrentList <- ls(envir = dataEnvironment)
    # Fetches each current object, then adds a new object for X and SD with
    # the new environment objects with the suffix "_mean()" & "_std()" 
    addMeanSDForEachEnvObj <- function(envObj) {
        envObjValue <- get(envObj, dataEnvironment)
        envObjValueMEAN <- sapply(envObjValue, mean)
        assign(paste(envObj, "_mean()", sep = ""), 
               envObjValueMEAN, dataEnvironment)
        envObjValueSD <- sapply(envObjValue, sd)
        assign(paste(envObj, "_std()", sep = ""), 
               envObjValueSD, dataEnvironment)
    }
    # Apply addMeanSDForEachEnvObj to each environment object that will add
    # mean and SD environment objects for each current object
    sapply(dataEnvironmentCurrentList, addMeanSDForEachEnvObj)
    
    # Remove all older environment list so we don't overpopulate environment
    rm(list = dataEnvironmentCurrentList, envir = dataEnvironment)
    
    # Return the same environment but this time with mean and SD objects
    dataEnvironment
}

# Step3: Uses descriptive activity names to name the activities in the data set
nameDescriptiveActivities <- function(dataEnvironment) {
    # Store the current dataEnvironment list
    dataEnvironmentCurrentList <- ls(envir = dataEnvironment)
    
    # Change all dynamic names (all objects in dataEnvironment have not been 
    # used with their definitive names yet) to descriptive activity names
    # ********************* acc **********************
    bodyAccXMEAN <- get("body_acc_x_MERGED_mean()", dataEnvironment)
    assign("bodyAccXMEAN", bodyAccXMEAN, dataEnvironment)
    bodyAccXSD <- get("body_acc_x_MERGED_std()", dataEnvironment)
    assign("bodyAccXSD", bodyAccXSD, dataEnvironment)
    
    bodyAccYMEAN <- get("body_acc_y_MERGED_mean()", dataEnvironment)
    assign("bodyAccYMEAN", bodyAccYMEAN, dataEnvironment)
    bodyAccYSD <- get("body_acc_y_MERGED_std()", dataEnvironment)
    assign("bodyAccYSD", bodyAccYSD, dataEnvironment)
    
    bodyAccZMEAN <- get("body_acc_z_MERGED_mean()", dataEnvironment)
    assign("bodyAccZMEAN", bodyAccZMEAN, dataEnvironment)
    bodyAccZSD <- get("body_acc_z_MERGED_std()", dataEnvironment)
    assign("bodyAccZSD", bodyAccZSD, dataEnvironment)
    
    # ********************* gyro *********************
    bodyGyroXMEAN <- get("body_gyro_x_MERGED_mean()", dataEnvironment)
    assign("bodyGyroXMEAN", bodyGyroXMEAN, dataEnvironment)
    bodyGyroXSD <- get("body_gyro_x_MERGED_std()", dataEnvironment)
    assign("bodyGyroXSD", bodyGyroXSD, dataEnvironment)
    
    bodyGyroYMEAN <- get("body_gyro_y_MERGED_mean()", dataEnvironment)
    assign("bodyGyroYMEAN", bodyGyroYMEAN, dataEnvironment)
    bodyGyroYSD <- get("body_gyro_y_MERGED_std()", dataEnvironment)
    assign("bodyGyroYSD", bodyGyroYSD, dataEnvironment)
    
    bodyGyroZMEAN <- get("body_gyro_z_MERGED_mean()", dataEnvironment)
    assign("bodyGyroZMEAN", bodyGyroZMEAN, dataEnvironment)
    bodyGyroZSD <- get("body_gyro_z_MERGED_std()", dataEnvironment)
    assign("bodyGyroZSD", bodyGyroZSD, dataEnvironment)
    
    # ******************* subject ********************
    subjectMEAN <- get("subject_MERGED_mean()", dataEnvironment)
    assign("subjectMEAN", subjectMEAN, dataEnvironment)
    subjectSD <- get("subject_MERGED_std()", dataEnvironment)
    assign("subjectSD", subjectSD, dataEnvironment)
    
    # ****************** total_acc *******************
    totalAccXMEAN <- get("total_acc_x_MERGED_mean()", dataEnvironment)
    assign("totalAccXMEAN", totalAccXMEAN, dataEnvironment)
    totalAccXSD <- get("total_acc_x_MERGED_std()", dataEnvironment)
    assign("totalAccXSD", totalAccXSD, dataEnvironment)
    
    totalAccYMEAN <- get("total_acc_y_MERGED_mean()", dataEnvironment)
    assign("totalAccYMEAN", totalAccYMEAN, dataEnvironment)
    totalAccYSD <- get("total_acc_y_MERGED_std()", dataEnvironment)
    assign("totalAccYSD", totalAccYSD, dataEnvironment)
    
    totalAccZMEAN <- get("total_acc_z_MERGED_mean()", dataEnvironment)
    assign("totalAccZMEAN", totalAccZMEAN, dataEnvironment)
    totalAccZSD <- get("total_acc_z_MERGED_std()", dataEnvironment)
    assign("totalAccZSD", totalAccZSD, dataEnvironment)
    
    # ********************** X ***********************
    XMEAN <- get("X_MERGED_mean()", dataEnvironment)
    assign("XMEAN", XMEAN, dataEnvironment)
    XSD <- get("X_MERGED_std()", dataEnvironment)
    assign("XSD", XSD, dataEnvironment)
    
    # ********************** y ***********************
    yMEAN <- get("y_MERGED_mean()", dataEnvironment)
    assign("yMEAN", yMEAN, dataEnvironment)
    ySD <- get("y_MERGED_std()", dataEnvironment)
    assign("ySD", ySD, dataEnvironment)
    
    # Remove all older environment list so we don't overpopulate environment
    rm(list = dataEnvironmentCurrentList, envir = dataEnvironment)
    
    # Return the same environment but with descriptive activity names
    dataEnvironment
}

# Step4: Appropriately labels the data set with descriptive variable names.
labelDataset <- function(dataEnvironment) {
    # Store the current dataEnvironment list
    dataEnvironmentCurrentList <- ls(envir = dataEnvironment)
    
    # Name each variable for an object name in environment matching its object
    # E.g. V1 will be changed to V1 for "bodyAccXSD" object
    # Then assign the same object into dataEnvironment
    correctlyLabelObj <- function(envObj) {
        envObjValue <- get(envObj, dataEnvironment)
        objColNames <- names(envObjValue)
        newColNames <- sapply(objColNames, 
                              function(colName) gsub("V", envObj, colName))
        names(envObjValue) <- newColNames
        assign(envObj, envObjValue, dataEnvironment)
    }
    # Change variable names for each environment object with descriptive
    # variable names rather than V1, V2, V3, ...etc.
    # Then assign that same object back in the environment
    sapply(dataEnvironmentCurrentList, correctlyLabelObj)
    
    # Return the same environment 
    # but this time each object's variable appropriately labelled
    dataEnvironment
}

# Step5: From the data set in step 4, creates a second, independent tidy 
#        data set with the average of each variable for each activity and 
#        each subject.
createTidyDataset <- function(dataEnvironment) {
    dataEnvironment
}


############ Start Analysis ############
datasetDir <- fetchAndUnzipData()
dataEnvironment <- mergeDatasets(datasetDir)
dataEnvironment <- extractDataset(dataEnvironment)
dataEnvironment <- nameDescriptiveActivities(dataEnvironment)
dataEnvironment <- labelDataset(dataEnvironment)
tidyDataset <- createTidyDataset(dataEnvironment)
########### Finish Analysis ############