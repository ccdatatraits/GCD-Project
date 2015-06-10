## Main project script to get, clean and produce tidy data for the project

# Fetches the required source data far the project if needed
fetchAndUnzipData <- function() {
    datasetDir <- 'UCI HAR Dataset'
    dataset <- 'Dataset.zip'
    if (!file.exists(dataset)) {
        sprintf('Fetching Data: %s\n', dataset)
        fileURL <- paste('https://d396qusza40orc.cloudfront.net/getdata%2F', 
                         'projectfiles%2FUCI%20HAR%20Dataset.zip', sep = '')
        download.file(fileURL, destfile = 'Dataset.zip', method = 'curl')
        dateDownloaded <- date()
        dateDownloaded
    }
    else print('Data already exist')
    if (!file.exists(datasetDir)) {
        print('Unzipping Data')
        unzip(dataset)
    }
    else print('Data already unzipped')
    datasetDir
}

createFakeDataset <- function() {
    L3 <- LETTERS[1:3]
    fac <- sample(L3, 10, replace = TRUE)
    (d <- data.frame(x = 1, y = 1:10, fac = fac))
    ## The "same" with automatic column names:
    data.frame(1, 1:10, sample(L3, 10, replace = TRUE))
}

# Step1: Merges the training and the test sets to create one data set.
mergeDatasets <- function(directory) {
    print(directory)
    createFakeDataset()
}

# Step2: Extracts only the measurements on the mean and 
#        standard deviation for each measurement. 
extractDataset <- function(dataset) {
    print(dataset)
}

# Step3: Uses descriptive activity names to name the activities in the data set
nameDescriptiveActivities <- function(dataset) {
    print(dataset)
    dataset
}

# Step4: Appropriately labels the data set with descriptive variable names.
labelDataset <- function(dataset) {
    print(dataset)
    dataset
}

# Step5: From the data set in step 4, creates a second, independent tidy 
#        data set with the average of each variable for each activity and 
#        each subject.
createTidyDataset <- function(dataset) {
    print(dataset)
    dataset
}


############ Start Analysis ############
datasetDir <- fetchAndUnzipData()
oneDataset <- mergeDatasets(datasetDir)
extractedDataset <- extractDataset(oneDataset)
namedDataset <- nameDescriptiveActivities(extractedDataset)
labelledDataset <- labelDataset(namedDataset)
tidyDataset <- createTidyDataset(labelledDataset)
########### Finish Analysis ############