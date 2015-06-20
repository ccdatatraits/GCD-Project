# GCD-Project
Getting &amp; Cleaning Data - Course Project

# Overview

## Code Structure
run_analysis.R is broken into various top level functions

* datasetDir <- fetchAndUnzipData()
    + Creates data directory and creates it if needed
    + Checks that the original ZIP in placed in the directory or not, 
    otherwise retrieves it from URL provided
    + Outputs dateDownloaded for the ZIP, if file was retrieved
    + Checks whether data has been unzipped or not, otherwise unzip the
    downloaded data
    + Returns directory location to where files were retrieved and unzipped
  
* dataEnvironment <- mergeDatasets(datasetDir)
    + Uses the dataset directory (datasetDir) to fetch each file in both 
    test and train folder
    + Makes sure that relative path for each file for test has a corresponding 
    train file. Otherwise the program stops
    + Creates a MERGED object for each combination of test and train data
        + e.g. X_test.txt and X_train.txt is merged into an object: X_MERGED
    + All of these objects are placed into a new dataEnvironment which is 
    created only for the purpose of data that will be used in the 
    following steps
    + All subsequent calls pass this new environment to get and store 
    data objects
  
* dataEnvironment <- extractDataset(dataEnvironment)

  
* dataEnvironment <- nameDescriptiveActivities(dataEnvironment)

  
* dataEnvironment <- labelDataset(dataEnvironment)

  
* tidyDataset <- createTidyDataset(dataEnvironment)

  
# Conclusion
