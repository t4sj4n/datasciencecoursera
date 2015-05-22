run_analysis <- function()
{
    library(data.table)
    library(dplyr)    
    
    # change datasetPath if outside your WD
    datasetPath <- file.path(getwd(), "UCI HAR Dataset")
    
    
    # load data from files
    
    dtSubjectTrain <- read.table(file.path(datasetPath, "train", "subject_train.txt"))
    dtSubjectTest  <- read.table(file.path(datasetPath, "test" , "subject_test.txt" ))
    setnames(dtSubjectTrain, 1, "subject")
    setnames(dtSubjectTest, 1, "subject")
    
    dtActivityTrain <- read.table(file.path(datasetPath, "train", "Y_train.txt"))
    dtActivityTest  <- read.table(file.path(datasetPath, "test" , "Y_test.txt" ))
    setnames(dtActivityTrain, 1, "activityNum")
    setnames(dtActivityTest, 1, "activityNum")
    
    dtTrain <- read.table(file.path(datasetPath, "train", "X_train.txt"))
    dtTest  <- read.table(file.path(datasetPath, "test" , "X_test.txt" ))
    
    features <- read.table(file.path(datasetPath, "features.txt"), stringsAsFactors = FALSE)
    
    activityLables <- read.table(file.path(datasetPath, "activity_labels.txt"), stringsAsFactors = FALSE)
    setnames(activityLables, 1:2, c("activityNum","activityName"))
    
          
    # merge train and test data sets
    dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
    dtActivity <- rbind(dtActivityTrain, dtActivityTest)
    dt <- rbind(dtTrain, dtTest)
    
    # labels the data set with descriptive variable names 
    setnames(dt, 1:ncol(dt), features[,2])

    # Extracts only the measurements on the mean and standard deviation for each measurement.
    mean_and_std_cols <- grep("mean\\(|std\\(", features[,2])
    dt <- dt[,mean_and_std_cols]
    
    # Uses descriptive activity names to name the activities in the data set
    dtActivity <- merge(dtActivity, activityLables, by = "activityNum")
    dtActivity <- dtActivity[2]
    
    # merge to one data set
    merged_data <- cbind(dt, dtActivity, dtSubject)
    
    # create tidy data set
    tidy_data <- merged_data %>% group_by(subject, activityName) %>% summarise_each(funs(mean))
   
    # write tidy data to file
    write.table(tidy_data, file="./tidy_data.txt", sep="\t", row.names=FALSE)
  
}


