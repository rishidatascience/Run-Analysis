library(plyr)
### Run the setwd() to set the current working directory 

run_analysis <- function() {

downloadData <- function(assgn.dir = "./data")
{
	### Utility Function 
	filePath <- function(...) { paste(..., sep = "/") }

    
    	### Check if the Input Directory Exists, if not create it
    	if(!file.exists(assgn.dir)) {
        dir.create("./data")
    	}

 	### Setting Local Variables
    	fileToDownload <- paste(assgn.dir, "dataset.zip", sep="/")
    	print(fileToDownload)
    	dirDownload  <- paste(assgn.dir, "UCI HAR Dataset", sep="/")
    	print(dirDownload)

 	### Download the File from given URL    
    	if(!file.exists(fileToDownload)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, fileToDownload)
    	}

    ### Extract the Zip file in the directory specified in exdir.
       if (!file.exists(dirDownload)) {
        unzip(fileToDownload, exdir = dirDownload)
   	}
  
}
  ### Call DownloadData function to download and unzip the dataset Zip File
  result <- downloadData() 
 
   ### Question 1 - Merges the training and the test sets to create one data set.
   message("Extracting the Training and Test Data from X files")
   TrainX <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
   TestX  <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt") 
   message("Merge the Training and Test Data from X files")
   mergedX <- rbind(TrainX, TestX)

   message("X files data merged")

  ### Extract the featureName column and assign it to Names of Merged Data 
  names(mergedX) <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/features.txt")[, 2]
  message("Updated names from features into Merged Data")

   #### Question 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
   limitedX <- mergedX[, grep("(mean|std)\\(\\)", names(mergedX))]
   message("Subsetted the data with names with STD or MEAN Headers")

   ### Read and Merge Activity data
   TrainY <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
   TestY <-  read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
   MergedY <- rbind(TrainY, TestY)[, 1]
   message("Extracting & Merged the Training and Test Data from Y files")


 
   ### Question -3 Uses descriptive activity names to name the activities in the data set
   activityNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
   actY <- activityNames[MergedY]
   message("Set the factor Actvities values in merged Y Tables") 
   

   ### Question - 4 Appropriately labels the data set with descriptive variable names
   names(limitedX) <- gsub("^t", "Time", names(limitedX))
   names(limitedX) <- gsub("^f", "Frequency", names(limitedX))
   names(limitedX) <- gsub("-mean\\(\\)", "Mean", names(limitedX))
   names(limitedX) <- gsub("-std\\(\\)", "StdDev", names(limitedX))
   names(limitedX) <- gsub("-", "", names(limitedX))
   names(limitedX) <- gsub("BodyBody", "Body", names(limitedX))

  message("Re-formated Header Names")

  # Create the Subject Data 
  TrainS <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
  TestS  <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
  subjects <- rbind(TrainS, TestS)[, 1]
 
  message("Extracted & Merged the Subject data")

  ### Join the Subjects, Activities and Limited Data) 
  SubjActyData <- cbind(Subject = subjects, Activity = actY, limitedX)

 message("Joined Subject, Activity & X tables")

   
  #### Question - 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject 
  
 ### limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
  tidyMeans <- ddply(SubjActyData, .(Subject, Activity), function(x) { colMeans(x[,-c(1,2)]) })
  
 message("Grouped the data w.r.to Subject & Activity")
 
  #### Write to afile
  write.table(tidyMeans, "tidyMeansData.txt", row.names = FALSE)
  message("Written data to tidyMeansdata.txt")
  tidyMeans
}

res  = run_analysis()
