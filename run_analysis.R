### Load requisite libraries
library(plyr)


## Step 1: Read in train data and test data and merge for X and Y, respectively

# Get X-data
trainX <- read.table("UCI HAR Dataset/train/X_train.txt")
testX <- read.table("UCI HAR Dataset/test/X_test.txt")
allX <- rbind(trainX, testX)

# Get Y-data
trainY <- read.table("UCI HAR Dataset/train/Y_train.txt")
testY <- read.table("UCI HAR Dataset/test/Y_test.txt")
allY <- rbind(trainY, testY)


## Step 2: Extract only features of import (mean/sd)

# Get features
features <- read.table("UCI HAR Dataset/features.txt")

# Select columns containing "mean" or "std"
featureSelect <- grep(".*mean.*|.*std.*", features[,2])
X.features <- allX[, featureSelect]


## Step 3: Uses descriptive activity names to name the activities in the data set

# Get label-data
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[,2] <- as.character(activities[,2])

# Apply labels
allY[, 1] <- activities[allY[, 1], 2]
names(allY) <- "activity"

## Step 4: Appropriately labels the data set with descriptive variable names.

# Clean variable labels
varNames <- features[featureSelect,2]
varNames = gsub('-mean', 'Mean', varNames)
varNames = gsub('-std', 'Std', varNames)
varNames <- gsub('[-()]', '', varNames)

# Apply variable labels
names(X.features) <- varNames


## Step 5: Merge all clean data together

# Get subject-data
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
allSubjects <- rbind(trainSubjects, testSubjects)
names(allSubjects) <- "subject"

# Merge
tidy <- cbind(allSubjects, allY, X.features)


## Step 6: From step 5, creates second, independent tidy data set with 
## the average of each variable for each activity and each subject

# Calculate averages by subject/activity
tidy.final <- ddply(tidy, .(subject, activity), 
                    function(dta) colMeans(dta[, -match(c("subject","activity"), names(dta))]))

# Reflect averaging in variable names
names(tidy.final)[-match(c("subject","activity"), names(tidy.final))] <- 
    paste("Mean.", names(tidy.final)[-match(c("subject","activity"), names(tidy.final))], sep = "")

# Export data
write.table(tidy.final, "tidy_final.txt", row.names = FALSE)