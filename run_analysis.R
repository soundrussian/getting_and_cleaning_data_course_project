library(dplyr)

# Download source data if we don't have it
if (!file.exists("./UCI HAR Dataset")) {
  zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zipUrl, "data.zip", method = "curl")
  unzip("data.zip")
  if(file.exists("data.zip")) { file.remove("data.zip") }
}

# Load train data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Combine variables into one dataset
train_data <- cbind(x_train, y_train, subjects_train)

# Load test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Combine variables into one dataset
test_data <- cbind(x_test, y_test, subjects_test)


# Merge train and test data together
merged_data <- rbind(train_data, test_data)

# Load features names
features <- read.table("./UCI HAR Dataset/features.txt")
featuresNames <- features[,2]

# Set feature names as column names
colnames(merged_data) <- c(as.character(featuresNames), 'activity', 'subject')

# Select columns containing mean and std measurements, and add activity and subject to them
meanAndStdColumns <- grep('mean\\(|std\\(', names(merged_data), value = TRUE)
meanStdActivityAndSubjectColumns <- c(meanAndStdColumns, 'activity', 'subject')

# Select only required data (activity, subject, and mean and std measurements)
meanAndStdData <- merged_data[, meanStdActivityAndSubjectColumns]

# Function to clean column names
# - removes ()
# - replaces - with _
# - converts to lower case
cleanFun <- function(name) {
  tolower(
    gsub("-", "_", 
         gsub("\\(|\\)", "", name
         )
    )
  )
}

# Clean column names
cleanNames <- sapply(names(meanAndStdData), cleanFun, USE.NAMES = FALSE)
colnames(meanAndStdData) <- cleanNames

# Read activities labels and factorize activity column
activities <- read.table('./UCI HAR Dataset/activity_labels.txt')
meanAndStdData$activity <- factor(meanAndStdData$activity, labels = tolower(activities[, 2]))

# Get tidy set by grouping by subject and activity and calculating mean for all other columns
tidySet <- meanAndStdData %>%
            group_by(subject, activity) %>%
            summarise_all(list(mean = mean))

# Create output dir if it doesn't exist
if(!file.exists('./output')) { dir.create('./output') }

# Write tidy set
outputFile <- "./output/tidy_set.txt"
write.table(tidySet, file = outputFile, sep = ",")
