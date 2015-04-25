if (getwd()!= "C:/Users/Paul/Documents/R/GettingData")
{setwd( "C:/Users/Paul/Documents/R/GettingData")}
library(dplyr)

## Begin by importing the data and assigning each to a variable
x_train <- read.table("~/R/GettingData/UCI HAR Dataset/train/X_train.txt")
y_train <-read.table("~/R/GettingData/UCI HAR Dataset/train/y_train.txt") 

x_test <- read.table("~/R/GettingData/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("~/R/GettingData/UCI HAR Dataset/test/y_test.txt")
Variable.names<-read.table("~/R/GettingData/UCI HAR Dataset/variables")

subjects_training <- read.table("~/R/GettingData/UCI HAR Dataset/train/subject_train.txt")
subjects_testing <- read.table("~/R/GettingData/UCI HAR Dataset/test/subject_test.txt")


# 1.Merges the training and the test sets to create one data set.

#merge the data in parts (x, y and subject)
motionDataX <- rbind(x_train, x_test)
names(motionDataX)<-Variable.names[,2]
# create 'y' data set
motionDataY <- rbind(y_train, y_test)
names(motionDataY) <- c("activity")

subjects <- rbind(subjects_training, subjects_testing)
names(subjects)<-c("subject")

myData1 <- cbind(subjects,motionDataY)
myData2 <- cbind(motionDataX,myData1)
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
MeansandStds<-Variable.names$V2[grep("mean\\(\\)|std\\(\\)", Variable.names$V2)]

desiredMeasurements<-c(as.character(MeansandStds), "subject", "activity")

myData3 <- subset(myData2, select=desiredMeasurements)                                



# 3.Uses descriptive activity names to name the activities in the data set
act =c("1 WALKING","2 WALKING_UPSTAIRS","3 WALKING_DOWNSTAIRS","4 SITTING","5 STANDING","6 LAYING")

myData3$activity <- as.character(myData3$activity)
myData3$activity[myData3$activity == "1"] <- "WALKING"
myData3$activity[myData3$activity == "2"] <- "WALKING_UPSTAIRS"
myData3$activity[myData3$activity == "3"] <- "WALKING_DOWNSTAIRS"
myData3$activity[myData3$activity == "4"] <- "SITTING"
myData3$activity[myData3$activity == "5"] <- "STANDING"
myData3$activity[myData3$activity == "6"] <- "LAYING"

myData3$activity <- as.factor(myData3$activity)

# 4.Appropriately labels the data set with descriptive variable names. 
names(myData3)<-gsub("^t", "time", names(myData3))
names(myData3)<-gsub("^f", "frequency", names(myData3))
names(myData3)<-gsub("Acc", "Accelerometer", names(myData3))
names(myData3)<-gsub("Gyro", "Gyroscope", names(myData3))
names(myData3)<-gsub("Mag", "Magnitude", names(myData3))
names(myData3)<-gsub("BodyBody", "Body", names(myData3))
# 5.From the data set in step 4, creates a second, independent tidy data set
#  with the average of each variable for each activity and each subject.

myData4<-aggregate(. ~subject + activity, myData3, mean)
myData4<-myData4[order(myData4$subject,myData4$activity),]
endTable <-myData4
## out put step
write.table(endTable, file ="C:/Users/Paul/Documents/R/GettingData/endtable.txt",row.names=FALSE)
