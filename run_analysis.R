# Clear the workspace
rm(list=ls())
# Que.1
# Read data files in
Features     <- read.table('./UCI HAR Dataset/features.txt',header=FALSE)
xTrain       <- read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       <- read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)
activityType <- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
subjectTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)

# Check the structure of each file
str(Features)
str(activityType)
str(yTrain)
str(xTrain)
str(subjectTrain)

#Properly name the columns of respective files
colnames(subjectTrain)<-"subjectId"
colnames(activityType)<- c("activityId", "activityType")
colnames(yTrain)<- "activityId"
colnames(xTrain)<- Features[, 2]

# Merge the files to create training Data
trainingData=cbind(yTrain, subjectTrain, xTrain)

# Check the new table
head(trainingData)

# Read the test data in to create the test table
subjectTest <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest       <- read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest       <- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)

# Check the structure of new tables read in
str(subjectTest)
str(xTest)
str(yTest)

# Properly name the columns of imported files
colnames(subjectTest) <- "subjectId"
colnames(xTest)<- Features[, 2]
colnames(yTest)<- "activityId"

# Merge the xTest, yTest and subjectTest data to create the test data
testData = cbind(yTest,subjectTest,xTest);

# Check the newly created table
head(testData)

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = Features[,2]; 
colnames(yTrain)        = "activityId";

# Create the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Combine training data and test data to create the final table
finalData = rbind(trainingData,testData)

# Que 2. Extract only the measurements on the mean and standard deviation for each measurement. 


# A vector for the column names from the finalData, to select the required mean() & stddev() columns
colNames  <- colnames(finalData)

# Need to create logicalVector that contains TRUE values for the ID, mean() & stddev() columns
# and FALSE for remaining
LogicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset the finalData table based on the logicalVector
finalData = finalData[LogicalVector==TRUE]
head(finalData)
dim(finalData)


#Que 3
# Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

#Check changes to the new final data 
dim(finalData)
# Update the colNames vector to include the new column names after merge
colNames  = colnames(finalData)
head(finalData)

# Que 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("-std$","StdDeviation",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("^(f)","Freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassign the new descriptive column names to the finalData
colnames(finalData) = colNames

# Que 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table without the activity type column
finalData2  = finalData[,names(finalData) != 'activityType'];

# Summarise finalData with no activity type, including just the mean of each variable, activity and subject
tidyData    = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],by=list(activityId=finalData2$activityId,subjectId = finalData2$subjectId),mean)

# Merge the tidyData with activityType for respective descriptive acitvity names
myTidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export my own tidyData 
write.table(tidyData, './myTidyData.txt',row.names=TRUE,sep='\t')

