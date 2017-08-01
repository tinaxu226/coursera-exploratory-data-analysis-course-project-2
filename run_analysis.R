# 1. Read in the data from files
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subjectTest)  = "subjectId";
colnames(xTest)        = features[,2]; 
colnames(yTest)        = "activityId";

# Merge the training and the test sets to create one data set
humanActivity <- rbind(
  cbind(subjectTrain,xTrain,yTrain),
  cbind(subjectTest,xTest,yTest)
)

#2. Extract only the measurements on the mean and standard deviation
#for each measurement
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

#3. Use descriptive activity names to name the activities 
#in the data set
humanActivity <- merge(humanActivity,activityType,by='activityId',all.x=TRUE);

# 4. Appropriately label the data set with descriptive activity names. 
humanActivityCols <- colnames(humanActivity)
# Cleaning up the variable names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

#5.Create a second, independent tidy set with the average of each
#variable for each activity and each subject
secTidySet <- aggregate(. ~subjectId + activityId, humanActivity, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
#write second tidy set into txt file
write.table(secTidySet, "secTidySet.txt", row.name=FALSE, sep="\t", quote = FALSE)
