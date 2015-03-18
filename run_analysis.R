library(plyr)

## reading the labels and subject files
act_labels = read.table(file="UCI HAR Dataset//activity_labels.txt", header = FALSE, quote = "", col.names = c("id","activity"))
test_labels = read.table(file="UCI HAR Dataset//test/y_test.txt", header = FALSE, quote = "", col.names = "id")
train_labels = read.table(file="UCI HAR Dataset//train/y_train.txt", header = FALSE, quote = "", col.names = "id")
train_subj = read.table(file="UCI HAR Dataset//train/subject_train.txt", header = FALSE, quote = "", col.names = "subject")
test_subj = read.table(file="UCI HAR Dataset//test/subject_test.txt", header = FALSE, quote = "", col.names = "subject")

## reading the feature file
features = read.table(file="UCI HAR Dataset//features.txt", header = FALSE, quote = "", col.names = c("id","feature"))

## reading the training and test sets
train_set_x = read.table(file="UCI HAR Dataset//train/X_train.txt", header = FALSE, quote = "", col.names = features[,2])
test_set_x = read.table(file="UCI HAR Dataset//test/X_test.txt", header = FALSE, quote = "", col.names = features[,2])

## 3 - Uses descriptive activity names to name the activities in the data set
## 4 - Appropriately labels the data set with descriptive variable names.

## Joining the activity labels and ids
train_act_label = arrange(join(train_labels,act_labels),id)
test_act_label = arrange(join(test_labels,act_labels),id)

## Adding "id", "activity" and "subject" variables to training and test sets
train_set_x$id = train_act_label$id
train_set_x$activity = train_act_label$activity
train_set_x$subject = train_subj$subject
test_set_x$id = test_act_label$id
test_set_x$activity = test_act_label$activity
test_set_x$subject = test_subj$subject

## 1 - Merges the training and the test sets to create one data set.

# Adding "type" variable and getting its number of rows to check the integrity after merging
train_set_x$type = "training"
test_set_x$type = "test"
chk_nrow_train = nrow(train_set_x)
chk_nrow_test = nrow(test_set_x)

full_df = merge(train_set_x,test_set_x, all=TRUE)

if(nrow(full_df) != (chk_nrow_test + chk_nrow_train)){
        print("[-]merge result: summ error")
} else {
        print("[+]merge result: OK")
}

## 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
ext_feat = features[grep("-mean\\(\\)|-std\\(\\)", features$feature),]
Mean_Std = full_df[,ext_feat$feature]
 
## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data = full_df[,c(1:561)]
tidy_data_avg = aggregate(tidy_data, list(full_df$activity, full_df$subject), mean)

## Saving data frame to .txt file
write.table(tidy_data_avg, file="results.txt", row.name=FALSE)