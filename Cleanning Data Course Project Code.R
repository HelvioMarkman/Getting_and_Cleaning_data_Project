url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(url = url,destfile = 'dataset.zip')

unzip(zipfile = 'dataset.zip')

#train.x <- read.fwf(file = 'UCI HAR Dataset\\train\\x_train.txt',widths = rep(16,561))
train.x <- readRDS('train_x.rda')
head(train.x[,1:5],5)
#test.x <- read.fwf(file = 'UCI HAR Dataset\\test\\x_test.txt',widths = rep(16,561))
test.x <- readRDS('test_x.rda')
head(test.x[,1:5],5)

train.y <- read.table(file = 'UCI HAR Dataset\\train\\y_train.txt')
str(train.y)
test.y <- read.table(file = 'UCI HAR Dataset\\test\\y_test.txt')
str(test.y)

### Reading features

features <- read.table(file = 'UCI HAR Dataset\\features.txt',sep=' ')
str(features)

### Reading Activity Labes
activity_labels <- read.table(file = 'UCI HAR Dataset\\activity_labels.txt',sep=' ')
names(activity_labels) = c('id','activity')
head(activity_labels)

### Reading Subject
train.subject <- read.table(file = 'UCI HAR Dataset\\train\\subject_train.txt')
str(train.subject)
test.subject <- read.table(file = 'UCI HAR Dataset\\test\\subject_test.txt')
str(test.subject)


###Label inputs and extracting means only (Question 4 and 2)
names(train.x)=features[,2]

names(test.x)=features[,2]

means_coluns = grepl("mean|std",names(train.x))
train.x.means <- train.x[,means_coluns]
test.x.means <- test.x[,means_coluns]
head(train.x.means[,1:5])
head(test.x.means[,1:5])


### Join subject and outputs for train and Activities label (Question 3)
train.tidy <- train.x.means
train.tidy[,'act_id'] <- train.y
train.tidy[,'subject'] <- train.subject
train.tidy <- merge(train.tidy,activity_labels,by.x='act_id',by.y='id',all=TRUE)

str(train.tidy)

### Join subject and outputs for test and Activities label (Question 3)
test.tidy <- test.x.means
test.tidy[,'act_id'] <- test.y
test.tidy[,'subject'] <- test.subject
test.tidy <- merge(test.tidy,activity_labels,by.x='act_id',by.y='id',all=TRUE)

str(test.tidy)


###Merge Train and test data sets (question 1)
tidy <- rbind(train.tidy,test.tidy)
str(tidy)

hide_cols <- (names(tidy) != 'activity')

####(Question 5)
tidy.summary <- aggregate(tidy[,hide_cols],by= list(tidy$act_id, tidy$subject),mean)
tidy.summary <- merge(tidy.summary,activity_labels,by.x='act_id',by.y='id')
ts_order <- order(tidy.summary$subject)
tidy.summary[ts_order[1:5],c('activity','subject','tBodyAcc-mean()-Y','tBodyAcc-mean()-Z')]

hide_cols <- grep("Activity|subject",names(tidy.summary))
ntidy.summary <- data.frame(subject = tidy.summary[,'subject'],Activity = tidy.summary[,'activity'], tidy.summary[,-hide_cols])

write.table(ntidy.summary,file = 'tidy_summary.txt',row.names = FALSE)
