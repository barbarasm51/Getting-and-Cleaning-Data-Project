## R code used to complete the class project for "Getting and Cleaning Data", due on July 26, 2015
## Data for the project was obtained from the following site: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## Data was manually downloaded into desktop folder "./Coursera"

## First set working directory and load "dplyr" library

setwd("/Users/barbaramahoney/Desktop/Coursera")

library(dplyr)
library(reshape)

## The first dataset loaded provides a list of the features measured for each test event, common to both training and testing
## Create a vector of features to be used to name the initial x file columns

features=read.csv("~/Desktop/Coursera/UCI HAR Dataset/features.txt",header=FALSE,sep="")
featurevector=features[,2]

## The next set of commands read in the x_train and x_test files.  For each of the original data files
## create a data frame which adds labeling information to the measurements as follows:
## column names are added as the data is read in, using the featurevector created from features.txt
## Assumption is that the columns are in the same order as found in features.txt
## (though no explicit reference was found to confirm this)
## xsource.file holds the original datafile name for reference
## xevent.number holds the consecutive row numbers as data was read in
## Assumption is that each row represents a set of measurements around a single event
## The y_train and y_test files are assumed to be in the same event order (though this is nowhere explicitly stated)
## The "tidy" reference is added in order to denote that the column names are descriptive, though the data is still wide

xtrain=read.csv("~/Desktop/Coursera/UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="",col.names=featurevector)
xtrainids=data.frame(xsource.file=rep("X_train.txt",nrow(xtrain)),xevent.number=as.numeric(rownames(xtrain)))
xtraintidy=cbind(xtrainids,xtrain)

xtest=read.csv("~/Desktop/Coursera/UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="",col.names=featurevector)
xtestids=data.frame(xsource.file=rep("X_test.txt",nrow(xtest)),xevent.number=as.numeric(rownames(xtest)))
xtesttidy=cbind(xtestids,xtest)

## Now read in the activity labels file.  This is the file used to decode activity numbers in the y_train and y_test files

activitylabels=read.csv("~/Desktop/Coursera/UCI HAR Dataset/activity_labels.txt",header=FALSE,sep="",col.names=c("Activity Number","Activity Name"))

## The next set of commands read in the y_train and y_test files.  For each of the original data files
## create a data frame which adds labeling information to the measurements as follows:
## ysource.file holds the original datafile name for reference
## yevent.number holds the consecutive row numbers as data was read in
## Assumption is that each row represents a set of measurements around a single event
## The x_train and x_test files are assumed to be in the same event order (though this is nowhere explicitly stated)

ytrain=read.csv("~/Desktop/Coursera/UCI HAR Dataset/train/Y_train.txt",header=FALSE,sep="",col.names="Activity")
ytrainids=data.frame(ysource.file=rep("Y_train.txt",nrow(ytrain)),yevent.number=as.numeric(rownames(ytrain)))
ytrainnew=cbind(ytrainids,ytrain)

ytest=read.csv("~/Desktop/Coursera/UCI HAR Dataset/test/Y_test.txt",header=FALSE,sep="",col.names="Activity")
ytestids=data.frame(ysource.file=rep("Y_test.txt",nrow(ytest)),yevent.number=as.numeric(rownames(ytest)))
ytestnew=cbind(ytestids,ytest)

## In the next steps, code is adding an Activity name to each row by matching on activity.number against the activitylabels DT
## The last step is to re-sort the resulting DT on event.number so that y file rows can be later matched correctly
## to x file rows.  This is necessary because the merge function changes the original order of the merged file.

ytrainnew2=merge(ytrainnew,activitylabels,by.x="Activity",by.y="Activity.Number")
ytraintidy=arrange(ytrainnew2,yevent.number)

ytestnew2=merge(ytestnew,activitylabels,by.x="Activity",by.y="Activity.Number")
ytesttidy=arrange(ytestnew2,yevent.number)

## The events described in the x and y files are associated with 30 numbered subjects (i.e.people)
## The number of rows of subject_train and subject_test files are the same as the number of events described in x and y
## The following code reads the subject files and creates data tables to be combined with the other data
## Labelling information is added, including the source file name and the event number 

trainsubj=read.csv("~/Desktop/Coursera/UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="",col.names="Subject Number")
trainsubjids=data.frame(subjsource.file=rep("subject_train.txt",nrow(trainsubj)),subjevent.number=as.numeric(rownames(trainsubj)))
trainsubjtidy=cbind(trainsubjids,trainsubj)

testsubj=read.csv("~/Desktop/Coursera/UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="",col.names="Subject Number")
testsubjids=data.frame(subjsource.file=rep("subject_test.txt",nrow(testsubj)),subjevent.number=as.numeric(rownames(testsubj)))
testsubjtidy=cbind(testsubjids,testsubj)

## The next step is to add columns to the tidy x tables (train and test) from the tidy y and subject files.
## This is valid at this stage because we've maintained the order of each dataset, x, y and subject

trainset=cbind(trainsubjtidy$Subject.Number,ytraintidy$Activity,ytraintidy$Activity.Name,xtraintidy)
testset=cbind(testsubjtidy$Subject.Number,ytesttidy$Activity,ytesttidy$Activity.Name,xtesttidy)

## The following steps reshape the resulting trainset or testset file by transposing feature from column to row, from wide to long
## The final steps create clearer and also consistent column names before we rbind the training and testing data together

trainsettidy=melt(trainset,id=c("xsource.file","xevent.number","trainsubjtidy$Subject.Number","ytraintidy$Activity","ytraintidy$Activity.Name"))
trainsettidy1=rename(trainsettidy,c("trainsubjtidy$Subject.Number"="subject.number","ytraintidy$Activity"="activity","ytraintidy$Activity.Name"="activity.name","variable"="feature","value"="measurement"))

testsettidy=melt(testset,id=c("xsource.file","xevent.number","testsubjtidy$Subject.Number","ytesttidy$Activity","ytesttidy$Activity.Name"))
testsettidy1=rename(testsettidy,c("testsubjtidy$Subject.Number"="subject.number","ytesttidy$Activity"="activity","ytesttidy$Activity.Name"="activity.name","variable"="feature","value"="measurement"))

## This step creates unique numbering for the upcoming rbind between train and test.  Prior to this step,
## the numbers were ambiguous, with both data sets having rows and therefore events numbered 1,2,3,...
## We arbitrarily re-number the training set events to chronologically follow the test set events.
## So now there is a common column named (unique) event.number

trainsettidy2=mutate(trainsettidy1,event.number=xevent.number + max(testsettidy1$xevent.number))
testsettidy2=mutate(testsettidy1,event.number=xevent.number)

## We use a simple rbind command to append the test set to the training set, and also drop the xevent.number for clarity's sake.
## combodata now completes step 1 in the project assignment:  
## "Merges the training and the test sets to create one data set."

combodata=rbind(filter(trainsettidy2[,c(1,3:8)]),filter(testsettidy2[,c(1,3:8)]))

## The following code subsets the combodata to pick up only the rows in which the feature name contains the character
## string "mean" or "std" to create subcombodata, which completes step 2 in the project assignment:
## "Extracts only the measurements on the mean and standard deviation for each measurement."
## Step 3 requires that we include activity names for clarity, also included in subcombodata
## Step 4 requires appropriate labels.  The following are the labels in subcombodata:
##"xsource.file", "subject.number","activity","activity.name","feature","measurement","event.number" 

subcombodata=combodata[grep("mean|std",combodata$feature),]

## This code completes Step 5.
## "From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each
## activity and each subject"
## On a set of groupings from subcombodata on combinations of activity, subject and feature (feature is the "variable")
## Calculate the average measurement (this is the "value" in the table) for each grouping.

tidyoutput=summarize(group_by(subcombodata,activity.name,subject.number,feature), mean(measurement))

## Last step is to write the tidy output to a text file
## Step 5 continued:  "...Please upload the tidy data set created in step 5 of the instructions"

write.table(tidyoutput,file="~/Desktop/Coursera/UCI HAR Dataset/tidyoutput.txt",sep=",",row.name=FALSE)
