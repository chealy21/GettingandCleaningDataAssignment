run_analysis<-function(){
        require(dplyr)
        require(reshape2)
        
        #download zip file
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "UCI HAR Dataset.zip")
        
        #unzip file
        unzip("UCI HAR Dataset.zip")
        
        #read files
        activitylabels<-read.table("./UCI HAR Dataset/activity_labels.txt")
        featurelabels<-read.table("./UCI HAR Dataset/features.txt")
        testsubjects<-read.table("./UCI HAR Dataset/test/subject_test.txt")
        testdata<-read.table("./UCI HAR Dataset/test/X_test.txt")
        testactivity<-read.table("./UCI HAR Dataset/test/y_test.txt")
        trainsubjects<-read.table("./UCI HAR Dataset/train/subject_train.txt")
        traindata<-read.table("./UCI HAR Dataset/train/X_train.txt")
        trainactivity<-read.table("./UCI HAR Dataset/train/y_train.txt")

        
        # rename variable labels
        featurelabels<-featurelabels %>% mutate(V2=gsub("tBodyAcc","timebodyacceleration",V2)) %>% mutate(V2=gsub("\\()","",V2)) %>% mutate(V2=gsub("tGravityAcc","timegravityacceleration",V2))%>% mutate(V2=gsub("tBodyGyro","timebodygyroscope",V2)) %>% mutate(V2=gsub("fBodyAcc","frequencybodyacceleration",V2))
        featurelabels<-mutate(featurelabels,V2=tolower(V2))
        
        #apply features as column names
        names(testdata)<-featurelabels[,2]
        names(traindata)<-featurelabels[,2]
        
        #label subject and activity
        names(testsubjects)<-"subject"
        names(testactivity)<-"activity"
        
        names(trainsubjects)<-"subject"
        names(trainactivity)<-"activity"
        
        #add activity description
        names(activitylabels)<-c("activity","descriptionofactivity")
        
        #update activity to remove underscores
        activitylabels<-mutate(activitylabels,description=gsub("_"," ",description))
        
        #join activities
        testactivity<-left_join(testactivity,activitylabels,by="activity")
        trainactivity<-left_join(trainactivity,activitylabels,by="activity")
        
        #subset mean and std
        summarytestdata<-testdata[,grep("mean|std",colnames(testdata))]
        summarytraindata<-traindata[,grep("mean|std",colnames(traindata))]
        
        #create test dataframe
        testdf<-cbind(testsubjects,testactivity,summarytestdata)
        
        #create train dataframe
        traindf<-cbind(trainsubjects,trainactivity,summarytraindata)
        
        #rbind test and train
        summarydata<-rbind(testdf,traindf)
        
        #remove extra column
        drops<-"activity"
        summarydata<-summarydata[,!(names(summarydata) %in% drops)]
        
        #melt
        summarymelt<-melt(summarydata,id=c("subject","descriptionofactivity"))
        
        #dcast to get mean by activity and subject
        summarytable<-dcast(summarymelt,subject+descriptionofactivity~variable,mean)
        
        #write table
        write.table(summarytable,file="summarytable.txt", sep=" ",col.names=TRUE)
        
}