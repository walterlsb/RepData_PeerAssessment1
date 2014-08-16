## Load the data

# temp<-tempfile()
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
# data<-read.csv(unz(temp,"activity.csv"),colClasses=c("numeric", "Date", "numeric"))
# unlink(temp)
if(!file.exists("activity.zip")){
        print("download")
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="activity.zip")
}
data<-read.csv(unz("activity.zip","activity.csv"),colClasses=c("numeric", "Date", "numeric"))
## Pre processing



## mean total number of steps per day
## histogram
library(ggplot2)
stepscomplete<-data[!is.na(data$steps),]
stepsum<-aggregate(stepscomplete$steps, by=list(stepscomplete$date), FUN="sum")
names(stepsum)<-c("date","sumsteps")
##qplot(x=date,y=sumsteps,data=stepsum,stat="identity",geom="bar",,main="Total steps per day")


hist(stepsum$sumsteps,breaks=75)

## statistics
summary(stepsum)
nona.meansteps<-mean(stepsum$sumsteps)
nona.mediansteps<-median(stepsum$sumsteps)

# library(stringr)
by5min<-data
# by5min$min<-ifelse(nchar(by5min$interval)==1,paste("0",by5min$interval,sep=""),ifelse(nchar(by5min$interval)==2,by5min$interval,str_sub(by5min$interval,-2)))
# 
# by5min$hour<-ifelse(nchar(by5min$interval)<=2,"0",ifelse(nchar(by5min$interval)==3,str_sub(by5min$interval,1,1),str_sub(by5min$interval,1,2)))
# library(lubridate)
# 
# by5min$datestamp<-ymd_hms(paste(as.character(by5min$date),by5min$hour,by5min$min,"00",sep="-"))

by5minagg<-aggregate(by5min$steps,by=list(by5min$interval),FUN="mean",na.action=NULL, na.rm=TRUE)
names(by5minagg)<-c("interval","steps")
by5minagg<-cbind("intervalno"=1:nrow(by5minagg),by5minagg)
qplot(x=interval,y=steps,data=by5minagg,stat="identity",geom="bar",,main="Mean steps by interval throughout the day")

moststepinterval<-by5minagg[by5minagg$steps==max(by5minagg$steps),"interval"]


## imputing missing data



are.na<-sum(is.na(data$steps)==TRUE)

lookup.interval.mean<-function(this.interval){
        by5minagg[by5minagg$interval==this.interval,"steps"]        
}



idata<-data
##idata$steps4<-ifelse(is.na(idata$steps),lookup.interval.mean(idata$interval),idata$steps)

joined<-merge(idata,by5minagg,by.x="interval",by.y="interval", all=TRUE)
joined$steps<-ifelse(is.na(joined$steps.x),joined$steps.y,joined$steps.x)

imputeddata<-joined[,c("steps","date","interval")]

imputedstepsum<-aggregate(imputeddata$steps, by=list(imputeddata$date), FUN="sum")
names(imputedstepsum)<-c("date","sumsteps")

library(gridExtra)
plot.ignore.na<-ggplot(stepsum, aes(x=sumsteps))+geom_histogram(binwidth=1000)+labs(title="Mean steps by interval throughout the day - ignoring missing data")+labs(x="Daily total steps")
plot.impute.na<-ggplot(imputedstepsum, aes(x=sumsteps))+geom_histogram(binwidth=1000)+labs(title="Mean steps by interval throughout the day - replace missing data with interval mean")+labs(x="Daily total steps")


grid.arrange(plot.ignore.na,plot.impute.na,ncol=1)


       



impna.meansteps<-mean(stepsum$sumsteps)
impna.mediansteps<-median(stepsum$sumsteps)




## weekend/weekday
require(lubridate)
imputeddata$weekend<-ifelse(wday(imputeddata$date) %in% c(1,7),"weekend","weekday")



imputedby5minagg<-aggregate(imputeddata$steps,by=list(imputeddata$interval,imputeddata$weekend),FUN="mean",na.action=NULL, na.rm=TRUE)
names(imputedby5minagg)<-c("interval","weekend","steps")
imputedby5minagg$weekend<-factor(imputedby5minagg$weekend)

qplot(interval,steps,data=imputedby5minagg,facets=weekend ~ ., geom="line")
