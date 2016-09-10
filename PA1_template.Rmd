###My Project On Activity Monitor Data
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. 
Now we will investigate the these activity data to find if there are any interesting patterns hidden in it.
**First investigation:What is mean total number of steps taken per day?

```{r,echo=TRUE}
#initiate related packages
library(dplyr)
library(tidyr)
library(ggplot2)
#load data into R
mydata<-read.csv(file="C:/Users/siteng/Documents/data/activity.csv")
#set date varible in the date format
mydata$date<-as.Date(as.character(mydata$date),"%Y-%m-%d")
##What is mean total number of steps taken per day?
mydata2<-mydata%>%group_by(date)%>%summarise(daysum=sum(steps))
with(mydata2,plot(date,daysum))
summary(mydata2$daysum)
```
From the plot it is easy to see the mean total number of steps taken per day is up and down from about 0 to 20000 steps. The mean of mean and median total number of steps taken per day is 10770 and 10760.
#The second investigation:What is the average daily activity pattern?
```{r,echo=TRUE}
mydata3<-mydata%>%group_by(interval)%>%summarise(intervalmean=mean(steps,na.rm=TRUE))
with(mydata3,plot(interval,intervalmean))
which.max(mydata3$intervalmean)
mydata3$interval[104]
```
From the time series plot we can find out the activity pattern about this person. The maximum steps is at 835minutes, in other words, around 2 pm.
**The third investigation:Imputing missing values
```{r}
sum(is.na(mydata$steps))
mydata4<-mydata
for(i in 1:length(mydata4$steps)){
        if(is.na(mydata4$steps[i]))
                mydata4$revisedsteps[i]<-as.numeric(filter(mydata3,interval==mydata4$interval[i])[2])
        else
                mydata4$revisedsteps[i]<-mydata4$steps[i]
}
mydata5<-mydata4%>%group_by(date)%>%summarise(daysum=sum(revisedsteps))
with(mydata5,plot(date,daysum))
summary(mydata5$daysum)
```
We can see the mean and median total number of steps taken per day are similar of the estimates from the first part of the assignment. 
**Are there differences in activity patterns between weekdays and weekends?
```{r}
mydata6<-mutate(mydata4,weekday=character(17568))    
for(i in 1:length(mydata6$steps)){
        if(weekdays(mydata6$date[i])%in%c("ÐÇÆÚÁù","ÐÇÆÚÈÕ")) 
        mydata6$weekday[i]<-"weekend"
else 
        mydata6$weekday[i]<-"weekday"
}   
mydata6$weekday<-as.factor(mydata6$weekday)
mydata7<-mydata6%>%group_by(weekday,interval)%>%summarise(intervalmean=mean(revisedsteps))
g<-ggplot(data=mydata7,aes(interval,intervalmean))
g+geom_line()+facet_grid(weekday~.)
```

From the plot above we can see there is a slightly different shift over weekdays and weekends activity. It is a resonable guess the shift is due to the get up time difference between weekdays and weekends.
