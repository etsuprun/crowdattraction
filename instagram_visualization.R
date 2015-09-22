require(data.table)
require(plyr)
require(ggplot2)

instagram<-
  data.table(
    read.csv(
      "https://raw.githubusercontent.com/etsuprun/crowdattraction/master/instagram.txt",
      sep=" ")
  )

daterange<-paste("Data from",min(as.Date(instagram$date)), "to",max(as.Date(instagram$date)))


instagram.byDaybyHour<-
  aggregate(cbind(count = date) ~ as.POSIXlt(instagram$date)$wday+hour, 
            data = instagram, 
            FUN = function(x){NROW(x)})
colnames(instagram.byDaybyHour)<-c("day.of.week","hour","count")


dayHourPlot<-qplot(hour,count,data=instagram.byDaybyHour,
              color=as.factor(day.of.week),
              main=paste("Total Number of Instagram Posts by Day of Week\nBaltimore National Aquarium\n\n",daterange,"\n"),
              xlab="Hour (from 0 to 23)")
dayHourPlot<-dayHourPlot+geom_point()+stat_smooth(se=F) +
  scale_color_manual("Legend Title\n",labels = c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                     values=c("red", "orange","green","blue","purple","black","magenta"))

instagram.byDate<-
  aggregate(cbind(count = date) ~ as.Date(date), 
            data = instagram, 
            FUN = function(x){NROW(x)})
colnames(instagram.byDate)<-c("date","count")

dayPlot<-qplot(date,count,data=instagram.byDate,
                   main=paste("Total Number of Instagram Posts by Date\nBaltimore National Aquarium\n\n",daterange,"\n"),
                   xlab="Date")+geom_smooth()

instagram.byMonth<-
  aggregate(cbind(count = date) ~ month(date), 
            data = instagram, 
            FUN = function(x){NROW(x)})
colnames(instagram.byMonth)<-c("month","count")

monthPlot<-qplot(month,count,data=instagram.byMonth,
               main=paste("Total Number of Instagram Posts by Month\nBaltimore National Aquarium\n\n",daterange,"\n"),
               xlab="Month")



instagram.byWeek<-
  aggregate(cbind(count = date) ~ week(date), 
            data = instagram, 
            FUN = function(x){NROW(x)})
colnames(instagram.byWeek)<-c("week","count")

weekPlot<-qplot(week,count,data=instagram.byWeek,
                 main=paste("Total Number of Instagram Posts by Week\nBaltimore National Aquarium\n\n",daterange,"\n"),
                 xlab="Week")

