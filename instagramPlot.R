require(ggplot2)

# This function takes in a start and end date and returns a plot of Instagram posts by day and by hour.

historicalData<-function(startDate,endDate) {

  instagram<-
    data.table(
      read.csv(
        "https://raw.githubusercontent.com/etsuprun/crowdattraction/master/instagram.txt",
        sep=" ")
    )
  instagram<-instagram[as.Date(instagram$date)<=endDate&as.Date(instagram$date)>=startDate]
  instagram.byDaybyHour<-
    aggregate(cbind(count = date) ~ as.POSIXlt(instagram$date)$wday+hour, 
              data = instagram, 
              FUN = function(x){NROW(x)})
  colnames(instagram.byDaybyHour)<-c("day.of.week","hour","count")
  
  daterange<-paste("Data from",min(as.Date(instagram$date)), "to",max(as.Date(instagram$date)))
  dayHourPlot<-qplot(hour,count,data=instagram.byDaybyHour,
                     color=as.factor(day.of.week),
                     main=paste("Total Number of Instagram Posts by Day of Week\nBaltimore National Aquarium\n\n",daterange,"\n"),
                     xlab="Hour (from 0 to 23)")
  dayHourPlot<-dayHourPlot+geom_point()+stat_smooth(se=F) +
    scale_color_manual("Legend Title\n",labels = c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                       values=c("red", "orange","green","blue","purple","black","magenta"))
  
  return(dayHourPlot)
}
