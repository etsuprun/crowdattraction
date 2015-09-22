require(data.table)
require(plyr)

instagram<-
  data.table(
    read.csv(
      "https://raw.githubusercontent.com/etsuprun/crowdattraction/master/instagram.txt",
      sep=" ")
  )

daterange<-paste("Data from",min(as.Date(instagram$date)), "to",max(as.Date(instagram$date)))


instagram.summary<-
  aggregate(cbind(count = date) ~ wday+hour, 
            data = instagram, 
            FUN = function(x){NROW(x)})
myplot<-qplot(hour,count,data=instagram.summary,
              color=wday,
              main=paste("Total Number of Instagram Posts by Day of Week\nBaltimore National Aquarium\n\n",daterange,"\n"),
              xlab="Hour (from 0 to 23)")
myplot<-myplot+geom_point()+stat_smooth(se=F)
print(myplot)
