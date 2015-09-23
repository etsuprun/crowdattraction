library(shiny)
require(data.table)
require(plyr)
library(ggplot2)
library(lubridate)
library(colorRamps)
library(RCurl)
library(curl)
library(gtable)
library(gridExtra)
instagram<-
  data.table(
    read.csv(
      curl("https://raw.githubusercontent.com/etsuprun/crowdattraction/master/instagram.txt"),
      sep=" ")
  )
historicalData<-function(startDate,endDate) {
  
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

# "Data from 2014-09-20 to 2015-09-21"
plotheat = function(product3){
  cols = c("#000000", blue2yellow(1000))
  par(xpd=NA)
  mar=c(0,0,0,0)
  plot(c(-2, 12), c(-2, 12), xaxt="n", yaxt="n", type="n", bty="n", ylab="", xlab="", main = "Activity Heat Map")
  for(i in 1:11){rect(xleft=0:6, xright=1:7, ytop=11-i+1, ybot=11-i, col=cols[product3[i,]+1])}
  text(x=-1, y=1:11-0.5, pos=2,labels=c(seq(7, 1, by=-1), seq(12, 9, by=-1)))
  text(x=0:6+0.5, y=-1, labels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
  text(x=-1.5, y=11, pos=3, labels="Hour \nof \nDay")
  
  
  rect(xleft=8.5, xright=9.5, ybot=0:10, ytop=1:11, col=cols[round(seq(0, 1, length.out=11), 3)*1000+1])
  text(x=11, y=1:11-0.5, pos=2,labels=seq(0, 1, length.out=11))
  text(x=3.5, y = -2.5, pos=3, labels = "Day of the Week")
  text(x=9, y=-3, pos=3, labels = "Heatmap \nIntensity \nPercentage") 
}

GetData <- function(){
  
  library(RCurl)
  
  text.file <- getURL("https://raw.githubusercontent.com/etsuprun/crowdattraction/master/instagram.txt")
  
  instagram.data <- read.table(text = text.file,
                               header=TRUE,
                               sep= "",
                               stringsAsFactors=FALSE)
  
  
  # Mung --------------------------------------------------------------
  
  # Number of posts per day of the week
  # table(instagram.data$wday)
  
  hours <- sapply(instagram.data$date,function(insta.date){
    
    # Pull time out from date
    time <- strsplit(insta.date," ")[[1]][2]
    
    # Pull hour out from time 
    hour <- as.numeric(strsplit(time, ":")[[1]][1])
    
  })
  
  
  days <- instagram.data$wday
  
  
  hours.and.days <- data.frame(day = days,
                               hour = hours,
                               stringsAsFactors = FALSE)
  
  
  return(hours.and.days)
  
  
}

hours.and.days <- GetData()

PlotDay <-function(day){
  
  # Number of Instagrams during Friday opening hours (9 am to 8 pm)
  num.Fri.hours <- dim( hours.and.days[hours.and.days$day == "Fri" 
                                       & hours.and.days$hour >= 9
                                       & hours.and.days$hour <= 20,] )[1]
  
  
  # Number of Instagrams during Friday opening hours (9 am to 5 pm)
  
  num.notFri.hours <- dim(hours.and.days[hours.and.days$day != "Fri" 
                                         & hours.and.days$hour >= 9
                                         & hours.and.days$hour <= 17,] )[1]
  
  
  totalNum.openingHours <- num.Fri.hours + num.notFri.hours
  
  
  if (day != 'Fri'){
    
    counts <- table( hours.and.days[hours.and.days$day == day 
                                    & hours.and.days$hour >= 9
                                    & hours.and.days$hour <= 17,] )
    
    
    averages <- counts[1:length(counts)]/totalNum.openingHours 
    
    averages <- averages *100
    
    df <- data.frame(x=factor(c("9","10","11","12","13",
                                "14","15","16","17"),levels=c("9","10","11","12","13",
                                                              "14","15","16","17")), y=averages)
    
    p <- ggplot(df,aes(x=x,y=y,group=1)) + geom_bar(stat="identity",fill="blue") + scale_y_continuous(name="Percentage of Vistors",limits=c(0,5)) + 
      geom_line(color="purple") + xlab(paste0("Hour of ",day)) + ggtitle("Visitors Percentage by Time of Day")
    p <- p + theme(plot.title = element_text(size = 20,face="bold")) + 
      theme(axis.title = element_text(size=20,face="bold"))
    
    return(p)
    # plot(averages,main=day,ylim=c(0,5),xaxt='n',ylab='Percentage of Vistors')
    # 
    # axis(side = 1, at = c(1:9), labels= c("9","10","11","12","13",
    #                                       "14","15","16","17"))
    
    
    
  }
  
  if (day == 'Fri'){
    
    
    counts <- table( hours.and.days[hours.and.days$day == 'Fri'
                                    & hours.and.days$hour >= 9
                                    & hours.and.days$hour <= 20,] )
    
    
    averages <- counts[1:length(counts)]/totalNum.openingHours 
    
    averages <- averages *100
    df <- data.frame(x=factor(c("9","10","11","12","13",
                                "14","15","16","17","18","19","20"),levels=c("9","10","11","12","13",
                                                                             "14","15","16","17","18","19","20")), y=averages)
    
    p <- ggplot(df,aes(x=x,y=y,group=1)) + geom_bar(stat="identity",fill="blue") + scale_y_continuous(name="Percentage of Vistors",limits=c(0,5)) + 
      geom_line(color="purple") + xlab(paste("Hour of ",day)) + ggtitle("Visitors Percentage by Time of Day")
    p <- p + theme(plot.title = element_text(size = 20,face="bold")) + 
      theme(axis.title = element_text(size=20,face="bold"))
    
    return(p)
    # plot(averages, main='Fri',ylim=c(0,5),xant='n',ylab='Percentage of Vistors')
    # 
    # axis(side = 1, at = c(1:12), labels= c("9","10","11","12","13",
    #                                        "14","15","16","17","18","19","20"))
    
    
  }
  
  
}

PlotHour <- function (hour){
  
  # Number of Instagrams during Friday opening hours (9 am to 8 pm)
  num.Fri.hours <- dim( hours.and.days[hours.and.days$day == "Fri" 
                                       & hours.and.days$hour >= 9
                                       & hours.and.days$hour <= 20,] )[1]
  
  
  # Number of Instagrams during Friday opening hours (9 am to 5 pm)
  
  num.notFri.hours <- dim(hours.and.days[hours.and.days$day != "Fri" 
                                         & hours.and.days$hour >= 9
                                         & hours.and.days$hour <= 17,] )[1]
  
  
  totalNum.openingHours <- num.Fri.hours + num.notFri.hours
  
  # ------------------------------------------------------------------
  
  
  counts <- table( hours.and.days[hours.and.days$hour == hour,] )
  date.order <- c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
  counts <- as.numeric(sapply(date.order, function(a){counts[which(rownames(counts)==a)]}))
  
  averages <- counts[1:length(counts)]/totalNum.openingHours 
  averages <- averages *100
  df <- data.frame(x=factor(c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun"),
                            levels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")), y=averages)
  p <- ggplot(df,aes(x=x,y=y,group=1)) + geom_bar(stat="identity",fill="blue") + scale_y_continuous(name="Percentage of Vistors",limits=c(0,5)) + 
    geom_line(color="purple") + xlab(paste0("Day of the Week at ",paste0(ifelse(hour<12,paste(hour,"AM"),paste(hour-12,"PM"))))) + 
    ggtitle("Visitors Percentage by Day of Week")
  p <- p + theme(plot.title = element_text(size = 20,face="bold")) + 
    theme(axis.title = element_text(size=20,face="bold"))
  return(p)
  # plot(averages,main=hour,ylim=c(0,5),xaxt='n',ylab='Percentage of Vistors')
  # 
  # axis(side = 1, at = c(1:7), labels= c("Fri","Mon","Sat","Sun","Thurs", "Tues",'Wed'))
  
  
}


# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  load("product3.rda")
  output$heatmap <- renderPlot({
    plotheat(product3)
  })
  
  output$histmap <- renderPlot({
    historicalData(input$dateRange[1],input$dateRange[2])
  })
  
  output$plotday <- renderPlot({
    day.num <- wday(input$date)
    day.index <- ifelse(day.num==1,7,day.num-1)
    date.order <- c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
    day.input <- date.order[day.index]
    PlotDay(day.input)
    
  })
  
  output$plothour <- renderPlot({
    hour.num <- as.numeric(gsub("[^0-9]","",input$hour))
    hour.num <- ifelse(hour.num>=9,hour.num,hour.num+12)
    PlotHour(hour.num)
  })
  
  output$combine <- renderPlot({
    par(mfrow=c(2,2))
    hour.num <- as.numeric(gsub("[^0-9]","",input$hour))
    hour.num <- ifelse(hour.num>=9,hour.num,hour.num+12) 
    day.num <- wday(input$date)
    day.index <- ifelse(day.num==1,7,day.num-1)
    date.order <- c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
    day.input <- date.order[day.index]
    gA <- ggplotGrob(PlotHour(hour.num))
    gB <- ggplotGrob(PlotDay(day.input))
    g <- grid.arrange(gB, gA, ncol = 2)
    grid.newpage()
    grid.draw(g)
  })
  
})
