
# -------------------------------------------------------------
# Sahil Shah <sahil.shah@u.northwestern.edu>
# Tues, 9/22/15
# -------------------------------------------------------------

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



PlotDay <-function (day){

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

		plot(averages,main=day,ylim=c(0,5),xaxt='n',ylab='Percentage of Vistors')

		axis(side = 1, at = c(1:9), labels= c("9","10","11","12","13",
									"14","15","16","17"))



	}

	if (day == 'Fri'){


		counts <- table( hours.and.days[hours.and.days$day == 'Fri'
			   					& hours.and.days$hour >= 9
			   					& hours.and.days$hour <= 20,] )


		averages <- counts[1:length(counts)]/totalNum.openingHours 

		averages <- averages *100

		plot(averages, main='Fri',ylim=c(0,5),xant='n',ylab='Percentage of Vistors')

		axis(side = 1, at = c(1:12), labels= c("9","10","11","12","13",
									"14","15","16","17","18","19","20"))


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


	averages <- counts[1:length(counts)]/totalNum.openingHours 

	averages <- averages *100


	plot(averages,main=hour,ylim=c(0,5),xaxt='n',ylab='Percentage of Vistors')

	axis(side = 1, at = c(1:7), labels= c("Fri","Mon","Sat","Sun","Thurs", "Tues",'Wed'))


}


# Plot -------------------------------------------------------------------

par(mfrow=c(2,6))

for (hour in 9:20){


	PlotHour(hour)

}









