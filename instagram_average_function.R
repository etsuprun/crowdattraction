# -------------------------------------------------------------
# Sahil Shah <sahil.shah@u.northwestern.edu>
# Tues, 9/22/15
# -------------------------------------------------------------

# Get --------------------------------------------------------------

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


# Function ------------------------------------------------------------

CalcAverageVisitors <- function(day,hour){
  # Calculate the average number of visitors 
  #
  # Args:
  #   day: string ('Sat','Sun','Mon','Tues','Wed','Thurs','Fri')
  #	  hour: numeric 24 horus
  # Returns:
  #   The percentage of visitors 

	  # Number of Instagrams during Friday opening hours (9 am to 8 pm)
	num.Fri.hours <- dim( hours.and.days[hours.and.days$day == "Fri" 
				   					& hours.and.days$hour >= 9
				   					& hours.and.days$hour <= 20,] )[1]


	# Number of Instagrams during Friday opening hours (9 am to 5 pm)

	num.notFri.hours <- dim(hours.and.days[hours.and.days$day != "Fri" 
				   					& hours.and.days$hour >= 9
				   					& hours.and.days$hour <= 17,] )[1]


	totalNum.openingHours <- num.Fri.hours + num.notFri.hours

	

	counts <- table( hours.and.days[hours.and.days$day == day 
			   					& hours.and.days$hour == hour ,] )


	averages <- counts[1]/totalNum.openingHours 

	averages <- averages *100

} 

