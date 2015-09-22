
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


hours.and.days<- data.frame(day = days,
							hour = hours)


