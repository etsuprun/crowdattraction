rm(list=ls())
require("httr")
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)

app_name <- "crowding"
client_id <- "8b2b210b5604465e8d1b071b35a67a88"
client_secret <- "03820818a6b5475783b754e618f0e8c6"
scope = "basic"

instagram <- oauth_endpoint(authorize = "https://api.instagram.com/oauth/authorize", access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]

### Data gather
wts_info = fromJSON(getURL("https://api.instagram.com/v1/locations/83598/media/recent?access_token=1023095736.1fb234f.e9586b1d6ed547e4991098e390bfc7d2&min_timestamp=1411257600&max_timestamp=1423957020"), unexpected.escape="keep")

df = data.frame(no = 1:length(wts_info$data))
for(i in 1:length(wts_info$data))
{
      #comments
      df$comments[i] <-wts_info$data[[i]]$comments$count
      
      #likes:
      df$likes[i] <- wts_info$data[[i]]$likes$count
      
      #date
      df$date[i] <- toString(as.POSIXct(as.numeric(wts_info$data[[i]]$created_time), origin="1970-01-01"))
}
overall = df
url = wts_info$pagination[1]

while(length(url)>0){
      print(url); flush.console()
      wts = fromJSON(getURL(url), unexpected.escape="keep")
      df = data.frame(no = 1:length(wts$data))
      for(i in 1:length(wts$data))
      {
            #comments
            df$comments[i] <-wts$data[[i]]$comments$count
            
            #likes:
            df$likes[i] <- wts$data[[i]]$likes$count
            
            #date
            df$date[i] <- toString(as.POSIXct(as.numeric(wts$data[[i]]$created_time), origin="1970-01-01"))
      }
      overall = rbind(overall, df)
      url = wts$pagination[1]
}

### Data process
overall$hour = hour(overall$date)
overall$minute = minute(overall$date)
overall$wday = wday(overall$date, label=T)
write.table(overall, file="crowdattraction/instagram.txt")

overall_open = overall[which((overall$hour<=16 & overall$hour>=9)==1),]
overall_open$quater = paste(overall_open$hour, floor(overall_open$minute/15), sep="-")
overall_open$d = unlist(str_split(overall_open$date, " "))[seq(1, 2*dim(overall_open)[1], by=2)]
overall_open$unique_id = paste(overall_open$d, overall_open$quater, sep="-")

coarsened = by(rep(1, dim(overall_open)[1]), overall_open$unique_id, sum)
coarse = data.frame(id = names(coarsened), count = as.numeric(coarsened))

calendar = seq(from=as.Date("2014/09/21"), to=as.Date("2015/09/21"), by="day")
scaffold = data.frame(date = rep(calendar, each=32), hours = rep(9:16, each=4), quater = rep(0:3, 8))
scaffold$id = paste(scaffold$date, scaffold$hours, scaffold$quater, sep="-")

product = merge(scaffold, coarse, by="id", all=T, sort=F)
product$count[is.na(product$count)] = 0
product$wday = wday(product$date, lab=T)
product$q = paste(product$hours, product$quater, sep="-")
product$qwday = paste(product$wday, product$q, sep="-")

test = by(product$count, product$qwday, mean)
test = data.frame(id = names(test), m = as.numeric(test))

product2 = data.frame(wday = rep(c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"), each=32), hours = rep(rep(9:16, each=4), 7), quater = rep(rep(0:3, 8), 7))
product2$id = paste(product2$wday, product2$hours, product2$quater, sep="-")
product2 = merge(product2, test, by="id", all=T, sort=F)

####
overall_open$id = paste(overall_open$wday, overall_open$hour, sep="-")
m = by(rep(1, dim(overall_open)[1]), overall_open$id, sum)
m = data.frame(id = names(m), m=as.numeric(m))
product = data.frame(wday = rep(c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"), each=8), hours = rep(9:16))
product$id = paste(product$wday, product$hours, sep="-")    
product = merge(product, m, by="id", sort=F)


product2 = matrix(product$m, nrow=8, ncol=7, byrow=F)
colnames(product2) = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
rownames(product2) = c(9:12, 1:4)
                     
friday_late = overall[overall$wday=="Fri" & overall$hour>=17 & overall$hour<=19,]
friday_late$id = paste(friday_late$wday, friday_late$hour)
friday_late = by(rep(1, dim(friday_late)[1]), friday_late$id, sum)

product2 = rbind(product2, 0, 0, 0)
product2[9:11,6] = friday_late
rownames(product2)[9:11] = c(5, 6, 7)

calendar = seq(from=as.Date("2014/09/21"), to=as.Date("2015/09/21"), by="day")
calendar = data.frame(date = calendar, wday = wday(calendar, label=T))
normalize = by(rep(1, dim(calendar)[1]), calendar$wday, sum)

product3 = t(t(product2)/as.numeric(normalize))
product3 = round(product3/max(product3), 3)*1000

plotHeat = function(product3){
      cols = c("#000000", blue2yellow(1000))
      par(xpd=NA)
      mar=c(0,0,0,0)
      plot(c(0, 10), c(0, 11), xaxt="n", yaxt="n", type="n", bty="n", ylab="", xlab="", main = "Activity Heat Map")
      for(i in 1:11){rect(xleft=0:6, xright=1:7, ytop=11-i+1, ybot=11-i, col=cols[product3[i,]+1])}
      text(x=-1, y=1:11-0.5, pos=2,labels=c(seq(7, 1, by=-1), seq(12, 9, by=-1)))
      text(x=0:6+0.5, y=-1, labels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
      
      rect(xleft=8.5, xright=9.5, ybot=0:10, ytop=1:11, col=cols[round(seq(0, 1, length.out=11), 3)*1000+1])
      text(x=11, y=1:11-0.5, pos=2,labels=seq(0, 1, length.out=11))
      text(x=3.5, y = -2.5, pos=3, labels = "Day of the Week")
      text(x=9, y=-2.5, pos=3, labels = "Heatmap \nIntensity \nPercentage")  
}


