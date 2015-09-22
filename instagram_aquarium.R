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
