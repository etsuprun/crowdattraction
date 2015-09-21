consumerKey = 'b-QAUged3VgxDWW_ZTpmUQ'
consumerSecret	= 'f9jH4UbngxMDLYI-m-wYtuFpo-I'
token	= 'QTi4YkpsnVHk22CSYFOTvaVPpYW2bH33'
token_secret =	'-FA53vHGue_spUII5leac3Vbo5s'
library(httr)
library(httpuv)
library(jsonlite)
# authorization
myapp = oauth_app("YELP", key=consumerKey, secret=consumerSecret)
sig=sign_oauth1.0(myapp, token=token,token_secret=token_secret)

term = 'museums'
id = 'national-aquarium'
limit = 5
yelpurl <- paste0("http://api.yelp.com/v2/search?term=",term,"&location=Baltimore")

locationdata=GET(yelpurl, sig)
locationdataContent = content(locationdata)
locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
head(data.frame(locationdataList))
result_location = data.frame(locationdataList)
target = result_location[4,]

urll <- 'http://www.yelp.com/biz/national-aquarium-baltimore'
library(XML)
doc <- htmlParse(urll)
x = xpathSApply(doc,'//span[@class="rating-qualifier"]', xmlValue, encoding="UTF-8")
library(stringr)

data_len = length(x)
regex = '[0-9]*/[0-9]*/[0-9]*\n'

for (i in 1:data_len) {
  yy = str_extract(x[i],regex)
  yy_2 = str_sub(yy, 1, str_length(yy)-1)
  yy_2
}

