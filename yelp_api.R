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
target = result_location[4,] #this is the National Aquarium
review_count= target["businesses.review_count"]
latitude = target[[23]][[8]][1]
longitude = target[[23]][[8]][2]


# crawling the webpage, we could be banned if Yelp is not happy with it
urll <- 'http://www.yelp.com/biz/national-aquarium-baltimore'
urll_2 <- 'http://www.yelp.com/biz/national-aquarium-baltimore?start=40'
urll_3 <- 'http://www.yelp.com/biz/national-aquarium-baltimore?start=80'
urll_4 <- 'http://www.yelp.com/biz/national-aquarium-baltimore?start=120'

library(XML)
#doc <- htmlParse(urll)
#doc <- htmlParse(urll_2)
doc <- htmlParse(urll_3)
#doc <- htmlParse(urll_4)
#saveXML(doc, file="aquarium_review_1.txt")
#saveXML(doc, file="aquarium_review_2.txt")
saveXML(doc, file="aquarium_review_3.txt")
#saveXML(doc, file="aquarium_review_4.txt")

# the time data is embedded in this tag
x = xpathSApply(doc,'//span[@class="rating-qualifier"]', xmlValue, encoding="UTF-8")
library(stringr)

# get the date information 
data_len = length(x)
regex = '[0-9]*/[0-9]*/[0-9]*\n'

times1 = rep(NA, 40)
times2 = rep(NA, 40)
times3 = rep(NA, 40)
times4 = rep(NA, 40)
times5 = rep(NA, 40)

for (i in 1:data_len) {
  yy = str_extract(x[i],regex)
  yy_2 = str_sub(yy, 1, str_length(yy)-1)
  #print(yy_2)
  times3[i] = as.Date(yy_2, format='%m/%d/%Y')
}
times3 = as.Date(times3, origin="1970-01-01")

