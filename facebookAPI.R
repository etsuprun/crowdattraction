app_id = '1234'
app_secret = '4567'
install.packages("Rfacebook")
install.packages("devtools")
install.packages("curl")
library(devtools)
install_github("pablobarbera/Rfacebook/Rfacebook")
require("Rfacebook")
fb_oauth <- fbOAuth(app_id, app_secret, extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
#load("fb_oauth")
token='xxxx'
me <- getUsers("me",token)
#depricated searchFacebook()
posts <- searchFacebook(string = "National Aquarium", token, n = 500, 
                        since = "9 september 2015 00:00", until = "10 september 2015 23:59")
page <- getPage("NationalAquarium", token, n = 5000)
page[which.max(page$likes_count), ]
