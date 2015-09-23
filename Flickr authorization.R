# Get authentication from flickr 

library(httr)

api_key = "YOUR_API_KEY"
secret = "YOUR_SECRET"

flickr.app <- oauth_app("Prediction",api_key,secret)
flickr.endpoint <- oauth_endpoint(
  request = "https://www.flickr.com/services/oauth/request_token"
  , authorize = "https://www.flickr.com/services/oauth/authorize"
  , access = "https://www.flickr.com/services/oauth/access_token"
)
tok <- oauth1.0_token(
  flickr.endpoint
  , flickr.app
  , cache = F
)