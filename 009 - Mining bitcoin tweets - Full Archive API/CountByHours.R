install.packages("httr")
install.packages("base64enc")
install.packages("RODBC")
library("httr")
library("base64enc")
library("RODBC")
source("helpFunctions.R")
Sys.setenv(TZ="UTC")

appname <- "CDTT"
key <- "xxx"
secret <- "xxx"

# base64 encoding
kands <- paste(key, secret, sep=":")
base64kands <- base64encode(charToRaw(kands))
base64kandsb <- paste("Basic", base64kands, sep=" ")

# request bearer token
# curl --verbose "https://api.twitter.com/oauth2/token" --header "Authorization: Basic $TOKEN" --header "Content-Type: application/x-www-form-urlencoded;charset=UTF-8" -d "grant_type=client_credentials"
resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                 add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                 body = "grant_type=client_credentials")

# get bearer token
bearer <- content(resToken)
bearerToken <- bearer[["access_token"]]
bearerTokenb <- paste("Bearer", bearerToken, sep=" ")

# get counts from archive
resCounts <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/LIVE/counts.json",
                  add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                  # body = "{\"query\": \"#bitcoin -\\\"RT @\\\"\",
                  #          \"bucket\": \"hour\", 
                  #          \"fromDate\": \"201801280000\",
                  #          \"toDate\": \"201801281043\" 
                  #        }")
                  body = list(query = "#bitcoin -is:retweet lang:en",
                              bucket = "minute",
                              fromDate = "201801280000",
                                toDate = "201808030000",
                              "next" = "eyJhdXRoZW50aWNpdHkiOiI0YjJjMTNjZDQzNjRkZGM4NzdjMDM2OGYyN2ZmNDZkMWFkNmQzMGM2M2JmNDJjMzM0YzM3MWViMDIzN2M5NzZmIiwiZnJvbURhdGUiOiIyMDE4MDEyODAwMDAiLCJ0b0RhdGUiOiIyMDE4MDgwMzAwMDAiLCJidWNrZXQiOiJkYXkiLCJuZXh0Ijp7Im1heERhdGUiOiIyMDE4MDEyOTAwMDAwMCIsImV4cGVuc2l2ZVF1ZXJ5IjpmYWxzZX19"
                              ), encode = "json")

content(resCounts, as = "text")
counts <- content(resCounts)
ctns <- counts[["results"]]
