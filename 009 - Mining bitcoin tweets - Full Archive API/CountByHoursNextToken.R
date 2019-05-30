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

# final days minute:
#  2018-02-01 done
#  2018-02-06 done
#  2018-02-14 done
#  2018-03-15 done

nexttoken <- NULL
allcounts <- list()
from <- "201803150000"
to <-   "201803160000"
b <-    "minute"
q <-    "#bitcoin -is:retweet lang:en"

repeat{
  if(!is.null(nexttoken)){
    resCounts <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/LIVE/counts.json",
                      add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                      body = list(query = q, bucket = b, fromDate = from, toDate = to,"next" = nexttoken
                      ), encode = "json")
  }else{
    resCounts <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/LIVE/counts.json",
                      add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                      body = list(query = q,bucket = b, fromDate = from, toDate = to), encode = "json")  
  }
  
  #content(resCounts, as = "text")
  resobj <- content(resCounts)
  counts <- resobj[["results"]]
  allcounts <- append(allcounts, counts)
  nexttoken <- resobj[["next"]]
  
  print("sleeping for 10 secs")
  Sys.sleep(10)
  
  if(is.null(nexttoken)){
    break
  }
}
