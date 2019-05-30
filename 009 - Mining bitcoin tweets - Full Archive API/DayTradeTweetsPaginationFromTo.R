options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages("httr")
install.packages("base64enc")
install.packages("RODBC")
library("httr")
library("base64enc")
library("RODBC")
source("helpFunctions.R")

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

#candidates -> see also TradingView

### 2018-02-01	27307
### 2018-02-06	30779
### 2018-02-14	21599
# 2018-03-15	21648

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=CryptoCurrTweets;trusted_connection=true')

alltweets <- list()
fromDate <- as.POSIXct("Thu Mar 15 00:00:00 +0000 2018", format="%a %b %d %H:%M:%S %z %Y")
toDate <- as.POSIXct("Thu Mar 15 00:15:00 +0000 2018", format="%a %b %d %H:%M:%S %z %Y")
print(fromDate)
print(toDate)
i <- 0
for (i in 1:96) {
  # get from full archive
  res <- POST(url = "https://api.twitter.com/1.1/tweets/search/fullarchive/LIVE.json",
                    add_headers("authorization" = bearerTokenb, "content-Type" = "application/json"),
                    body = list(query = "#bitcoin -is:retweet lang:en",
                                maxResults = 500,
                                fromDate = format(as.POSIXct(fromDate,format="%a %b %d %H:%M:%S %z %Y"),"%Y%m%d%H%M"),
                                toDate =   format(as.POSIXct(toDate,format="%a %b %d %H:%M:%S %z %Y"),"%Y%m%d%H%M")), encode = "json")

  #content(resTweets, as = "text")
  resobj <- content(res)
  tweetsList <- resobj[["results"]]
  alltweets <- append(alltweets, tweetsList)
  
  print(fromDate)
  print(toDate)
  print("---")
  Sys.sleep(3)
  
  fromDate <- toDate
  toDate <- toDate + 15*60
} 


#save to SQL Server
for(tweet in alltweets)
{
  #store current user profile
  usr <- tweet[["user"]]
  user <- data.frame("id" = em(usr[["id"]]),
                     "id_str" = em(usr[["id_str"]]),
                     "name" = em(usr[["name"]]),
                     "screen_name" = em(usr[["screen_name"]]),
                     "location" = em(usr[["location"]]),
                     "url" = em(usr[["url"]]),
                     "description" = em(usr[["description"]]),
                     "translator_type" = em(usr[["translator_type"]]),
                     "protected" = em(usr[["protected"]]),
                     "verified" = em(usr[["verified"]]),
                     "followers_count" = em(usr[["followers_count"]]),
                     "friends_count" = em(usr[["friends_count"]]),
                     "listed_count" = em(usr[["listed_count"]]),
                     "favourites_count" = em(usr[["favourites_count"]]),
                     "statuses_count" = em(usr[["statuses_count"]]),
                     "created_at" = fd(usr[["created_at"]]),
                     "utc_offset" = em(usr[["utc_offset"]]),
                     "time_zone" = em(usr[["time_zone"]]),
                     "geo_enabled" = em(usr[["geo_enabled"]]),
                     "lang" = em(usr[["lang"]]),
                     "contributors_enabled" = em(usr[["contributors_enabled"]]),
                     "is_translator" = em(usr[["is_translator"]]),
                     "profile_background_color" = em(usr[["profile_background_color"]]),
                     "profile_background_image_url" = em(usr[["profile_background_image_url"]]),
                     "profile_background_image_url_https" = em(usr[["profile_background_image_url_https"]]),
                     "profile_background_tile" = em(usr[["profile_background_tile"]]),
                     "profile_link_color" = em(usr[["profile_link_color"]]),
                     "profile_sidebar_border_color" = em(usr[["profile_sidebar_border_color"]]),
                     "profile_sidebar_fill_color" = em(usr[["profile_sidebar_fill_color"]]),
                     "profile_text_color" = em(usr[["profile_text_color"]]),
                     "profile_use_background_image" = em(usr[["profile_use_background_image"]]),
                     "profile_image_url" = em(usr[["profile_image_url"]]),
                     "profile_image_url_https" = em(usr[["profile_image_url_https"]]),
                     "profile_banner_url" = em(usr[["profile_banner_url"]]),
                     "default_profile" = em(usr[["default_profile"]]),
                     "default_profile_image" = em(usr[["default_profile_image"]])
                     ,stringsAsFactors = FALSE
  )
  sqlSave(conn, user, tablename = "UsersIntr", append = TRUE, rownames = FALSE)
  
  #store tweet data
  twt <- data.frame("created_at" = fd(tweet[["created_at"]]),
                    "id" = em(tweet[["id"]]),
                    "id_str" = em(tweet[["id_str"]]),
                    "source" = em(tweet[["source"]]),
                    "truncated" = em(tweet[["truncated"]]),
                    "in_reply_to_status_id" = em(tweet[["in_reply_to_status_id"]]),
                    "in_reply_to_status_id_str" = em(tweet[["in_reply_to_status_id_str"]]),
                    "in_reply_to_user_id" = em(tweet[["in_reply_to_user_id"]]),
                    "in_reply_to_user_id_str" = em(tweet[["in_reply_to_user_id_str"]]),
                    "in_reply_to_screen_name" = em(tweet[["in_reply_to_screen_name"]]),
                    "coordinates" = ls(tweet[["coordinates"]]),
                    "place_id" = em(tweet[["place"]][["id"]]),
                    "contributors" = em(tweet[["contributors"]]),
                    "is_quote_status" = em(tweet[["is_quote_status"]]),
                    "full_text" = txt(tweet),
                    "quote_count" = em(tweet[["quote_count"]]),
                    "reply_count" = em(tweet[["reply_count"]]),
                    "retweet_count" = em(tweet[["retweet_count"]]),
                    "favorite_count" = em(tweet[["favorite_count"]]),
                    "hashtags" = ls(tweet[["entities"]][["hashtags"]]),
                    "symbols" = ls(tweet[["entities"]][["symbols"]]),
                    "lang" = em(tweet[["lang"]]),
                    "favorited" = em(tweet[["favorited"]]),
                    "retweeted" = em(tweet[["retweeted"]]),
                    "quoted_status_id" = em(tweet[["quoted_status_id"]]),
                    "quoted_status_id_str" = em(tweet[["quoted_status_id_str"]]),
                    "retweeted_status_id" = em(tweet[["retweeded_status"]][["retweeted_status_id"]]),
                    "retweeted_status_id_str" = em(tweet[["retweeded_status"]][["retweeted_status_id_str"]]),
                    "user_id" = em(usr[["id"]]),
                    stringsAsFactors = FALSE
  )
  sqlSave(conn, twt, tablename = "BitcoinTweetsArchived", append = TRUE, rownames = FALSE)
  
  plc <- tweet[["place"]]
  if(!is.null(plc)){
    place <- data.frame("id" = em(plc[["id"]]),
                        "url" = em(plc[["url"]]),
                        "place_type" = em(plc[["place_type"]]),
                        "name" = em(plc[["name"]]),
                        "full_name" = em(plc[["full_name"]]),
                        "country_code" = em(plc[["country_code"]]),
                        "country" = em(plc[["country"]]),
                        "type" = em(plc[["bounding_box"]][["type"]]),
                        "c1" = ls(plc[["bounding_box"]][["coordinates"]][[1]][[1]]),
                        "c2" = ls(plc[["bounding_box"]][["coordinates"]][[1]][[1]]),
                        "c3" = ls(plc[["bounding_box"]][["coordinates"]][[1]][[1]]),
                        "c4" = ls(plc[["bounding_box"]][["coordinates"]][[1]][[1]]),
                        stringsAsFactors = FALSE
    )
    sqlSave(conn, place, tablename = "PlacesIntr", append = TRUE, rownames = FALSE)
  }
}

odbcClose(conn)
odbcCloseAll()  




