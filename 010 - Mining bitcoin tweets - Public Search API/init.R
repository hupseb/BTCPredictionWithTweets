options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages("httpuv")
install.packages("devtools")
install.packages("RODBC")
library("devtools")
devtools::install_github("mkearney/rtweet", force = TRUE, ref="d5c3233")
library("rtweet")
library("RODBC")
library("httpuv")

#bitcoin tweets without retweets
#initial crawl
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
crawlday <- "2019-03-10"
crawldayNext <- "2019-03-11"
tk <- create_token(app="BitcoinTweetsApp",consumer_key="xxx",consumer_secret="xxx", access_token="xxx", access_secret="xxx", set_renv = FALSE)
rt <- search_tweets("#bitcoin -\"RT @\"", n = 500, token = tk, lang = "en", until = crawldayNext)
rt$text_url_cleaned <- NA
sqlSave(conn, rt, tablename = "BitcoinTweets", append = TRUE, rownames = FALSE, verbose = TRUE
        , varTypes = c(created_at="datetime", quoted_created_at= "datetime", retweet_created_at="datetime", account_created_at="datetime"))


#crawl until gap from last crawl is filled
for (i in 1:35) {
  print(i)
  query = sprintf("SELECT TOP 1 [status_id] FROM [BitcoinTwts].[dbo].[BitcoinTweets] WHERE created_at >= '%s' ORDER BY status_id ASC", crawlday)
  oldest <- sqlQuery(conn, query, errors = TRUE, as.is=T)
  rt <- try({search_tweets("#bitcoin -\"RT @\"", n = 1000, token = tk, lang = "en", until = crawldayNext, max_id = oldest[1,])}, silent = TRUE) 
  if (nrow(rt) == 0) {
      print("sleeping for ~15 minutes")
      Sys.time()
      Sys.sleep(910)
  }else{
    rt <- rt[!(rt$status_id==oldest[1,]),]
    rt$text_url_cleaned <- NA
    #remove tweets from day before
    tweets2save <- subset(rt, created_at >= crawlday)
    if(nrow(tweets2save) == 0){
      print("loop done")
      break;
    }
    sqlSave(conn, tweets2save, tablename = "BitcoinTweets", append = TRUE, rownames = FALSE)
    Sys.sleep(10)    
  }
}
odbcClose(conn)
odbcCloseAll()


#TODO
#1. check if last date is has 24 entries
#
#-- Analyze days with gaps
#SELECT d = CONVERT(DATE, [created_at_by_hour] ), c = COUNT(*)
#FROM [BitcoinTwts].[dbo].[BitcoinTweetsCountedByHour]
#GROUP BY CONVERT(DATE, [created_at_by_hour])
#ORDER BY d;
#
#2. Fill crawlday from DB
#2.1 Get crawldaynext as date format
#
#3. Schedule on a Server with a job every 15 minutes
#
#4. Develop nice layout for a website
#
#5. Implement it for the FANG stocks
#
#6. Write a paper
#
#7. Publish paper

