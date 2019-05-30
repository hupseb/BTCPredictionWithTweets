install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest", "jsonlite", "RODBC", "wordcloud", "sentimentr", "magrittr", "dplyr", "pacman"))
library("RODBC")
library("devtools")
library("sentimentr")
library("magrittr")
options(scipen = 999)

# tweets collected from the iphonex release
# query: "#iphonex -\"RT @\" -#freeshipping -#deals -#sales -#sale -#FREE -#Earthquake -#giveaway -#gameinsight -#Gamesinsight -#iPhoneGames -#Bible -#FreeGames -#FreeApp -#Sale -#zuzu_shop -#Gadgets -#food -#diet -#foodporn -#dessert -#fruit -#pie -#yum -#recipe -#ganic -#fruits -#salad -#nutrition", n = 50, token = tk, lang = "en", until = "2017-11-17"

# get stored tweets from db and explore in RStudio.

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=iPhoneXReleaseTweets;trusted_connection=true')
iphonex.raw <- sqlQuery(conn, "SELECT [status_id]
                        ,[created_at]
                        ,[screen_name]
                        ,[tweet]
                        ,[sentiment] AS cf_sentiment
                         FROM [iPhoneXReleaseTweets].[dbo].[CrowdFlower]
                         WHERE (relevant_yn LIKE 'yes' AND relevant_yn_confidence = 1) AND (sentiment_confidence = 1) AND ([sentiment] LIKE 'positive' OR [sentiment] LIKE 'negative')", as.is = TRUE)

#-------------------------------------------------------------------------------------------------------------

iphonex.sentenced <- iphonex.raw %>% 
  get_sentences(iphonex.raw$tweet)

#sentimentr_sentiword <- function() sentiment(iphonex.sentenced, lexicon::hash_sentiment_sentiword) 

## Make syuzhet dictionaries into sentimentr keys
nrc <- data.frame(
  words = rownames(syuzhet:::nrc),
  polarity = syuzhet:::nrc[, "positive"] - syuzhet:::nrc[, "negative"],
  stringsAsFactors = FALSE
) %>%
{as_key(.[.[["polarity"]] != 0, ])}

#sorted by accuracy
#bing -> 3312
#afinn -> 3205
#syuzhet_dict -> 2700
#nrc -> 2686

dict <- as_key(syuzhet:::syuzhet_dict)
#dict <- as_key(syuzhet:::afinn)
#dict <- as_key(syuzhet:::bing)
#dict <- nrc

sentimentr_sentiword <- function() sentiment(iphonex.sentenced, dict) 
dat <- sentimentr_sentiword()
fullaggregated <- aggregate(list(sentimentr_syuzhet_dict = dat$sentiment), by=list(element_id=dat$element_id), FUN=sum)
aggregated <- aggregate(sentiment ~ element_id + status_id + created_at + screen_name + cf_sentiment, dat, sum)
View(merge(fullaggregated, dat))

View(aggregated)

senticonv <- function(x) {
  switch(x,
         'positive' = 1,
         'neutral' = 0,
         'negative' = -1,
         as.character(x)
  )
}

accuracy <- function(sa, sb) {
  if ((sa > 0 && sb > 0) || (sa < 0 && sb < 0) || (sa == 0 && sb == 0)) {
    return(1)
  }
  else {
    return(0)
  }
}

aggregated$cf_sentiment_val <- sapply(aggregated$cf_sentiment, senticonv)
aggregated$acc <- mapply(accuracy,aggregated$sentiment,aggregated$cf_sentiment_val)
aggregated <- aggregated[order(aggregated$sentiment, decreasing=FALSE),]
View(aggregated)

library("ggplot2")
ggplot(aggregated) +
  geom_col(width=0.1, aes(x = reorder(aggregated$element_id, -sentiment), y = cf_sentiment_val, fill = "CF", colour = "CF")) +
  geom_col(width=0.1, aes(x = reorder(aggregated$element_id, -sentiment), y = sentiment, fill = "SM",  colour = "SM")) + 
  scale_colour_manual("", 
                      breaks = c("CF", "SM"),
                      values = c("#7BADD7", "#29567D"),
                      guide = FALSE) +
  scale_fill_manual("sources:", 
                      breaks = c("CF", "SM"),
                      values = c("#7BADD7", "#29567D"),
                      labels = c("CrowdFlower", "SentimentR")) +
  ylim(-2, 2) +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=2364) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  labs(x = "tweets", y = "sentiment") 

ggsave("CrowdFlower_vs_SentimentR.png", plot = last_plot(), limitsize = FALSE, scale = 1, units = "cm", device = "png")

#accuracy
sum(aggregated$acc)
