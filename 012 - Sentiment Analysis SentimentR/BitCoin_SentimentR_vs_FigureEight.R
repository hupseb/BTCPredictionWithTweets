install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest", "jsonlite", "RODBC", "wordcloud", "sentimentr", "magrittr", "dplyr", "pacman"))
library("RODBC")
library("devtools")
library("sentimentr")
library("magrittr")
options(scipen = 999)

# get stored tweets from db and explore in RStudio.
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
bitcoin.raw <- sqlQuery(conn, "SELECT TOP (1000) [status_id]
                        ,[created_at]
                        ,[user_id]
                        ,[tweet]
                        ,[sentiment] AS fe_sentiment
                        ,[retweet_count]
                        ,[favorite_count]
                        FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw]
                        WHERE (relevant_yn LIKE 'yes' AND relevant_yn_confidence = 1) AND sentiment_confidence = 1 AND source = 'Twitter Web Client'
                        AND [created_at] >= Convert(datetime2, '2018-06-01 00:00:00.000' )
                        ORDER BY [created_at]", as.is = TRUE)
close(conn)

bitcoin.sentenced <- bitcoin.raw %>% 
  get_sentences(bitcoin.raw$tweet)

#-------------------------------------------------------------------------------------------------------------



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

## Make syuzhet dictionaries into sentimentr keys
nrc <- data.frame(
  words = rownames(syuzhet:::nrc),
  polarity = syuzhet:::nrc[, "positive"] - syuzhet:::nrc[, "negative"],
  stringsAsFactors = FALSE
) %>%
{as_key(.[.[["polarity"]] != 0, ])}

#nrc -> 2686
#bing -> 3371
#afinn -> 3396
#syuzhet_dict -> 2700
#vadar -> 3122
#inquirer -> 2882
#loughran_mcdonald -> 3167
#senticnet -> 1530
#sentiword -> 1752

#polarity_dt = lexicon::hash_sentiment_jockers,
#valence_shifters_dt = lexicon::hash_valence_shifters,
#hyphen = "",
#amplifier.weight = 0.8
#n.before = 5
#n.after = 2,
#question.weight = 1, adversative.weight = 0.85, missing_value = 0, ...)


#dict <- lexicon::hash_sentiment_huliu
#dict <- as_key(syuzhet:::afinn)
#dict <- lexicon::hash_sentiment_jockers_rinker
#dict <- lexicon::hash_sentiment_vadar
#dict <- lexicon::hash_sentiment_inquirer
#dict <- as_key(syuzhet:::bing)
#dict <- as_key(syuzhet:::syuzhet_dict)
dict <- lexicon::hash_sentiment_loughran_mcdonald
#dict <- lexicon::hash_sentiment_nrc
#dict <- lexicon::hash_sentiment_sentiword
#dict <- lexicon::hash_sentiment_senticnet


sentimentr <- function() sentiment(bitcoin.sentenced,
                                             polarity_dt = dict,
                                             valence_shifters_dt = lexicon::hash_valence_shifters,
                                             amplifier.weight = 0.8,
                                             n.before = 5,
                                             n.after = 2,
                                             question.weight = 0,
                                             adversative.weight = 0.85
                                             )

dat <- sentimentr()
fullaggregated <- aggregate(list(sentimentr_dict = dat$sentiment), by=list(element_id=dat$element_id), FUN=sum)
aggregated <- aggregate(sentiment ~ element_id + status_id + created_at + user_id + fe_sentiment, dat, sum)
View(merge(fullaggregated, dat))

aggregated$fe_sentiment_val <- sapply(aggregated$fe_sentiment, senticonv)
aggregated$acc <- mapply(accuracy,aggregated$sentiment,aggregated$fe_sentiment_val)
aggregated <- aggregated[order(aggregated$sentiment, decreasing=FALSE),]
View(aggregated)

#accuracy
sum(aggregated$acc)

library("ggplot2")
ggplot(aggregated) +
  geom_col(width=0.1, aes(x = reorder(aggregated$element_id, -sentiment), y = fe_sentiment_val, fill = "CF", colour = "CF")) +
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

ggsave("CrowdFlowerVsSentimentR.png", plot = last_plot(), limitsize = FALSE, scale = 2.5, width=15, height=7, dpi=200, units = "cm", device = "png")
ggsave("CrowdFlowerVsSentimentR12x8.png", plot = last_plot(), limitsize = FALSE, scale = 2.5, width=12, height=8, dpi=200, units = "cm", device = "png")
