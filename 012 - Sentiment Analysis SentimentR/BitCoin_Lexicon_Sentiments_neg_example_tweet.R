options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest", "jsonlite", "RODBC", "wordcloud", "sentimentr", "magrittr", "dplyr", "pacman"))
library("RODBC")
library("devtools")
library("sentimentr")
library("magrittr")


# get stored tweets from db and explore in RStudio.
#conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
#bitcoin.raw <- sqlQuery(conn, "SELECT TOP (1000) [status_id]
#                        ,[created_at]
#                        ,[user_id]
#                        ,[tweet]
#                        ,[sentiment] AS fe_sentiment
#                        FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw]
#                        WHERE (relevant_yn LIKE 'yes' AND relevant_yn_confidence = 1) AND sentiment_confidence = 1 AND source = 'Twitter Web Client'
#                        AND [created_at] >= Convert(datetime2, '2018-02-10 00:00:00.000' )
#                        ORDER BY [created_at]", as.is = TRUE)
#close(conn)
#
#bitcoin.sentenced <- bitcoin.raw %>% 
#  get_sentences(bitcoin.raw$tweet)

######################################################################################################################

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


######################################################################################################################
dics <- list(#lexicon::hash_sentiment_huliu,
             #lexicon::hash_sentiment_jockers,
             #lexicon::hash_sentiment_jockers_rinker,
             #lexicon::hash_sentiment_loughran_mcdonald,
             #lexicon::hash_sentiment_nrc,
             #lexicon::hash_sentiment_senticnet,
             #lexicon::hash_sentiment_sentiword,
             #lexicon::hash_sentiment_slangsd,
             #lexicon::hash_sentiment_socal_google,
             as_key(syuzhet:::afinn)
             #as_key(syuzhet:::bing),
             #as_key(syuzhet:::syuzhet_dict)
             )

tblnames <- list(#"Final_Lex_huliu",
                 #"Final_Lex_jockers",
                 #"Final_Lex_jockers_rinker",
                 #"Final_Lex_loughran_mcdonald",
                 #"Final_Lex_nrc",
                 #"Final_Lex_senticnet",
                 #"Final_Lex_sentiword",
                 #"Final_Lex_slangsd",
                 #"Final_Lex_socal_google",
                 "Final_Lex_afinn"
                 #"Final_Lex_bing",
                 #"Final_Lex_syuzhet_dict"
                 )

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
newdata = sqlQuery(conn, paste("SELECT TOP 10 * FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered_FinalInterval] WHERE status_id = '991312813988630528' ORDER BY [status_id]"), as.is = TRUE)

n <- 1
nr <- nrow(newdata)
newdata_sp = split(newdata, rep(1:ceiling(nr/n), each=n, length.out=nr))

for(j in 1:length(dics)){
  dic      <- dics[[j]]
  tbl_name <- tblnames[[j]]
  
  for(i in 1:length(newdata_sp)){
    bitcoin.raw <- newdata_sp[[i]]
    print(bitcoin.raw[1,1])
    print(tbl_name)
    
    bitcoin.sentenced <- bitcoin.raw %>% get_sentences(bitcoin.raw$tweet)
    
    sentimentr <- function() sentiment(bitcoin.sentenced,
                                       polarity_dt = dic,
                                       valence_shifters_dt = lexicon::hash_valence_shifters,
                                       amplifier.weight = 0.8,
                                       n.before = 5,
                                       n.after = 2,
                                       question.weight = 0,
                                       adversative.weight = 0.85)
    
    dat <- sentimentr()
    fullaggregated <- aggregate(list(sentimentr_dict = dat$sentiment), by=list(element_id=dat$element_id), FUN=sum)
    aggregated <- aggregate(sentiment ~ element_id + status_id + created_at + user_id, dat, sum)
    #View(merge(fullaggregated, dat))
    
    aggregated <- aggregated[order(aggregated$sentiment, decreasing=FALSE),]
    #View(aggregated)
    
    #sqlSave(conn, aggregated, tablename = tbl_name, rownames = FALSE, append = TRUE)
  }
}

#############################################################################################

#library("ggplot2")
#ggplot(aggregated) +
#  geom_col(width=0.1, aes(x = reorder(aggregated$element_id, -sentiment), y = fe_sentiment_val, fill = "CF", colour = "CF")) +
#  geom_col(width=0.1, aes(x = reorder(aggregated$element_id, -sentiment), y = sentiment, fill = "SM",  colour = "SM")) + 
#  scale_colour_manual("", 
#                      breaks = c("CF", "SM"),
#                      values = c("#7BADD7", "#29567D"),
#                      guide = FALSE) +
#  scale_fill_manual("sources:", 
#                      breaks = c("CF", "SM"),
#                      values = c("#7BADD7", "#29567D"),
#                      labels = c("CrowdFlower", "SentimentR")) +
#  ylim(-2, 2) +
#  geom_hline(yintercept=0) + 
#  geom_vline(xintercept=2364) +
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
#  labs(x = "tweets", y = "sentiment") 
#
#ggsave("CrowdFlowerVsSentimentR.png", plot = last_plot(), limitsize = FALSE, scale = 2.5, width=15, height=7, dpi=200, units = "cm", device = "png")
#ggsave("CrowdFlowerVsSentimentR12x8.png", plot = last_plot(), limitsize = FALSE, scale = 2.5, width=12, height=8, dpi=200, units = "cm", device = "png")
