options(scipen=999)
Sys.setenv(TZ='UTC')

#install.packages("httpuv")
#install.packages("devtools")
#install.packages("RODBC")
install.packages("GGally")
install.packages("fpp2")
install.packages("gridExtra")
library("mgcv") 
library("devtools")
library("forecast")
library("RODBC")
library("httpuv")
library("xts")
library("plyr")
library("GGally")
library("fpp2")
library("stringi")
library("gridExtra")

corrPltFun <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) 
}

table_name = "Final_Lex_syuzhet_dict_res"

#get figure eight results
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
btcTwts = sqlQuery(conn, paste("SELECT A.[status_id]
                               ,A.[created_at]
                               ,A.[user_id]
                               ,A.[sentiment]
                               ,B.favorite_count
                               ,B.retweet_count
                               FROM [BitcoinTwts].[dbo].[Final_Lex_syuzhet_dict] AS A
                               INNER JOIN [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered_FinalInterval] AS B ON A.[status_id] = B.[status_id]
                               ORDER BY A.[created_at]"), as.is = TRUE)
odbcClose(conn)

#get amount of tweets
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("SELECT * FROM [BitcoinAnalysis].[dbo].[06. TwitterVolume_d]
                WHERE [date] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
                [date] <  Convert(datetime2, '2018-06-10 00:00:00.000' )")
twtCnt <- sqlQuery(conn, query, errors = TRUE, as.is=T)
odbcClose(conn)

#get bitcoin price
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("SELECT * FROM [BitcoinAnalysis].[dbo].[01. Bitfinex_C_TradingView_d]
                WHERE [timestamp] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
                [timestamp] <  Convert(datetime2, '2018-06-10 00:00:00.000' )")
btcPrice <- sqlQuery(conn, query, errors = TRUE, as.is=T)
odbcClose(conn)

btcTwts$created_at_date <- as.Date(btcTwts$created_at, "%Y-%m-%d")
btcTwts$sentiment_str <- btcTwts$sentiment
btcTwts$sentiment <- as.numeric(btcTwts$sentiment)

srS <- btcTwts %>% mutate(sentiment_pos = ifelse(btcTwts$sentiment > 0, 1, 0))
srS <- srS %>% mutate(sentiment_neg = ifelse(srS$sentiment < 0, -1,0))
srS <- srS %>% mutate(sentiment_neu = ifelse(srS$sentiment == 0, 1,0))
srS <- srS %>% mutate(sentiment_pos_amp = ifelse((srS$sentiment_pos == 1 & srS$favorite_count > 0),
                                                 srS$sentiment*(srS$favorite_count+1),
                                                 (ifelse((srS$sentiment_pos == 1 & srS$favorite_count == 0),
                                                         1*srS$sentiment,
                                                         0))))
srS <- srS %>% mutate(sentiment_neg_amp = ifelse((srS$sentiment_neg == -1 & srS$favorite_count > 0),
                                                 srS$sentiment*(srS$favorite_count+1),
                                                 (ifelse((srS$sentiment_neg == -1 & srS$favorite_count == 0),
                                                         1*srS$sentiment,
                                                         0))))

#aggregate to daily
sentiment_raw <- aggregate(x = srS[c("sentiment")],FUN = sum, by = list(date = srS$created_at_date))
sentiment_pos <- aggregate(x = srS[c("sentiment_pos")],FUN = sum, by = list(date = srS$created_at_date))
sentiment_neg <- aggregate(x = srS[c("sentiment_neg")],FUN = sum, by = list(date = srS$created_at_date))
sentiment_neu <- aggregate(x = srS[c("sentiment_neu")],FUN = sum, by = list(date = srS$created_at_date))
sentiment_pos_amp <- aggregate(x = srS[c("sentiment_pos_amp")],FUN = sum, by = list(date = srS$created_at_date))
sentiment_neg_amp <- aggregate(x = srS[c("sentiment_neg_amp")],FUN = sum, by = list(date = srS$created_at_date))

sentiment_npr <- sentiment_pos
colnames(sentiment_npr) <- c('date','sentiment_npr')
sentiment_npr[,2] <- ((sentiment_pos[,2]-(sentiment_neg[,2]*-1))/(sentiment_pos[,2]+(sentiment_neg[,2]*-1)))

sentiment_npr_amp <- sentiment_pos_amp
colnames(sentiment_npr_amp) <- c('date','sentiment_npr_amp')
sentiment_npr_amp[,2] <- ((sentiment_pos_amp[,2]-(sentiment_neg_amp[,2]*-1))/(sentiment_pos_amp[,2]+(sentiment_neg_amp[,2]*-1)))

sentiment_npnr <- sentiment_neu
colnames(sentiment_npnr) <- c('date','sentiment_npnr')
sentiment_npnr[,2] <- ((sentiment_pos[,2]-sentiment_neg[,2]*-1)/(sentiment_pos[,2]+sentiment_neg[,2]*-1+sentiment_neu[,2]))

sentiment_sum_pos_neg <- sentiment_pos
colnames(sentiment_sum_pos_neg) <- c('date','sentiment_sum_pos_neg')
sentiment_sum_pos_neg[,2] <- (sentiment_pos[,2] + sentiment_neg[,2])

sentiment_npr_alt <- sentiment_pos
colnames(sentiment_npr_alt) <- c('date','sentiment_npr_alt')
sentiment_npr_alt[,2] <- ifelse(sentiment_pos[,2] < (sentiment_neg[,2]*-1), -1*((sentiment_neg[,2]*-1)/sentiment_pos[,2]),
                                ifelse(sentiment_pos[,2] > (sentiment_neg[,2]*-1), sentiment_pos[,2]/(sentiment_neg[,2]*-1), 0))

#prepare df objects for xts conversion
twtCnt[,1] <- as.Date(twtCnt[,1])
colnames(twtCnt) <- c("date","amount")
btcPriceDf <- data.frame(as.Date(btcPrice[,1]),as.numeric(btcPrice[,5])) 
colnames(btcPriceDf) <- c("date","Close")

#convert to xts
xts_sentiment_raw <- xts(sentiment_raw[,2], order.by = as.Date(sentiment_raw[,1]))
xts_sentiment_pos <- xts(sentiment_pos[,2], order.by = as.Date(sentiment_pos[,1]))
xts_sentiment_neg <- xts(sentiment_neg[,2], order.by = as.Date(sentiment_neg[,1]))
xts_sentiment_pos_amp <- xts(sentiment_pos_amp[,2], order.by = as.Date(sentiment_pos_amp[,1]))
xts_sentiment_neg_amp <- xts(sentiment_neg_amp[,2], order.by = as.Date(sentiment_neg_amp[,1]))
xts_sentiment_npr <- xts(sentiment_npr[,2], order.by = as.Date(sentiment_npr[,1]))
xts_sentiment_npr_amp <- xts(sentiment_npr_amp[,2], order.by = as.Date(sentiment_npr_amp[,1]))
xts_sentiment_npnr <- xts(sentiment_npnr[,2], order.by = as.Date(sentiment_npnr[,1]))
xts_sentiment_sum_pos_neg <- xts(sentiment_sum_pos_neg[,2], order.by = as.Date(sentiment_sum_pos_neg[,1]))
xts_sentiment_npr_alt <- xts(sentiment_npr_alt[,2], order.by = as.Date(sentiment_npr_alt[,1]))
xts_TwtCnt <- xts(twtCnt[,2], order.by = as.Date(twtCnt[,1]))
xts_BtcPrice <- xts(btcPriceDf[,2], order.by = as.Date(btcPriceDf[,1]))

#percentage change in btcPrice
xts_sentiment_rawPer <- (xts_sentiment_raw/lag(xts_sentiment_raw,1) - 1)
xts_sentiment_pPer <- (xts_sentiment_pos/lag(xts_sentiment_pos,1) - 1)
xts_sentiment_nPer <- (xts_sentiment_neg/lag(xts_sentiment_neg,1) - 1)
xts_sentiment_pos_ampPer <- (xts_sentiment_pos_amp/lag(xts_sentiment_pos_amp,1) - 1)
xts_sentiment_neg_ampPer <- (xts_sentiment_neg_amp/lag(xts_sentiment_neg_amp,1) - 1)
xts_sentiment_nprPer <- (xts_sentiment_npr/lag(xts_sentiment_npr,1) - 1)
xts_sentiment_npr_ampPer <- (xts_sentiment_npr_amp/lag(xts_sentiment_npr_amp,1) - 1)
xts_sentiment_npnrPer <- (xts_sentiment_npnr/lag(xts_sentiment_npnr,1) - 1)
xts_sentiment_sum_pos_negPer <- (xts_sentiment_sum_pos_neg/lag(xts_sentiment_sum_pos_neg,1) - 1)
xts_sentiment_npr_altPer <- (xts_sentiment_npr_alt/lag(xts_sentiment_npr_alt,1) - 1)
xts_TwtCntPer <- (xts_TwtCnt/lag(xts_TwtCnt,-1) - 1)
xts_BtcPricePer <- (xts_BtcPrice/lag(xts_BtcPrice,-1) - 1)

################################################################################################################
# best correlation: xts_sentiment_sum_pos_neg, diff(xts_sentiment_npr)
################################################################################################################
result <- data.frame("lag" = 4:-4)

tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_raw, 4), lag(xts_sentiment_raw,3), lag(xts_sentiment_raw, 2), lag(xts_sentiment_raw, 1), xts_sentiment_raw, lag(xts_sentiment_raw,-1), lag(xts_sentiment_raw,-2), lag(xts_sentiment_raw,-3), lag(xts_sentiment_raw,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_raw <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_pos, 4), lag(xts_sentiment_pos,3), lag(xts_sentiment_pos, 2), lag(xts_sentiment_pos, 1), xts_sentiment_pos, lag(xts_sentiment_pos,-1), lag(xts_sentiment_pos,-2), lag(xts_sentiment_pos,-3), lag(xts_sentiment_pos,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_pos <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_neg, 4), lag(xts_sentiment_neg,3), lag(xts_sentiment_neg, 2), lag(xts_sentiment_neg, 1), xts_sentiment_neg, lag(xts_sentiment_neg,-1), lag(xts_sentiment_neg,-2), lag(xts_sentiment_neg,-3), lag(xts_sentiment_neg,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_neg <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_pos_amp, 4), lag(xts_sentiment_pos_amp,3), lag(xts_sentiment_pos_amp, 2), lag(xts_sentiment_pos_amp, 1), xts_sentiment_pos_amp, lag(xts_sentiment_pos_amp,-1), lag(xts_sentiment_pos_amp,-2), lag(xts_sentiment_pos_amp,-3), lag(xts_sentiment_pos_amp,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_pos_amp <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_neg_amp, 4), lag(xts_sentiment_neg_amp,3), lag(xts_sentiment_neg_amp, 2), lag(xts_sentiment_neg_amp, 1), xts_sentiment_neg_amp, lag(xts_sentiment_neg_amp,-1), lag(xts_sentiment_neg_amp,-2), lag(xts_sentiment_neg_amp,-3), lag(xts_sentiment_neg_amp,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_neg_amp <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_npr, 4), lag(xts_sentiment_npr,3), lag(xts_sentiment_npr, 2), lag(xts_sentiment_npr, 1), xts_sentiment_npr, lag(xts_sentiment_npr,-1), lag(xts_sentiment_npr,-2), lag(xts_sentiment_npr,-3), lag(xts_sentiment_npr,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_npr <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_npr_amp, 4), lag(xts_sentiment_npr_amp,3), lag(xts_sentiment_npr_amp, 2), lag(xts_sentiment_npr_amp, 1), xts_sentiment_npr_amp, lag(xts_sentiment_npr_amp,-1), lag(xts_sentiment_npr_amp,-2), lag(xts_sentiment_npr_amp,-3), lag(xts_sentiment_npr_amp,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_npr_amp <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_npnr, 4), lag(xts_sentiment_npnr,3), lag(xts_sentiment_npnr, 2), lag(xts_sentiment_npnr, 1), xts_sentiment_npnr, lag(xts_sentiment_npnr,-1), lag(xts_sentiment_npnr,-2), lag(xts_sentiment_npnr,-3), lag(xts_sentiment_npnr,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_npnr <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_sum_pos_neg, 4), lag(xts_sentiment_sum_pos_neg,3), lag(xts_sentiment_sum_pos_neg, 2), lag(xts_sentiment_sum_pos_neg, 1), xts_sentiment_sum_pos_neg, lag(xts_sentiment_sum_pos_neg,-1), lag(xts_sentiment_sum_pos_neg,-2), lag(xts_sentiment_sum_pos_neg,-3), lag(xts_sentiment_sum_pos_neg,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_sum_pos_neg <- tmp[-1,1]
tmp <- cor(data.frame(xts_BtcPrice, lag(xts_sentiment_npr_alt, 4), lag(xts_sentiment_npr_alt,3), lag(xts_sentiment_npr_alt, 2), lag(xts_sentiment_npr_alt, 1), xts_sentiment_npr_alt, lag(xts_sentiment_npr_alt,-1), lag(xts_sentiment_npr_alt,-2), lag(xts_sentiment_npr_alt,-3), lag(xts_sentiment_npr_alt,-4)), method="pearson", use="pairwise.complete.obs")
result$sentiment_npr_alt <- tmp[-1,1]

corrDf <- data.frame(xts_BtcPrice, xts_sentiment_raw)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_pos)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_neg)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_pos_amp)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_neg_amp)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npr)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npr_amp)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npnr)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_sum_pos_neg)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npr_alt)
corrDf %>% ggpairs() 

#colorful ggpairs and decomposing
ts = ts(as.ts(xts_sentiment_neg), frequency = 7)
decompose_sentiment_neg = decompose(ts, "additive")
corrDf <- data.frame(merge(xts_BtcPrice,decompose_sentiment_neg$trend))
colnames(corrDf)[1] <- "BTC/USD"
colnames(corrDf)[2] <- "Twt_dcmp_trnd"
ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))

# percentage calculation - did not work out #########################
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_rawPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_pPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_nPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_pos_ampPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_neg_ampPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_nprPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_npr_ampPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_npnrPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_sum_pos_negPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_npr_altPer)
corrDf %>% ggpairs() 
#####################################################################
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_raw), 4), lag(diff(xts_sentiment_raw), 3), lag(diff(xts_sentiment_raw), 2), lag(diff(xts_sentiment_raw), 1), diff(xts_sentiment_raw), lag(diff(xts_sentiment_raw), -1), lag(diff(xts_sentiment_raw), -2), lag(diff(xts_sentiment_raw), -3), lag(diff(xts_sentiment_raw), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_raw <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_pos), 4), lag(diff(xts_sentiment_pos), 3), lag(diff(xts_sentiment_pos), 2), lag(diff(xts_sentiment_pos), 1), diff(xts_sentiment_pos), lag(diff(xts_sentiment_pos), -1), lag(diff(xts_sentiment_pos), -2), lag(diff(xts_sentiment_pos), -3), lag(diff(xts_sentiment_pos), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_pos <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_neg), 4), lag(diff(xts_sentiment_neg),3), lag(diff(xts_sentiment_neg), 2), lag(diff(xts_sentiment_neg), 1), diff(xts_sentiment_neg), lag(diff(xts_sentiment_neg), -1), lag(diff(xts_sentiment_neg), -2), lag(diff(xts_sentiment_neg), -3), lag(diff(xts_sentiment_neg),-4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_neg <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_pos_amp), 4), lag(diff(xts_sentiment_pos_amp), 3), lag(diff(xts_sentiment_pos_amp), 2), lag(diff(xts_sentiment_pos_amp), 1), diff(xts_sentiment_pos_amp), lag(diff(xts_sentiment_pos_amp), -1), lag(diff(xts_sentiment_pos_amp), -2), lag(diff(xts_sentiment_pos_amp), -3), lag(diff(xts_sentiment_pos_amp) , -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_pos_amp <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_neg_amp), 4), lag(diff(xts_sentiment_neg_amp), 3), lag(diff(xts_sentiment_neg_amp), 2), lag(diff(xts_sentiment_neg_amp), 1), diff(xts_sentiment_neg_amp), lag(diff(xts_sentiment_neg_amp), -1), lag(diff(xts_sentiment_neg_amp), -2), lag(diff(xts_sentiment_neg_amp), -3), lag(diff(xts_sentiment_neg_amp), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_neg_amp <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_npr), 4), lag(diff(xts_sentiment_npr),3), lag(diff(xts_sentiment_npr), 2), lag(diff(xts_sentiment_npr), 1), diff(xts_sentiment_npr), lag(diff(xts_sentiment_npr), -1), lag(diff(xts_sentiment_npr), -2), lag(diff(xts_sentiment_npr), -3), lag(diff(xts_sentiment_npr), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_npr <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_npr_amp), 4), lag(diff(xts_sentiment_npr_amp), 3), lag(diff(xts_sentiment_npr_amp), 2), lag(diff(xts_sentiment_npr_amp), 1), diff(xts_sentiment_npr_amp), lag(diff(xts_sentiment_npr_amp), -1), lag(diff(xts_sentiment_npr_amp), -2), lag(diff(xts_sentiment_npr_amp), -3), lag(diff(xts_sentiment_npr_amp), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_npr_amp <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_npnr), 4), lag(diff(xts_sentiment_npnr), 3), lag(diff(xts_sentiment_npnr), 2), lag(diff(xts_sentiment_npnr), 1), diff(xts_sentiment_npnr), lag(diff(xts_sentiment_npnr), -1), lag(diff(xts_sentiment_npnr), -2), lag(diff(xts_sentiment_npnr), -3), lag(diff(xts_sentiment_npnr), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_npnr <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_sum_pos_neg), 4), lag(diff(xts_sentiment_sum_pos_neg), 3), lag(diff(xts_sentiment_sum_pos_neg), 2), lag(diff(xts_sentiment_sum_pos_neg), 1), diff(xts_sentiment_sum_pos_neg), lag(diff(xts_sentiment_sum_pos_neg), -1), lag(diff(xts_sentiment_sum_pos_neg), -2), lag(diff(xts_sentiment_sum_pos_neg), -3), lag(diff(xts_sentiment_sum_pos_neg), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_sum_pos_neg <- tmp[-1,1]
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_sentiment_npr_alt), 4), lag(diff(xts_sentiment_npr_alt), 3), lag(diff(xts_sentiment_npr_alt), 2), lag(diff(xts_sentiment_npr_alt), 1), diff(xts_sentiment_npr_alt), lag(diff(xts_sentiment_npr_alt) , -1), lag(diff(xts_sentiment_npr_alt), -2), lag(diff(xts_sentiment_npr_alt), -3), lag(diff(xts_sentiment_npr_alt), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_sentiment_npr_alt <- tmp[-1,1]

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
sqlSave(conn, result, tablename = table_name, rownames = FALSE, append = TRUE)
odbcClose(conn)

corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_raw))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_pos))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_neg))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_pos_amp))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_neg_amp))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npr))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npr_amp))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npnr))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_sum_pos_neg))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npr_alt))
corrDf %>% ggpairs() 

#corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npnr)
#ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))
#corrDf %>% ggpairs() 

################################################################################################################


