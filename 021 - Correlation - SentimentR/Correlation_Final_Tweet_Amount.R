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

table_name = "Final_Lex_Tweet_Amount_res"

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

#prepare df objects for xts conversion
twtCnt[,1] <- as.Date(twtCnt[,1])
colnames(twtCnt) <- c("date","amount")
btcPriceDf <- data.frame(as.Date(btcPrice[,1]),as.numeric(btcPrice[,5])) 
colnames(btcPriceDf) <- c("date","Close")

#convert to xts
xts_TwtCnt <- xts(twtCnt[,2], order.by = as.Date(twtCnt[,1]))
xts_BtcPrice <- xts(btcPriceDf[,2], order.by = as.Date(btcPriceDf[,1]))

################################################################################################################
# best correlation: xts_sentiment_sum_pos_neg, diff(xts_sentiment_npr)
################################################################################################################
result <- data.frame("lag" = 4:-4)

tmp <- cor(data.frame(xts_BtcPrice, lag(xts_TwtCnt, 4), lag(xts_TwtCnt,3), lag(xts_TwtCnt, 2), lag(xts_TwtCnt, 1), xts_TwtCnt, lag(xts_TwtCnt,-1), lag(xts_TwtCnt,-2), lag(xts_TwtCnt,-3), lag(xts_TwtCnt,-4)), method="pearson", use="pairwise.complete.obs")
result$amount_raw <- tmp[-1,1]

corrDf <- data.frame(xts_BtcPrice, xts_TwtCnt)
corrDf %>% ggpairs() 

#colorful ggpairs and decomposing
ts = ts(as.ts(xts_sentiment_neg), frequency = 7)
decompose_sentiment_neg = decompose(ts, "additive")
corrDf <- data.frame(merge(xts_BtcPrice,decompose_sentiment_neg$trend))
colnames(corrDf)[1] <- "BTC/USD"
colnames(corrDf)[2] <- "Twt_dcmp_trnd"
ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))

#####################################################################
tmp <- cor(data.frame(diff(xts_BtcPrice), lag(diff(xts_TwtCnt), 4), lag(diff(xts_TwtCnt), 3), lag(diff(xts_TwtCnt), 2), lag(diff(xts_TwtCnt), 1), diff(xts_TwtCnt), lag(diff(xts_TwtCnt), -1), lag(diff(xts_TwtCnt), -2), lag(diff(xts_TwtCnt), -3), lag(diff(xts_TwtCnt), -4)), method="pearson", use="pairwise.complete.obs")
result$diff_amount_raw <- tmp[-1,1]

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
sqlSave(conn, result, tablename = table_name, rownames = FALSE, append = TRUE)
odbcClose(conn)

corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_TwtCnt))
corrDf %>% ggpairs() 

#corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npnr)
#ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))
#corrDf %>% ggpairs() 
################################################################################################################


