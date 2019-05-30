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

table_name = "Final_SVM_sentiment_res"

#get figure eight results
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
btcTwts = sqlQuery(conn, paste("SELECT A.[status_id]
                               ,A.[decisionValue] AS sentiment
                               ,B.[user_id]
                               ,B.[created_at]
                               ,B.favorite_count
                               ,B.retweet_count
                               FROM [BitcoinTwts].[dbo].[Final_SVM_sentiment] AS A
                               INNER JOIN [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered_FinalInterval] AS B ON A.[status_id] = B.[status_id]
                               ORDER BY B.[created_at]"), as.is = TRUE)
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

#get other timeseries for bitcoin
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("SELECT [Date],[Market_Capitalization] FROM [BitcoinAnalysis].[dbo].[15.14 market-cap] WHERE ([Date] >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND ([Date] < CONVERT(datetime2, '2018-06-10 00:00:00.000'))")
marketcap <- sqlQuery(conn, query, errors = TRUE, as.is=T)
odbcClose(conn)

xts_MarketCap <- xts(as.numeric(marketcap[,2]), order.by = as.Date(marketcap[,1]))

##################################################################################################

fit <- auto.arima(as.ts(xts_BtcPrice), xreg=as.ts(xts_sentiment_raw))
fit
autoplot(fit)
autoplot(forecast(fit,h=5, xreg=as.ts(xts_sentiment_raw)))
checkresiduals(fit)

fit <- auto.arima(as.ts(xts_BtcPrice))
fit
fc <- forecast(fit,h=5)
fc
fit <- auto.arima(as.ts(xts_BtcPrice), xreg=as.ts(xts_TwtCnt))
fit
autoplot(as.ts(xts_BtcPrice))
autoplot(fit[["fitted"]])

autoplot(forecast(fit))
checkresiduals(fit)

fit <- arima(as.ts(xts_BtcPrice), order=c(3,1,0))
autoplot(forecast(fit,h=5))
checkresiduals(fit)

ggAcf(as.ts(xts_BtcPrice), main="")
ggPacf(as.ts(xts_BtcPrice), main="")

#interesting: https://towardsdatascience.com/bitcoin-price-prediction-using-time-series-forecasting-9f468f7174d3
#
#fitTest <- auto.arima(as.ts(xts_BtcPrice), xreg=as.ts(log(lag(xts_BtcPrice,1))))
#accuracy(fitTest[["fitted"]],btcPriceDf[,2])

fit.btcPrice <- tslm(as.ts(xts_BtcPrice) ~ as.ts(xts_TwtCnt) + as.ts(lag(xts_BtcPrice,-1)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

diffBtcPrice <- tail(as.ts(diff(xts_BtcPrice)),-1)
diffTwtCnt <- tail(as.ts(diff(xts_TwtCnt)),-1)
fit.btcPrice <- tslm(diffBtcPrice ~ diffTwtCnt)
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))






fit.btcPrice <- auto.arima(xts_BtcPrice)
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(lag(xts_BtcPrice,-1)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice, order=c(1,0,0))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice, order=c(1,1,0), xreg=as.ts(diff(xts_TwtCnt)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice, order=c(1,1,0), xreg=as.ts(lag(xts_TwtCnt,-5)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_TwtCnt))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

TwtCnts <- cbind(
  Lag0 = as.ts(xts_TwtCnt),
  Lag1 = stats::lag(as.ts(xts_TwtCnt),-1),
  Lag2 = stats::lag(as.ts(xts_TwtCnt),-2),
  Lag3 = stats::lag(as.ts(xts_TwtCnt),-3))

fit.btcPrice <- auto.arima(xts_BtcPrice[4:133], xreg=TwtCnts[4:133,2:2])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

##################################################################################

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_sum_pos_neg))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice, order=c(1,1,0), xreg=as.ts(lag(xts_sentiment_sum_pos_neg,-1)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(lag(xts_sentiment_sum_pos_neg,-1)))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_npnr))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_npr))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_npr_alt))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_pos))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_pos_amp))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice, xreg=as.ts(xts_sentiment_raw))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

##############




regressors <- cbind(
  twtcnt = as.ts(xts_TwtCnt),
  raw = as.ts(xts_sentiment_raw),
  twtcntLag1 = stats::lag(as.ts(xts_TwtCnt),-1),
  sum_pos_neg = as.ts(xts_sentiment_sum_pos_neg),
  sum_pos_negLag1 = stats::lag(as.ts(xts_sentiment_sum_pos_neg),-1),
  marketcap = as.ts(xts_MarketCap),
  rawLag1 = stats::lag(as.ts(xts_sentiment_raw),-1),
  marketcap = stats::lag(as.ts(xts_MarketCap),-1),
  npr = as.ts(xts_sentiment_npr),
  nprLag1 = stats::lag(xts_sentiment_npr),-1)

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,c(9)])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,4:4])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,2:2])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

splinef(lambda = 0, xts_BtcPrice) %>% autoplot()

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,c(1,4)])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,c(4)])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,c(4,5)])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], xreg=regressors[2:133,c(1,5)])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

#TODO:
#check if regression is better
#get other regressors from excelsheets and database
#create binary matrix with other regressors

acf(xts_BtcPrice)