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
library("timeDate")

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
query = sprintf("/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [Date]
                ,[Annual_Hash_Growth]
                ,[Blockchain_Size]
                ,[Block_Height]
                ,[Block_Interval]
                ,[Daily_Blocks]
                ,[Block_Size]
                ,[Chain_Value_Density]
                ,[Daily_Transactions]
                ,[Difficulty]
                ,[Fee_Percentage]
                ,[Fee_Rate]
                ,[Hash_Rate]
                ,[Two_Week_Hash_Growth]
                ,[Market_Capitalization]
                ,[Metcalfe_s_Law___TX]
                ,[Metcalfe_s_Law___UTXO]
                ,[Miner_Revenue]
                ,[Miner_Revenue_Value]
                ,[Money_Supply]
                ,[Output_Value]
                ,[Output_Volume]
                ,[Bitcoin_Core__BTC__Price]
                ,[Quarterly_Hash_Growth]
                ,[Total_Transactions]
                ,[Transaction_Amount]
                ,[Transaction_Fees]
                ,[Fees_Value]
                ,[Transaction_Size]
                ,[Transactions_per_Block]
                ,[Transaction_Value]
                ,[Avg__UTXO_Amount]
                ,[UTXO_Growth]
                ,[UTXO_Set_Size]
                ,[Avg__UTXO_Value]
                ,[Velocity_of_Money]
                ,[Velocity___Quarterly]
                ,[Velocity___Daily]
                FROM [BitcoinAnalysis].[dbo].[15xx_BTC_technical_ts]
                WHERE ([Date] >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND ([Date] < CONVERT(datetime2, '2018-06-10 00:00:00.000'))
                ORDER BY [Date]")
btc_technical_ts <- sqlQuery(conn, query, errors = TRUE, as.is=T)
btc_ts <- btc_technical_ts
odbcClose(conn)

#adjustments
btc_ts[,c(2,3,8,10,11,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,31,32,35,36,37,38)] <-
  sapply(btc_ts[,c(2,3,8,10,11,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,31,32,35,36,37,38)], as.numeric)

#add other time series
#get other timeseries for bitcoin
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("/****** Script for SelectTopNRows command from SSMS  ******/
                SELECT [Date]
                ,[volatility]
                ,[shorts]
                ,[longs]
                ,[stockpulse_sent]
                ,[stockpulse_buzz]
                ,[sp500]
                FROM [BitcoinAnalysis].[dbo].[0x_mutual_ts]
                WHERE ([Date] >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND ([Date] < CONVERT(datetime2, '2018-06-10 00:00:00.000'))
                ORDER BY [Date]")
btc_mutual_ts <- sqlQuery(conn, query, errors = TRUE, as.is=T)
odbcClose(conn)

#adjustments
btc_mutual_ts[,c(2,3,4,5,6,7)] <- sapply(btc_mutual_ts[,c(2,3,4,5,6,7)], as.numeric)

#add to btc_ts
btc_ts$volatility <- btc_mutual_ts$volatility
btc_ts$shorts <- btc_mutual_ts$shorts
btc_ts$longs  <- btc_mutual_ts$longs
btc_ts$stockpulse_sent <- btc_mutual_ts$stockpulse_sent
btc_ts$stockpulse_buzz <- btc_mutual_ts$stockpulse_buzz
btc_ts$sp500 <- btc_mutual_ts$sp500

#add tweet time series
btc_ts$SVM_sentiment_neg <- as.ts(xts_sentiment_neg)
btc_ts$SVM_sentiment_neg_amp <- as.ts(xts_sentiment_neg_amp)
btc_ts$SVM_sentiment_npnr <- as.ts(xts_sentiment_npnr)
btc_ts$SVM_sentiment_npr <- as.ts(xts_sentiment_npr)
btc_ts$SVM_sentiment_npr_alt <- as.ts(xts_sentiment_npr_alt)
btc_ts$SVM_sentiment_npr_amp <- as.ts(xts_sentiment_npr_amp)
btc_ts$SVM_sentiment_pos <- as.ts(xts_sentiment_pos)
btc_ts$SVM_sentiment_pos_amp <- as.ts(xts_sentiment_pos_amp)
btc_ts$SVM_sentiment_raw <- as.ts(xts_sentiment_raw)
btc_ts$SVM_sentiment_sum_pos_neg <- as.ts(xts_sentiment_sum_pos_neg)

#add tweet coun
btc_ts$TwtCnt <- as.ts(xts_TwtCnt)

#add RF and best three lexicons
#... ?best three lexicons and both ML?

##################################################################################################

results <- data.frame(
  ME = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MPE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  ACF1 = numeric(),
  AIC = numeric(),
  AICc = numeric(),
  BIC= numeric()
)

#do loop over all btc ts
for (i in 2:ncol(btc_ts)) {
  print(colnames(btc_ts)[i])
  fit.btcPrice <- auto.arima(xts_BtcPrice[2:133],xreg=btc_ts[2:133,c(i)])
  newrow <- data.frame(ME = summary(fit.btcPrice)[,'ME'],
                       RMSE = summary(fit.btcPrice)[,'RMSE'],
                       MAE = summary(fit.btcPrice)[,'MAE'],
                       MPE = summary(fit.btcPrice)[,'MPE'],
                       MAPE = summary(fit.btcPrice)[,'MAPE'],
                       MASE = summary(fit.btcPrice)[,'MASE'],
                       ACF1 = summary(fit.btcPrice)[,'ACF1'],
                       AIC = fit.btcPrice[["aic"]],
                       AICC = fit.btcPrice[["aicc"]],
                       BIC = fit.btcPrice[["bic"]])
  rownames(newrow) <- colnames(btc_ts)[i]
  results <- rbind(results,newrow)
  #rownames(results)[i] <- colnames(btc_ts)[i]
  #... print error measures and information criteria
}

#do loop over all btc ts lagged for one day
#...

results_lag1 <- data.frame(
  ME = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MPE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  ACF1 = numeric(),
  AIC = numeric(),
  AICc = numeric(),
  BIC= numeric()
)

for (i in 2:ncol(btc_ts)) {
  print(colnames(btc_ts)[i])
  cur_xts <- xts(btc_ts[,c(i)], order.by = as.Date(btc_ts[,1]))
  cur_xts <- stats::lag(cur_xts,1)
  fit.btcPrice <- auto.arima(xts_BtcPrice[2:133],xreg=cur_xts[2:133])
  newrow <- data.frame(ME = summary(fit.btcPrice)[,'ME'],
                       RMSE = summary(fit.btcPrice)[,'RMSE'],
                       MAE = summary(fit.btcPrice)[,'MAE'],
                       MPE = summary(fit.btcPrice)[,'MPE'],
                       MAPE = summary(fit.btcPrice)[,'MAPE'],
                       MASE = summary(fit.btcPrice)[,'MASE'],
                       ACF1 = summary(fit.btcPrice)[,'ACF1'],
                       AIC = fit.btcPrice[["aic"]],
                       AICC = fit.btcPrice[["aicc"]],
                       BIC = fit.btcPrice[["bic"]])
  rownames(newrow) <- colnames(btc_ts)[i]
  results_lag1 <- rbind(results_lag1,newrow)
  #rownames(results)[i] <- colnames(btc_ts)[i]
  #... print error measures and information criteria
}

results_lag2 <- data.frame(
  ME = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  MPE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  ACF1 = numeric(),
  AIC = numeric(),
  AICc = numeric(),
  BIC= numeric()
)

for (i in 2:ncol(btc_ts)) {
  print(colnames(btc_ts)[i])
  cur_xts <- xts(btc_ts[,c(i)], order.by = as.Date(btc_ts[,1]))
  cur_xts <- stats::lag(cur_xts,2)
  fit.btcPrice <- auto.arima(xts_BtcPrice[3:133],xreg=cur_xts[3:133])
  newrow <- data.frame(ME = summary(fit.btcPrice)[,'ME'],
                       RMSE = summary(fit.btcPrice)[,'RMSE'],
                       MAE = summary(fit.btcPrice)[,'MAE'],
                       MPE = summary(fit.btcPrice)[,'MPE'],
                       MAPE = summary(fit.btcPrice)[,'MAPE'],
                       MASE = summary(fit.btcPrice)[,'MASE'],
                       ACF1 = summary(fit.btcPrice)[,'ACF1'],
                       AIC = fit.btcPrice[["aic"]],
                       AICC = fit.btcPrice[["aicc"]],
                       BIC = fit.btcPrice[["bic"]])
  rownames(newrow) <- colnames(btc_ts)[i]
  results_lag2<- rbind(results_lag2,newrow)
  #rownames(results)[i] <- colnames(btc_ts)[i]
  #... print error measures and information criteria
}



cur_1 <- xts(btc_ts[,"SVM_sentiment_neg"], order.by = as.Date(btc_ts[,1]))
cur_1 <- stats::lag(cur_1,-1)
cur_2 <- xts(btc_ts[,"SVM_sentiment_npr"], order.by = as.Date(btc_ts[,1]))

fit.btcPrice <- auto.arima(xts_BtcPrice,xreg=data.frame(cur_1,cur_2))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))


cur_1 <- xts(btc_ts[,"SVM_sentiment_neg"], order.by = as.Date(btc_ts[,1]))
cur_1 <- stats::lag(cur_1,-1)
cur_2 <- xts(btc_ts[,"stockpulse_buzz"], order.by = as.Date(btc_ts[,1]))
cur_2 <- stats::lag(cur_2,-1)
cur_3 <- xts(btc_ts[,"Transactions_per_Block"], order.by = as.Date(btc_ts[,1]))
cur_3 <- stats::lag(cur_3,-1)
cur_4 <- xts(btc_ts[,"Transaction_Value"], order.by = as.Date(btc_ts[,1]))
cur_4 <- stats::lag(cur_4,-1)
cur_5 <- xts(btc_ts[,"Transaction_Fees"], order.by = as.Date(btc_ts[,1]))
cur_5 <- stats::lag(cur_5,-1)
cur_6 <- xts(btc_ts[,"Block_Interval"], order.by = as.Date(btc_ts[,1]))
cur_6 <- stats::lag(cur_6,-1)
cur_7 <- xts(btc_ts[,"Daily_Blocks"], order.by = as.Date(btc_ts[,1]))
cur_7 <- stats::lag(cur_7,-1)
#getting worse
cur_8 <- xts(btc_ts[,"Daily_Transactions"], order.by = as.Date(btc_ts[,1]))
cur_8 <- stats::lag(cur_8,-1)
cur_9 <- xts(btc_ts[,"longs"], order.by = as.Date(btc_ts[,1]))
cur_9 <- stats::lag(cur_9,-1)


cur_11 <- xts(btc_ts[,"shorts"], order.by = as.Date(btc_ts[,1]))
cur_12 <- xts(btc_ts[,"SVM_sentiment_raw"], order.by = as.Date(btc_ts[,1]))
cur_13 <- xts(btc_ts[,"SVM_sentiment_npr_alt"], order.by = as.Date(btc_ts[,1]))

cur_14 <- xts(btc_ts[,"SVM_sentiment_npr"], order.by = as.Date(btc_ts[,1]))
cur_14 <- stats::lag(cur_14,1)
cur_15 <- xts(btc_ts[,"Avg__UTXO_Value"], order.by = as.Date(btc_ts[,1]))
cur_15 <- stats::lag(cur_15,1)
cur_16 <- xts(btc_ts[,"Metcalfe_s_Law___UTXO"], order.by = as.Date(btc_ts[,1]))
cur_16 <- stats::lag(cur_16,1)

cur_21 <- xts(btc_ts[,"sp500"], order.by = as.Date(btc_ts[,1]))
cur_21  <- stats::lag(cur_21 ,-2)
cur_22 <- xts(btc_ts[,"stockpulse_buzz"], order.by = as.Date(btc_ts[,1]))
cur_22  <- stats::lag(cur_22 ,-2)
cur_23 <- xts(btc_ts[,"SVM_sentiment_npr"], order.by = as.Date(btc_ts[,1]))
cur_23  <- stats::lag(cur_23 ,-2)
#cur_21 <- xts(btc_ts[,"sp500"], order.by = as.Date(btc_ts[,1]))

cur_30 <- xts(btc_ts[,"SVM_sentiment_npr"], order.by = as.Date(btc_ts[,1]))
cur_32 <- xts(btc_ts[,"shorts"], order.by = as.Date(btc_ts[,1]))
cur_33 <- xts(btc_ts[,"Daily_Transactions"], order.by = as.Date(btc_ts[,1]))
cur_34 <- xts(btc_ts[,"Annual_Hash_Growth"], order.by = as.Date(btc_ts[,1]))
cur_35 <- xts(btc_ts[,"longs"], order.by = as.Date(btc_ts[,1]))

cur_31 <- xts(btc_ts[,"sp500"], order.by = as.Date(btc_ts[,1]))
cur_31 <- stats::lag(cur_31,1)
cur_36 <- xts(btc_ts[,"Block_Interval"], order.by = as.Date(btc_ts[,1]))
cur_36 <- stats::lag(cur_36,1)
cur_37 <- xts(btc_ts[,"longs"], order.by = as.Date(btc_ts[,1]))
cur_37 <- stats::lag(cur_37,1)
cur_38 <- xts(btc_ts[,"Daily_Transactions"], order.by = as.Date(btc_ts[,1]))
cur_38 <- stats::lag(cur_38,1)
cur_39 <- xts(btc_ts[,"stockpulse_buzz"], order.by = as.Date(btc_ts[,1]))
cur_39 <- stats::lag(cur_39,1)
cur_40 <- xts(btc_ts[,"TwtCnt"], order.by = as.Date(btc_ts[,1]))
cur_40 <- stats::lag(cur_40,1)
cur_41 <- xts(btc_ts[,"Transaction_Size"], order.by = as.Date(btc_ts[,1]))
cur_41 <- stats::lag(cur_41,1)

cur_42 <- xts(btc_ts[,"sp500"], order.by = as.Date(btc_ts[,1]))
cur_42 <- stats::lag(cur_42,2)
cur_43 <- xts(btc_ts[,"longs"], order.by = as.Date(btc_ts[,1]))
cur_43 <- stats::lag(cur_43,2)
cur_44 <- xts(btc_ts[,"Metcalfe_s_Law___UTXO"], order.by = as.Date(btc_ts[,1]))
cur_44 <- stats::lag(cur_44,2)
cur_45 <- xts(btc_ts[,"stockpulse_sent"], order.by = as.Date(btc_ts[,1]))
cur_45 <- stats::lag(cur_45,2)
cur_46 <- xts(btc_ts[,"Transaction_Fees"], order.by = as.Date(btc_ts[,1]))
cur_46 <- stats::lag(cur_46,2)
cur_47 <- xts(btc_ts[,"Velocity_of_Money"], order.by = as.Date(btc_ts[,1]))
cur_47 <- stats::lag(cur_47,2)
cur_48 <- xts(btc_ts[,"Money_Supply"], order.by = as.Date(btc_ts[,1]))
cur_48 <- stats::lag(cur_48,2)


fit.btcPrice <- auto.arima(xts_BtcPrice[3:133])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice[3:133], order=c(2,1,3))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))


#best model
fit.btcPrice.best <- arima(xts_BtcPrice[3:133], order=c(2,1,3), xreg=data.frame(cur_30[3:133],
                                                                                #cur_31[3:133],
                                                                                cur_32[3:133],
                                                                                cur_33[3:133],
                                                                                #cur_34[2:133],
                                                                                cur_35[3:133],
                                                                                cur_36[3:133],
                                                                                #cur_37[2:133],
                                                                                #cur_38[2:133],
                                                                                cur_39[3:133],
                                                                                cur_40[3:133],
                                                                                #cur_41[2:133],
                                                                                cur_42[3:133],
                                                                                cur_43[3:133],
                                                                                cur_44[3:133],
                                                                                cur_45[3:133],
                                                                                cur_46[3:133],
                                                                                cur_47[3:133],
                                                                                cur_48[3:133]
                                                                                ))
summary(fit.btcPrice.best)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice.best)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))










fit.btcPrice <- auto.arima(xts_BtcPrice[2:133])
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

fit.btcPrice <- arima(xts_BtcPrice[2:133], order=c(2,1,3))
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

#best model
fit.btcPrice.best <- arima(xts_BtcPrice[2:133], order=c(2,1,3), xreg=data.frame(cur_30[2:133],
                                                                                #cur_31[2:133],
                                                                                cur_32[2:133],
                                                                                cur_33[2:133],
                                                                                #cur_34[2:133],
                                                                                cur_35[2:133],
                                                                                cur_36[2:133],
                                                                                #cur_37[2:133],
                                                                                #cur_38[2:133],
                                                                                cur_39[2:133],
                                                                                cur_40[2:133]
                                                                                #,cur_41[2:133]
))
summary(fit.btcPrice.best)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice.best)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))












for (i in 20:0) {
  fit.btcPrice <- auto.arima(xts_BtcPrice[2:(133-i)])
  summary(fit.btcPrice)
  fc <- forecast(fit.btcPrice, h=1)
  print(fc)
}


for (i in 20:0) {
  fit.btcPrice.best <- arima(xts_BtcPrice[2:(133-i)],  order=c(2,1,3), xreg=data.frame(cur_30[2:(133-i)],
                                                                                       cur_32[2:(133-i)],
                                                                                       cur_33[2:(133-i)],
                                                                                       cur_35[2:(133-i)],
                                                                                       cur_36[2:(133-i)],
                                                                                       cur_39[2:(133-i)],
                                                                                       cur_40[2:(133-i)]))
  
  fc_30 <- forecast(auto.arima(cur_30[2:(133-i)]), h=1)
  fc_32 <- forecast(auto.arima(cur_32[2:(133-i)]), h=1)
  fc_33 <- forecast(auto.arima(cur_33[2:(133-i)]), h=1)
  fc_35 <- forecast(auto.arima(cur_35[2:(133-i)]), h=1)
  
  #lag backwards
  fc_36 <- xts(btc_ts[,"Block_Interval"], order.by = as.Date(btc_ts[,1]))
  fc_36 <- fc_36[133-i]
  fc_39 <- xts(btc_ts[,"stockpulse_buzz"], order.by = as.Date(btc_ts[,1]))
  fc_39 <- fc_39[133-i]
  fc_40 <- xts(btc_ts[,"TwtCnt"], order.by = as.Date(btc_ts[,1]))
  fc_40 <- fc_40[133-i]
  
  fc <- forecast(fit.btcPrice.best, h=1, xreg=data.frame(fc_30[[4]][1],
                                                         fc_32[[4]][1],
                                                         fc_33[[4]][1],
                                                         fc_35[[4]][1],
                                                         fc_36[[1]],
                                                         fc_39[[1]],
                                                         fc_40[[1]]))
  print(fc)
}



