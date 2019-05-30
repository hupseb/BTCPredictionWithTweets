options(scipen=999)
Sys.setenv(TZ='UTC')

library("devtools")
#use version 8.4
devtools::install_github("robjhyndman/forecast", force = TRUE, ref="0fdb430")
library("forecast")

install.packages("GGally")
install.packages("fpp2")
install.packages("gridExtra")

library("mgcv") 
library("RODBC")
library("httpuv")
library("xts")
library("plyr")
library("GGally")
library("fpp2")
library("stringi")
library("gridExtra")
library("timeDate")

#get SVM results for OOS
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
btcTwts = sqlQuery(conn, paste("SELECT A.[status_id]
                               ,A.[decisionValue] AS sentiment
                               ,B.[user_id]
                               ,B.[created_at]
                               ,B.favorite_count
                               ,B.retweet_count
                               FROM [BitcoinTwts].[dbo].[Final_SVM_sentiment_OOS] AS A
                               INNER JOIN [BitcoinTwts].[dbo].[tmp_BitcoinTweetsUnion_Filtered_OOSInterval] AS B ON A.[status_id] = B.[status_id] 
                               ORDER BY B.[created_at]"), as.is = TRUE)
odbcClose(conn)

#get bitcoin price for OOS
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("SELECT * FROM [BitcoinAnalysis].[dbo].[01. Bitfinex_C_TradingView_d]
                WHERE ([timestamp] >= CONVERT(datetime2, '2018-06-10 00:00:00.000')) AND
                ([timestamp] <  CONVERT(datetime2, '2018-10-21 00:00:00.000'))
                ORDER BY [timestamp]")
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
xts_BtcPrice <- xts(btcPriceDf[,2], order.by = as.Date(btcPriceDf[,1]))

#get other timeseries for bitcoin
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("/****** Script for SelectTopNRows command from SSMS  ******/
                SELECT B.[timestamp]
                ,A.[close] as sp500
                ,B.[close] as shorts
                FROM [BitcoinAnalysis].[dbo].[09. S&P500_d] AS A
                INNER JOIN [BitcoinAnalysis].[dbo].[07.1 Bitfinex_Shorts_d] AS B
                ON A.timestamp = B.timestamp
                WHERE (B.[timestamp] >= CONVERT(datetime2, '2018-06-10 00:00:00.000')) AND
                (B.[timestamp] <  CONVERT(datetime2, '2018-10-21 00:00:00.000'))
                ORDER BY B.[timestamp]")
btc_mutual_ts <- sqlQuery(conn, query, errors = TRUE, as.is=T)
odbcClose(conn)

#adjustments
btc_mutual_ts[,c(2,3)] <- sapply(btc_mutual_ts[,c(2,3)], as.numeric)

#add to btc_ts
btc_ts <- btc_mutual_ts

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

##################################################################################################
fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], stepwise=FALSE, approx=FALSE, trace=TRUE, d=2)
summary(fit.btcPrice)
autoplot(as.ts(xts_BtcPrice)) +
  autolayer(fitted(fit.btcPrice)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

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
  print("------------------------")
  print(colnames(btc_ts)[i])
  print("------------------------")
  #fit.btcPrice <- auto.arima(xts_BtcPrice[2:133], stepwise=FALSE, approx=FALSE, xreg=diff(btc_ts[1:133,c(i)]))
  fit.btcPrice <- Arima(xts_BtcPrice[2:133], order=c(1,2,1), xreg=btc_ts[2:133,c(i)])
  summ <- summary(fit.btcPrice)
  newrow <- data.frame(ME = summ[,'ME'],
                       RMSE = summ[,'RMSE'],
                       MAE = summ[,'MAE'],
                       MPE = summ[,'MPE'],
                       MAPE = summ[,'MAPE'],
                       MASE = summ[,'MASE'],
                       ACF1 = summ[,'ACF1'],
                       AIC = fit.btcPrice[["aic"]],
                       AICC = fit.btcPrice[["aicc"]],
                       BIC = fit.btcPrice[["bic"]])
  rownames(newrow) <- colnames(btc_ts)[i]
  results <- rbind(results,newrow)
}

##################################################################################################

cur_33 <- xts(btc_ts[,"SVM_sentiment_raw"], order.by = as.Date(btc_ts[,1]))
cur_36 <- xts(btc_ts[,"shorts"], order.by = as.Date(btc_ts[,1]))

#create regression sets
regsets <- list(NULL,
                data.frame(
                  cur_33
                ))
names(regsets) <- c( "No Regressors", "Best Twitter Sentiment")

#add more regression sets  
regsets[["Best Technical Predictors"]] <- data.frame(
  cur_36   
)

regsets[["Best Technical Predictors + Best Twitter Sentiment"]] <- data.frame(
  cur_33,
  cur_36
)

#diff_constant is one
dc <- 1
n <- 1
profit_dfs <- list();
profit_res <- data.frame(profit_dfs_nr=integer(),
                         profit=double(),
                         arima_size=integer(),
                         forecast_days=integer(),
                         window_week_start_day=integer(),
                         first_forecast_date=character(),
                         last_forecast_date=character(),
                         twitter_sent_regs=logical(),
                         btc_tec_regs=logical())

for(i in seq(50, 120, by = 3)) {
  for(j in seq(1, 7, by = 1)) {
    for(k in seq(1, length(regsets), by = 1)) {
      regsetname <- names(regsets[k])
      regset <- regsets[[regsetname]]
      
      #do rolling forecast for 20 days
      data <- seq(0,0,length.out=20)
      dates <- seq(as.Date(index(xts_BtcPrice[(j+dc+i)-20+1,])),length=20,by="days")
      fc_xts <- xts(x=data, order.by=dates)
      
      for (l in 20:1) {
        start <- j+dc+(20-l)
        end <- start+(i-20)
        
        #keep start fix instead of keeping ARIMA size fix
        #ARIMA will grow in total 20 days
        #start day j will be still be respected
        #start <- j+dc
        
        if(!is.null(regset)){
          fit.btcPrice.best <- Arima(xts_BtcPrice[start:end], order=c(1,2,1), xreg=regset[start:end,])
          #fit.btcPrice.best <- auto.arima(xts_BtcPrice[start:end], xreg=regset[start:end,])
        }else{
          fit.btcPrice.best <- Arima(xts_BtcPrice[start:end], order=c(1,2,1))
          #fit.btcPrice.best <- auto.arima(xts_BtcPrice[start:end], stepwise=FALSE, approx=FALSE)
        }
        
        #forecast each regressor variable
        if(!is.null(regset)){
          regset_fcs <- list()
          for(m in seq(1, ncol(regset), by = 1)) {
            reg <- regset[,m]
            #print(paste0("forecast for regressor: ", names(regset[m])))
            fc_reg <- forecast(auto.arima(reg[start:end],stepwise=FALSE, approx=FALSE), h=1)
            #print(fc_reg[["mean"]][1])
            
            #save forecast of regressors variables to dataframe
            regset_fcs[[names(regset[m])]] <- fc_reg[["mean"]][1]
          }
        }
        
        #retrieve current day forecast with dataframe
        if(!is.null(regset)){
          fc <- forecast(fit.btcPrice.best, h=1, xreg=as.data.frame(regset_fcs))
        }else{
          fc <- forecast(fit.btcPrice.best, h=1)
        }
        
        #add to timeseries for all 20 forecasts
        fc_day <- index(xts_BtcPrice[end + 1,])
        fc_xts[fc_day] <- fc[["mean"]][1]
        
        print("---------------------------------------------------------------")
        print(paste0("regset name: ", regsetname))   
        print(paste0("window size: ", i))
        print(paste0("week day:    ", j))
        print(paste0("ARIMA start: ", start))
        print(paste0("ARIMA end  : ", end))
        start_str <- paste0(index(xts_BtcPrice[start,]), " ARIMA start")
        end_str <- paste0(index(xts_BtcPrice[end,]), " ARIMA end")
        print(start_str)
        print(end_str)
        fc_day_str <- paste0(fc_day, " forecast: ")
        print(paste0(fc_day_str, fc[["mean"]][1]))
        print(dates)
        print("---------------------------------------------------------------")
      }
      
      #calculate profit like in excel sheet
      btc_prices <- xts_BtcPrice[index(fc_xts)]
      xts_BtcPrice_lagged <- stats::lag(xts_BtcPrice,1)
      btc_prices_lag1 <- xts_BtcPrice_lagged[index(fc_xts)]
      
      fc_differences <- fc_xts - btc_prices_lag1
      btc_lastday_differences <- btc_prices - btc_prices_lag1
      
      fc_differences_smaller_0 <- fc_differences < 0
      btc_lastday_differences_smaller_0 <- btc_lastday_differences < 0
      direction_ok <- fc_differences_smaller_0 == btc_lastday_differences_smaller_0
      win <- abs(btc_lastday_differences) * direction_ok
      no_win <- win == 0
      lose <- abs(btc_lastday_differences) * no_win
      
      profit_df <- merge(btc_prices,
                         btc_prices_lag1,
                         fc_xts,fc_differences,
                         btc_lastday_differences,
                         fc_differences_smaller_0,
                         btc_lastday_differences_smaller_0,
                         direction_ok,
                         win,
                         lose)
      
      #save profit result in table for ANOVA
      profit_res <- rbind(profit_res, data.frame(profit_dfs_nr=n,
                                                 profit=sum(profit_df$win) - sum(profit_df$lose),
                                                 arima_size=i-20,
                                                 forecast_days=20,
                                                 window_week_start_day=j,
                                                 first_forecast_date=as.character(index(first(profit_df))),
                                                 last_forecast_date=as.character(index(last(profit_df))),
                                                 twitter_sent_regs=regsetname == "Best Twitter Sentiment" || regsetname == "Best Technical Predictors + Best Twitter Sentiment",
                                                 btc_tec_regs=regsetname == "Best Technical Predictors" || regsetname == "Best Technical Predictors + Best Twitter Sentiment"))
      
      profit_dfs[[n]] <- profit_df
      n <- n + 1
    }
  }
}

write.csv(profit_res, file = "1_2_1_rolling_without_xreg_diff_3_final_OOS.csv",row.names=FALSE)

anovaData <- profit_res

#columns as factors
anovaData$arima_size <- as.factor(anovaData$arima_size)
anovaData$window_week_start_day <- as.factor(anovaData$window_week_start_day)
anovaData$forecast_days <- as.numeric(anovaData$forecast_days)

lmProfit = lm(anovaData$profit ~
                anovaData$arima_size +
                anovaData$window_week_start_day + anovaData$twitter_sent_regs +
                anovaData$btc_tec_regs
)

anova(lmProfit)
summary(lmProfit)
#plot(lmProfit)
#confint(lmProfit)


