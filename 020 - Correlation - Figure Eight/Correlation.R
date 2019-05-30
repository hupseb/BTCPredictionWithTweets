options(scipen=999)
Sys.setenv(TZ='UTC')

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

#get figure eight results
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')
query = sprintf("SELECT * FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw]")
feSent <- sqlQuery(conn, query, errors = TRUE, as.is=T)
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

feSent$sentiment_confidence <- as.numeric(feSent$sentiment_confidence)
feSent$created_at <- as.Date(feSent$created_at, "%Y-%m-%d")
feS <- feSent %>% mutate(sentiment_pos_c = ifelse(feSent$sentiment == "positive", 1*feSent$sentiment_confidence,0))
feS <- feS %>% mutate(sentiment_neg_c = ifelse(feSent$sentiment == "negative", -1*feSent$sentiment_confidence,0))
feS <- feS %>% mutate(sentiment_pos_amp = ifelse(feSent$sentiment == "positive", 1*(feSent$favorite_count),0))
feS <- feS %>% mutate(sentiment_neg_amp = ifelse(feSent$sentiment == "negative", -1*(feSent$favorite_count),0))
feS <- feS %>% mutate(sentiment_pos = ifelse(feSent$sentiment == "positive", 1,0))
feS <- feS %>% mutate(sentiment_neg = ifelse(feSent$sentiment == "negative", -1,0))
feS <- feS %>% mutate(sentiment_neu = ifelse(feSent$sentiment == "neutral", 1,0))

#remove NAs
feS[c("sentiment_neg_c")][is.na(feS[c("sentiment_neg_c")])] <- 0
feS[c("sentiment_pos_c")][is.na(feS[c("sentiment_pos_c")])] <- 0
feS[c("sentiment_pos_amp")][is.na(feS[c("sentiment_pos_amp")])] <- 0
feS[c("sentiment_neg_amp")][is.na(feS[c("sentiment_neg_amp")])] <- 0
feS[c("sentiment_pos")][is.na(feS[c("sentiment_pos")])] <- 0
feS[c("sentiment_neg")][is.na(feS[c("sentiment_neg")])] <- 0
feS[c("sentiment_neu")][is.na(feS[c("sentiment_neu")])] <- 0

#aggregate to daily
sentiment_neg_c <- aggregate(x = feS[c("sentiment_neg_c")],FUN = sum, by = list(date = feS$created_at))
sentiment_pos_c <- aggregate(x = feS[c("sentiment_pos_c")],FUN = sum, by = list(date = feS$created_at))
sentiment_pos_amp <- aggregate(x = feS[c("sentiment_pos_amp")],FUN = sum, by = list(date = feS$created_at))
sentiment_neg_amp <- aggregate(x = feS[c("sentiment_neg_amp")],FUN = sum, by = list(date = feS$created_at))
sentiment_neg <- aggregate(x = feS[c("sentiment_neg")],FUN = sum, by = list(date = feS$created_at))
sentiment_pos <- aggregate(x = feS[c("sentiment_pos")],FUN = sum, by = list(date = feS$created_at))
sentiment_neu <- aggregate(x = feS[c("sentiment_neu")],FUN = sum, by = list(date = feS$created_at))
sentiment_npr_amp <- sentiment_pos_amp
sentiment_npr_amp[,2] <- ((sentiment_pos_amp[,2]-(sentiment_neg_amp[,2]*-1))/(sentiment_pos_amp[,2]+(sentiment_neg_amp[,2]*-1)))
sentiment_npr <- sentiment_pos
sentiment_npr[,2] <- ((sentiment_pos[,2]-(sentiment_neg[,2]*-1))/(sentiment_pos[,2]+(sentiment_neg[,2]*-1)))
sentiment_nprcc <- sentiment_pos_c
sentiment_nprcc[,2] <- ((sentiment_pos_c[,2]-(sentiment_neg_c[,2]*-1))/(sentiment_pos_c[,2]+(sentiment_neg_c[,2]*-1)))
sentiment_npnr <- sentiment_neu
sentiment_npnr[,2] <- ((sentiment_pos[,2]-sentiment_neg[,2]*-1)/(sentiment_pos[,2]+sentiment_neg[,2]*-1+sentiment_neu[,2]))
sentiment_sum_pos_c_neg_c <- sentiment_pos_c
sentiment_sum_pos_c_neg_c[,2] <- (sentiment_pos_c[,2] + sentiment_neg_c[,2])
sentiment_sum_pos_neg <- sentiment_pos
sentiment_sum_pos_neg[,2] <- (sentiment_pos[,2] + sentiment_neg[,2])
sentiment_npr_alt <- sentiment_pos
sentiment_npr_alt[,2] <- ifelse(sentiment_pos[,2] < (sentiment_neg[,2]*-1), -1*((sentiment_neg[,2]*-1)/sentiment_pos[,2]),
                     ifelse(sentiment_pos[,2] > (sentiment_neg[,2]*-1), sentiment_pos[,2]/(sentiment_neg[,2]*-1), 0))

#prepare df objects for xts conversion
twtCnt[,1] <- as.Date(twtCnt[,1])
colnames(twtCnt) <- c("date","amount")
btcPriceDf <- data.frame(as.Date(btcPrice[,1]),as.numeric(btcPrice[,5])) 
colnames(btcPriceDf) <- c("date","Close")

#convert to xts
xts_TwtCnt <- xts(twtCnt[,2], order.by = as.Date(twtCnt[,1]))
xts_BtcPrice <- xts(btcPriceDf[,2], order.by = as.Date(btcPriceDf[,1]))
xts_sentiment_sum_pos_c_neg_c <- xts(sentiment_sum_pos_c_neg_c[,2], order.by = as.Date(sentiment_sum_pos_c_neg_c[,1]))
xts_sentiment_sum_pos_neg <- xts(sentiment_sum_pos_neg[,2], order.by = as.Date(sentiment_sum_pos_neg[,1]))
xts_sentiment_neg_c <- xts(sentiment_neg_c[,2], order.by = as.Date(sentiment_neg_c[,1]))
xts_sentiment_pos_c <- xts(sentiment_pos_c[,2], order.by = as.Date(sentiment_pos_c[,1]))
xts_sentiment_pos_amp <- xts(sentiment_pos_amp[,2], order.by = as.Date(sentiment_pos_amp[,1]))
xts_sentiment_neg_amp <- xts(sentiment_neg_amp[,2], order.by = as.Date(sentiment_neg_amp[,1]))
xts_sentiment_npr_amp <- xts(sentiment_npr_amp[,2], order.by = as.Date(sentiment_npr_amp[,1]))
xts_sentiment_npr <- xts(sentiment_npr[,2], order.by = as.Date(sentiment_npr[,1]))
xts_sentiment_nprcc <- xts(sentiment_nprcc[,2], order.by = as.Date(sentiment_nprcc[,1]))
xts_sentiment_npnr <- xts(sentiment_npnr[,2], order.by = as.Date(sentiment_npnr[,1]))
xts_sentiment_neg <- xts(sentiment_neg[,2], order.by = as.Date(sentiment_neg[,1]))
xts_sentiment_pos <- xts(sentiment_pos[,2], order.by = as.Date(sentiment_pos[,1]))
xts_sentiment_npr_alt <- xts(sentiment_npr_alt[,2], order.by = as.Date(sentiment_npr_alt[,1]))

#percentage change in btcPrice
xts_BtcPricePer <- (xts_BtcPrice/lag(xts_BtcPrice,-1) - 1)
xts_TwtCntPer <- (xts_TwtCnt/lag(xts_TwtCnt,-1) - 1)
xts_sentiment_sum_pos_c_neg_cPer <- (xts_sentiment_sum_pos_c_neg_c/lag(xts_sentiment_sum_pos_c_neg_c,1) - 1)
xts_sentiment_sum_pos_negPer <- (xts_sentiment_sum_pos_neg/lag(xts_sentiment_sum_pos_neg,1) - 1)
xts_sentiment_neg_cPer <- (xts_sentiment_neg_c/lag(xts_sentiment_neg_c,1) - 1)
xts_sentiment_pos_cPer <- (xts_sentiment_pos_c/lag(xts_sentiment_pos_c,1) - 1)
xts_sentiment_pos_ampPer <- (xts_sentiment_pos_amp/lag(xts_sentiment_pos_amp,1) - 1)
xts_sentiment_neg_ampPer <- (xts_sentiment_neg_amp/lag(xts_sentiment_neg_amp,1) - 1)
xts_sentiment_npr_ampPer <- (xts_sentiment_npr_amp/lag(xts_sentiment_npr_amp,1) - 1)
xts_sentiment_nprPer <- (xts_sentiment_npr/lag(xts_sentiment_npr,1) - 1)
xts_sentiment_nprccPer <- (xts_sentiment_nprcc/lag(xts_sentiment_nprcc,1) - 1)
xts_sentiment_npnrPer <- (xts_sentiment_npnr/lag(xts_sentiment_npnr,1) - 1)
xts_sentiment_nPer <- (xts_sentiment_neg/lag(xts_sentiment_neg,1) - 1)
xts_sentiment_pPer <- (xts_sentiment_pos/lag(xts_sentiment_pos,1) - 1)

########################################
#amount of tweets at lag 1 the strongest
corrDf <- data.frame(xts_BtcPrice, xts_TwtCnt)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, lag(xts_TwtCnt,1))
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, lag(xts_TwtCnt,2))
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, lag(xts_TwtCnt,3))
corrDf %>% ggpairs()
corrDf <- data.frame(xts_BtcPrice, lag(xts_TwtCnt,4))
corrDf %>% ggpairs()
#corrDf <- data.frame(log(xts_BtcPrice), log(xts_TwtCnt))
#corrDf %>% ggpairs() 
#corrDf <- data.frame(log(xts_BtcPrice), log(lag(xts_TwtCnt,1)))
#corrDf %>% ggpairs() 

#frequency is weekly in xts_TwtCnt
ts_twtCnt = ts(twtCnt[,2], frequency = 7)
decompose_twtCnt = decompose(ts_twtCnt, "additive")
#plot(as.ts(decompose_twtCnt$seasonal))
#plot(as.ts(decompose_twtCnt$trend))
#plot(as.ts(decompose_twtCnt$random))
plot(decompose_twtCnt)
autoplot(as.ts(decompose_twtCnt$trend) + as.ts(decompose_twtCnt$random))
autoplot(ts_twtCnt)
autoplot(decompose_twtCnt$trend)

View(as.ts(decompose_twtCnt$trend) + as.ts(decompose_twtCnt$random))

###########################################################################
#strong relation between trend of tweet amount and BTC/USD
###########################################################################
corrDf <- data.frame(merge(xts_BtcPrice,decompose_twtCnt$trend))
colnames(corrDf)[1] <- "BTC/USD"
colnames(corrDf)[2] <- "Twt_dcmp_trnd"
ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))

corrDf <- data.frame(merge(xts_BtcPrice,decompose_twtCnt$trend + decompose_twtCnt$random))
ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))
corrDf <- data.frame(merge(diff(xts_BtcPrice),decompose_twtCnt$random))
ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))

#Create moving average of Bitcoin price and compare it to decomposed trend
dates <- seq(as.Date("2018-01-28"),length=133,by="days")
btcprice_ma <- ma(xts_BtcPrice, order = 6, centre = T)
btcprice_ma <- xts(btcprice_ma, order.by = as.Date(btcPriceDf[,1]))
autoplot(btcprice_ma)
autoplot(xts_BtcPrice)
xts_decompose_twtCnt <- xts(decompose_twtCnt$trend,order.by=dates)
corrDf <- data.frame(merge((btcprice_ma),xts_decompose_twtCnt))
corrDf %>% ggpairs() 
#corrDf <- data.frame(merge((btcprice_ma),xts_BtcPrice))
#corrDf %>% ggpairs() 
stl_ts_twtCnt <- stl(ts_twtCnt, s.window = 7, s.degree=0)
xts_stl_ts_twtCnt <- xts(stl_ts_twtCnt$time.series[,2] + stl_ts_twtCnt$time.series[,3],order.by=dates)
corrDf <- data.frame(merge(xts_BtcPrice,xts_stl_ts_twtCnt))
corrDf %>% ggpairs() 
corrDf <- data.frame(merge(btcprice_ma,xts_stl_ts_twtCnt))
corrDf %>% ggpairs() 
xts_stl_ts_twtCnt <- xts(stl_ts_twtCnt$time.series[,2],order.by=dates)
corrDf <- data.frame(merge(xts_BtcPrice,xts_stl_ts_twtCnt))
corrDf %>% ggpairs() 
corrDf <- data.frame(merge(btcprice_ma,xts_stl_ts_twtCnt))
corrDf %>% ggpairs() 

#####################################################################################
#####################################################################################

#weak correlation between percentage change of Bitcoin price and the ratio of sentiment
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_neg)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_neg_amp)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPricePer, xts_sentiment_neg_ampPer)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_sum_pos_neg)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_sum_pos_c_neg_c)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npr)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_nprcc)
corrDf %>% ggpairs() 
corrDf <- data.frame(xts_BtcPrice, xts_sentiment_pos)
corrDf %>% ggpairs() 


corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_neg*-1), diff(xts_sentiment_pos))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_neg_c*-1), diff(xts_sentiment_pos_c))
corrDf %>% ggpairs()
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npr))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_npnr))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_sum_pos_neg))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_sum_pos_c_neg_c))
corrDf %>% ggpairs() 
corrDf <- data.frame(diff(xts_BtcPrice), diff(xts_sentiment_nprcc))
corrDf %>% ggpairs() 

autoplot(xts_sentiment_sum_pos_c_neg_c)
autoplot(diff(xts_sentiment_sum_pos_c_neg_c))
autoplot(xts_sentiment_sum_pos_neg)
autoplot(diff(xts_BtcPrice))
autoplot(diff(xts_sentiment_sum_pos_c_neg_c))


geom_bar(data = xts_sentiment_sum_pos_c_neg_c, stat = "count",
         position = "stack", ..., width = NULL, binwidth = NULL,
         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

update_geom_defaults('bar', list(colour = NULL, size = 0))
df1 <-  data.frame(merge(xts_BtcPrice,xts_sentiment_sum_pos_c_neg_c))
df1 <- cbind(df1, rownames(df1))
rownames(df1) <- NULL
colnames(df1) <- c("BTC_Price", "Sentiment", "Date")
p <- ggplot(df1, aes(x=Date, group = 1))
p <- p + geom_line(aes(y=(BTC_Price)-8686.142), colour="blue", size=1.0)
p <- p + geom_point(aes(y=(BTC_Price)-8686.142), colour="blue")
p <- p + geom_bar(aes(y=((Sentiment)-17.62139)*80), stat="identity", width=0.3, colour="red", fill="red", alpha=0.7)
p <- p + scale_y_continuous(sec.axis = sec_axis(~./2000*60, name = "Sentiment of Tweets"))
p




ggpairs(corrDf, lower = list(continuous = wrap(corrPltFun, pts=list(colour="black"), smt=list(method="lm", colour="blue"))))

ccf(as.ts(diff(xts_BtcPrice)[-1]), as.ts(diff(xts_sentiment_sum_pos_c_neg_c)[-1]))

corrDf <- data.frame(xts_BtcPrice, xts_sentiment_npnr)
corrDf %>% ggpairs() 












#tslm model
merged <- merge(xts_BtcPricePer,xts_sentiment_posPer)
merged <- merge(merged,xts_sentiment_negPer)
merged <- merge(merged,xts_TwtCntPer)
merged <- as.ts(merged)
fit.btcPrice <- tslm (xts_BtcPricePer ~ xts_sentiment_posPer + xts_sentiment_negPer + xts_TwtCntPer, data=merged)
summary(fit.btcPrice)

#reject H0 if p-value is smaller than 0.05
#H0 is that there is no serial correlation of any order up to p.
checkresiduals(fit.btcPrice)

#autoplot fit
autoplot(merged[,"xts_BtcPricePer"], series="Data") +
  autolayer(fitted(fit.btcPrice), series="Fitted") +
  xlab("day") + ylab(" ") +
  ggtitle("Daily Bitcoin Close Price on Bitfinex") +
  guides(colour=guide_legend(title=" "))

#spline
xts_BtcPrice %>% splinef(lambda=0) %>% autoplot()
