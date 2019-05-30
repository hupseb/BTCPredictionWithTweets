btcPriceDf_high <- data.frame(as.Date(btcPrice[,1]),as.numeric(btcPrice[,3])) 
colnames(btcPriceDf_high) <- c("date","Close")
xts_BtcPrice_high <- xts(btcPriceDf_high[,2], order.by = as.Date(btcPriceDf_high[,1]))


refit <- Arima(xts_BtcPrice_high[2:5], model=fit.btcPrice.best, xreg=data.frame(diff(cur_31)[2:5],
                                                                                diff(cur_35)[2:5])) 

summary(refit)
autoplot(as.ts(xts_BtcPrice_high[2:5])) +
  autolayer(fitted(refit)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

forecast(refit, h=1, xreg=data.frame(diff(cur_31)[6],
                                     diff(cur_35)[6]))

xts_BtcPrice_high[6]







naked <- Arima(xts_BtcPrice_high[2:5], order=c(2,1,2)) 

summary(naked)
autoplot(as.ts(xts_BtcPrice_high[2:5])) +
  autolayer(fitted(naked)) +
  xlab("day") + ylab("") +
  ggtitle("Bitcoin price change") +
  guides(colour=guide_legend(title=" "))

forecast(naked, h=1)
xts_BtcPrice_high[6]