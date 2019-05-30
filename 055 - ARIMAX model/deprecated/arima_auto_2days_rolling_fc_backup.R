#best model
fit.btcPrice.best <- Arima(xts_BtcPrice[2:133], lambda="auto", order=c(2,1,3), xreg=data.frame(cur_30[2:133],
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