options(scipen=999)
Sys.setenv(TZ='UTC')

#install.packages("httpuv")
#install.packages("devtools")
#install.packages("RODBC")
install.packages("GGally")
install.packages("fpp2")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("FinCal")
install.packages("reshape2")
library("reshape2")
library("FinCal")
library("ggplot2") 
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

#missing:
#volatility of Bitcoin
#inflation of Bitcoin

mean(btc_ts$Bitcoin_Core__BTC__Price)
sd(btc_ts$Bitcoin_Core__BTC__Price)
xts_Bitcoin_Core__BTC__Price <- xts(btc_ts[,"Bitcoin_Core__BTC__Price"], order.by = as.Date(btc_ts[,1]))
pl_Bitcoin_Core__BTC__Price <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Bitcoin_Core__BTC__Price)) +
  geom_line(color='blue') + theme(legend.position = "none") +
  labs(title = "Bitcoin Price in USD") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Market_Capitalization)/1000000
sd(btc_ts$Market_Capitalization)/1000000
xts_Market_Capitalization <- xts(btc_ts[,"Market_Capitalization"], order.by = as.Date(btc_ts[,1]))
pl_Market_Capitalization <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Market_Capitalization/1000000)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Market Capitalization in million USD") +
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Money_Supply)
sd(btc_ts$Money_Supply)
xts_Money_Supply <- xts(btc_ts[,"Money_Supply"], order.by = as.Date(btc_ts[,1]))
pl_Money_Supply <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Money_Supply)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Money Supply in BTC") +
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Chain_Value_Density)
sd(btc_ts$Chain_Value_Density)
xts_Chain_Value_Density <- xts(btc_ts[,"Chain_Value_Density"], order.by = as.Date(btc_ts[,1]))
pl_Chain_Value_Density <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Chain_Value_Density)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Chain Value Density in $/MB") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Daily_Transactions)
sd(btc_ts$Daily_Transactions)
xts_Daily_Transactions <- xts(btc_ts[,"Daily_Transactions"], order.by = as.Date(btc_ts[,1]))
pl_Daily_Transactions <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Daily_Transactions)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Daily Transactions") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Transaction_Value)
sd(btc_ts$Transaction_Value)
xts_Transaction_Value <- xts(btc_ts[,"Transaction_Value"], order.by = as.Date(btc_ts[,1]))
pl_Transaction_Value <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Transaction_Value)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Transaction Value in USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Total_Transactions)
sd(btc_ts$Total_Transactions)
xts_Total_Transactions <- xts(btc_ts[,"Total_Transactions"], order.by = as.Date(btc_ts[,1]))
pl_Total_Transactions <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Total_Transactions)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Total Transactions") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Transaction_Amount)
sd(btc_ts$Transaction_Amount)
xts_Transaction_Amount <- xts(btc_ts[,"Transaction_Amount"], order.by = as.Date(btc_ts[,1]))
pl_Transaction_Amount <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Transaction_Amount)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Transaction Amount in BTC") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

#1st page: Bitcoin Technical and Market Time Series
#BitcoinTimeSeriesCharts_8_27-9_352.pdf
options(digits=7)
grid.arrange(#pl_Market_Capitalization,
             #pl_Market_Capitalization,
             pl_Bitcoin_Core__BTC__Price,
             pl_Market_Capitalization,
             pl_Money_Supply,
             pl_Chain_Value_Density,
             pl_Daily_Transactions,
             pl_Transaction_Value,
             pl_Total_Transactions,
             pl_Transaction_Amount,             
             ncol=2)

mean(btc_ts$Transaction_Fees)
sd(btc_ts$Transaction_Fees)
xts_Transaction_Fees <- xts(btc_ts[,"Transaction_Fees"], order.by = as.Date(btc_ts[,1]))
pl_Transaction_Fees <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Transaction_Fees)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Transaction Fees in BTC") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Avg__UTXO_Amount)
sd(btc_ts$Avg__UTXO_Amount)
xts_Avg__UTXO_Amount <- xts(btc_ts[,"Avg__UTXO_Amount"], order.by = as.Date(btc_ts[,1]))
pl_Avg__UTXO_Amount <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Avg__UTXO_Amount)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Average UTXO Amount in BTC") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Avg__UTXO_Value)
sd(btc_ts$Avg__UTXO_Value)
xts_Avg__UTXO_Value <- xts(btc_ts[,"Avg__UTXO_Value"], order.by = as.Date(btc_ts[,1]))
pl_Avg__UTXO_Value <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Avg__UTXO_Value)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Average UTXO Value in USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Fee_Rate)
sd(btc_ts$Fee_Rate)
xts_Fee_Rate <- xts(btc_ts[,"Fee_Rate"], order.by = as.Date(btc_ts[,1]))
pl_Fee_Rate <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Fee_Rate)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Fee Rate in Satoshi/B") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Fee_Percentage)
sd(btc_ts$Fee_Percentage)
xts_Fee_Percentage <- xts(btc_ts[,"Fee_Percentage"], order.by = as.Date(btc_ts[,1]))
pl_Fee_Percentage <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Fee_Percentage)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Fee Percentage") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Fees_Value)
sd(btc_ts$Fees_Value)
Fees_Value <- xts(btc_ts[,"Fees_Value"], order.by = as.Date(btc_ts[,1]))
pl_Fees_Value <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Fees_Value)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Fees Value in USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Block_Size)/1024
sd(btc_ts$Block_Size)/1024
Block_Size <- xts(btc_ts[,"Block_Size"], order.by = as.Date(btc_ts[,1]))
pl_Block_Size <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Block_Size/1024)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Block Size in KB") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Block_Interval)/60
sd(btc_ts$Block_Interval)/60
Block_Interval <- xts(btc_ts[,"Block_Interval"], order.by = as.Date(btc_ts[,1]))
pl_Block_Interval <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Block_Interval/60)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Block Interval in Minutes") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Blockchain_Size)/1024/1024/1024
sd(btc_ts$Blockchain_Size)/1024/1024/1024
Blockchain_Size <- xts(btc_ts[,"Blockchain_Size"], order.by = as.Date(btc_ts[,1]))
pl_Blockchain_Size <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = (((Blockchain_Size/1024)/1024)/1024))) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Blockchain Size in GB") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Block_Height)
sd(btc_ts$Block_Height)
Block_Height <- xts(btc_ts[,"Block_Height"], order.by = as.Date(btc_ts[,1]))
pl_Block_Height <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Block_Height)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Blockchain Height in Blocks") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

#2nd page: Bitcoin Technical and Market Time Series
#BitcoinTimeSeriesCharts_2nd.pdf
options(digits=7)
grid.arrange(pl_Transaction_Fees,
             pl_Avg__UTXO_Amount,
             pl_Avg__UTXO_Value,
             pl_Fee_Rate,
             pl_Fee_Percentage,
             pl_Fees_Value,
             pl_Block_Size,
             pl_Block_Interval,
             pl_Blockchain_Size,
             pl_Block_Height,             
  ncol=2)


mean(btc_ts$Daily_Blocks)
sd(btc_ts$Daily_Blocks)
Daily_Blocks <- xts(btc_ts[,"Daily_Blocks"], order.by = as.Date(btc_ts[,1]))
pl_Daily_Blocks <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Daily_Blocks)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Daily Blocks") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Transactions_per_Block)
sd(btc_ts$Transactions_per_Block)
Transactions_per_Block <- xts(btc_ts[,"Transactions_per_Block"], order.by = as.Date(btc_ts[,1]))
pl_Transactions_per_Block <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Transactions_per_Block)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Transactions per Block") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Hash_Rate)/1000000
sd(btc_ts$Hash_Rate)/1000000
Hash_Rate <- xts(btc_ts[,"Hash_Rate"], order.by = as.Date(btc_ts[,1]))
pl_Hash_Rate <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Hash_Rate/1000000)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Hash Rate in Exahashes per second") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Difficulty)/1000000000
sd(btc_ts$Difficulty)/1000000000
Difficulty <- xts(btc_ts[,"Difficulty"], order.by = as.Date(btc_ts[,1]))
pl_Difficulty <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Difficulty/1000000000)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Difficulty in billion") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Miner_Revenue)
sd(btc_ts$Miner_Revenue)
Miner_Revenue <- xts(btc_ts[,"Miner_Revenue"], order.by = as.Date(btc_ts[,1]))
pl_Miner_Revenue <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Miner_Revenue)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Miner Revenue in BTC") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Miner_Revenue_Value)
sd(btc_ts$Miner_Revenue_Value)
Miner_Revenue_Value <- xts(btc_ts[,"Miner_Revenue_Value"], order.by = as.Date(btc_ts[,1]))
pl_Miner_Revenue_Value <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Miner_Revenue_Value)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Miner Revenue in USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Two_Week_Hash_Growth)
sd(btc_ts$Two_Week_Hash_Growth)
Two_Week_Hash_Growth <- xts(btc_ts[,"Two_Week_Hash_Growth"], order.by = as.Date(btc_ts[,1]))
pl_Two_Week_Hash_Growth <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Two_Week_Hash_Growth)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Hash Growth Biweekly in Percentage") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Quarterly_Hash_Growth)
sd(btc_ts$Quarterly_Hash_Growth)
Quarterly_Hash_Growth <- xts(btc_ts[,"Quarterly_Hash_Growth"], order.by = as.Date(btc_ts[,1]))
pl_Quarterly_Hash_Growth <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Quarterly_Hash_Growth)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Hash Growth Quarterly in Percentage") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Annual_Hash_Growth)
sd(btc_ts$Annual_Hash_Growth)
Annual_Hash_Growth <- xts(btc_ts[,"Annual_Hash_Growth"], order.by = as.Date(btc_ts[,1]))
pl_Annual_Hash_Growth <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Annual_Hash_Growth)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Hash Growth Annual in Percentage") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Metcalfe_s_Law___UTXO)
sd(btc_ts$Metcalfe_s_Law___UTXO)
Metcalfe_s_Law___UTXO <- xts(btc_ts[,"Metcalfe_s_Law___UTXO"], order.by = as.Date(btc_ts[,1]))
pl_Metcalfe_s_Law___UTXO <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Metcalfe_s_Law___UTXO)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Metcalfe's Law in UTXO") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

#3nd page: Bitcoin Technical and Market Time Series
#BitcoinTimeSeriesCharts_3nd.pdf
options(digits=7)
grid.arrange(pl_Daily_Blocks,
             pl_Transactions_per_Block,
             pl_Hash_Rate,
             pl_Difficulty,
             pl_Miner_Revenue,
             pl_Miner_Revenue_Value,
             pl_Two_Week_Hash_Growth,
             pl_Quarterly_Hash_Growth,
             pl_Annual_Hash_Growth,
             pl_Metcalfe_s_Law___UTXO,             
             ncol=2)

mean(btc_ts$Metcalfe_s_Law___TX)
sd(btc_ts$Metcalfe_s_Law___TX)
Metcalfe_s_Law___TX <- xts(btc_ts[,"Metcalfe_s_Law___TX"], order.by = as.Date(btc_ts[,1]))
pl_Metcalfe_s_Law___TX <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Metcalfe_s_Law___TX)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Metcalfe's Law in TX") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Velocity_of_Money)
sd(btc_ts$Velocity_of_Money)
Velocity_of_Money <- xts(btc_ts[,"Velocity_of_Money"], order.by = as.Date(btc_ts[,1]))
pl_Velocity_of_Money <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Velocity_of_Money)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Velocity of Money") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Velocity___Quarterly)
sd(btc_ts$Velocity___Quarterly)
Velocity___Quarterly <- xts(btc_ts[,"Velocity___Quarterly"], order.by = as.Date(btc_ts[,1]))
pl_Velocity___Quarterly <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Velocity___Quarterly)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Velocity of Money Quarterly") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Velocity___Daily)
sd(btc_ts$Velocity___Daily)
Velocity___Daily <- xts(btc_ts[,"Velocity___Daily"], order.by = as.Date(btc_ts[,1]))
pl_Velocity___Daily <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Velocity___Daily)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Velocity of Money Daily") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Transaction_Size)/1024
sd(btc_ts$Transaction_Size)/1024
Transaction_Size <- xts(btc_ts[,"Transaction_Size"], order.by = as.Date(btc_ts[,1]))
pl_Transaction_Size <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Transaction_Size/1024)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Transaction Size in KB") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Output_Volume)
sd(btc_ts$Output_Volume)
Output_Volume <- xts(btc_ts[,"Output_Volume"], order.by = as.Date(btc_ts[,1]))
pl_Output_Volume <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Output_Volume)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Output Volume in BTC") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$Output_Value)/1000000
sd(btc_ts$Output_Value)/1000000
Output_Value <- xts(btc_ts[,"Output_Value"], order.by = as.Date(btc_ts[,1]))
pl_Output_Value <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = Output_Value/1000000)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Output Value in millon USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$UTXO_Set_Size)/1000000
sd(btc_ts$UTXO_Set_Size)/1000000
UTXO_Set_Size <- xts(btc_ts[,"UTXO_Set_Size"], order.by = as.Date(btc_ts[,1]))
pl_UTXO_Set_Size <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = UTXO_Set_Size/1000000)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "UTXO Set Size") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_ts$UTXO_Growth)
sd(btc_ts$UTXO_Growth)
UTXO_Growth <- xts(btc_ts[,"UTXO_Growth"], order.by = as.Date(btc_ts[,1]))
pl_UTXO_Growth <- ggplot(btc_ts, aes(x = as.Date(btc_ts[,1]), y = UTXO_Growth)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "UTXO Growth") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))


#4nd page: Bitcoin Technical and Market Time Series
#BitcoinTimeSeriesCharts_4nd.pdf
options(digits=7)
grid.arrange(pl_Metcalfe_s_Law___TX,
             pl_Velocity_of_Money,
             pl_Velocity___Quarterly,
             pl_Velocity___Daily,
             pl_Transaction_Size,
             pl_Output_Volume,
             pl_Output_Value,
             pl_UTXO_Set_Size,
             pl_UTXO_Growth,           
             ncol=2)

############################################################################

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

mean(btc_mutual_ts$volatility)
sd(btc_mutual_ts$volatility)
xts_volatility <- xts(btc_mutual_ts[,"volatility"], order.by = as.Date(btc_mutual_ts[,1]))
pl_volatility <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = volatility)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "BCX Price Volatility in Percentage") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_mutual_ts$shorts)
sd(btc_mutual_ts$shorts)
xts_shorts <- xts(btc_mutual_ts[,"shorts"], order.by = as.Date(btc_mutual_ts[,1]))
pl_shorts <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = shorts)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Bitfinex Shorts in USD") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_mutual_ts$longs)
sd(btc_mutual_ts$longs)
xts_longs <- xts(btc_mutual_ts[,"longs"], order.by = as.Date(btc_mutual_ts[,1]))
pl_longs <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = longs)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "Bitfinex Longs in USD") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_mutual_ts$stockpulse_sent)
sd(btc_mutual_ts$stockpulse_sent)
xts_stockpulse_sent <- xts(btc_mutual_ts[,"stockpulse_sent"], order.by = as.Date(btc_mutual_ts[,1]))
pl_stockpulse_sent <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = stockpulse_sent)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "StockPulse Sentiment") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_mutual_ts$stockpulse_buzz)
sd(btc_mutual_ts$stockpulse_buzz)
xts_stockpulse_buzz <- xts(btc_mutual_ts[,"stockpulse_buzz"], order.by = as.Date(btc_mutual_ts[,1]))
pl_stockpulse_buzz <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = stockpulse_buzz)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "StockPulse Buzz") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

mean(btc_mutual_ts$sp500)
sd(btc_mutual_ts$sp500)
xts_sp500 <- xts(btc_mutual_ts[,"sp500"], order.by = as.Date(btc_mutual_ts[,1]))
pl_sp500 <- ggplot(btc_mutual_ts, aes(x = as.Date(btc_mutual_ts[,1]), y = sp500)) +
  geom_line() + theme(legend.position = "none") +
  labs(title = "S&P 500") + #, subtitle = "in million USD") + 
  labs(x = NULL) + labs(y = NULL) + theme(plot.title = element_text(size=9, face="bold"))

#MutualTimeSeriesCharts.pdf
options(digits=7)
grid.arrange(pl_volatility,
             pl_shorts,
             pl_longs,
             pl_stockpulse_sent,
             pl_stockpulse_buzz,
             pl_sp500,          
             ncol=2)