options(scipen=999)
Sys.setenv(TZ='UTC')

library("devtools")
library("forecast")
library("RODBC")

#get tweets per hour
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
btcTwts = sqlQuery(conn, paste("SELECT [created_at_by_hour], [Expr1] as amount
                                FROM [BitcoinTwts].[dbo].[BitcoinTweetsCountedByHour]
                                  WHERE [created_at_by_hour] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
                                        [created_at_by_hour] <  Convert(datetime2, '2018-02-28 00:00:00.000' )
                                ORDER BY [created_at_by_hour] ASC"), as.is = TRUE)
odbcClose(conn)

btcTwts_ts <- ts(btcTwts[,2], frequency = 24*7)
plot(btcTwts_ts)
decomp <- decompose(btcTwts_ts)
plot(decomp)

#btcTwts_ts <- ts(btcTwts[1:(24*3),2], frequency = 24)
#plot(btcTwts_ts)
#decomp <- decompose(btcTwts_ts)
#plot(decomp)
     
plot(decomp, axes = FALSE)
a = seq.POSIXt(as.POSIXct("2018-01-28 00:00:00.000"), as.POSIXct("2018-02-09 23:00:00"), by="hour")
axis(1, at = 1:72, labels = a[1:(24*3)], las=2, cex.axis=0.6)
axis(2, cex.axis=0.6)


axis(1, at = as.numeric(a)/365.25+1970, labels = a, cex.axis=0.6)
axis(2, cex.axis=0.6)