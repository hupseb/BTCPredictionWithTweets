SELECT d = CONVERT(DATE, created_at ), c = COUNT(*)
FROM [BitcoinTwts].[dbo].[BitcoinTweets]
GROUP BY CONVERT(DATE, created_at)
ORDER BY d;

SELECT d = CONVERT(DATE, created_at ), c = COUNT(*)
FROM [BitcoinTwts].[dbo].[BitcoinTweets]
GROUP BY CONVERT(DATE, created_at)
ORDER BY c;

SELECT dateadd(hour, datediff(hour, 0, [created_at]), 0) as TimeStampHour, Count(*)
FROM [BitcoinTwts].[dbo].[BitcoinTweets]
GROUP BY dateadd(hour, datediff(hour, 0, [created_at]), 0)
ORDER BY dateadd(hour, datediff(hour, 0, [created_at]), 0);

------------------------------------------------------------------------------------------

SELECT dateadd(hour, datediff(hour, 0, [created_at]), 0) as TimeStampHour, Count(*) tweetsPerDay
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
 WHERE [created_at] >= Convert(datetime2, '2018-07-23 00:00:00.000' ) AND
        [created_at] < Convert(datetime2, '2018-07-24 00:00:00.000' )
GROUP BY dateadd(hour, datediff(hour, 0, [created_at]), 0)
ORDER BY dateadd(hour, datediff(hour, 0, [created_at]), 0) ASC

-- Analyze days with gaps
SELECT d = CONVERT(DATE, [created_at_by_hour] ), c = COUNT(*)
FROM [BitcoinTwts].[dbo].[BitcoinTweetsCountedByHour]
GROUP BY CONVERT(DATE, [created_at_by_hour])
ORDER BY d;

SELECT dateadd(hour, datediff(hour, 0, [created_at]), 0) as TimeStampHour, Count(*) as cnt
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
--WHERE [BitcoinTwts].[dbo].[BitcoinTweetsUnion].[created_at] >= Convert(datetime, '2018-06-16' ) AND
--[BitcoinTwts].[dbo].[BitcoinTweetsUnion].[created_at] <= Convert(datetime, '2018-06-17' )
GROUP BY dateadd(hour, datediff(hour, 0, [created_at]), 0)
ORDER BY Count(*) DESC

SELECT d = CONVERT(DATE, [created_at] ), c = COUNT(*)
FROM [BitcoinTwts].[dbo].[BitcoinTweets]
WHERE [created_at] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
      [created_at] <  Convert(datetime2, '2018-06-10 00:00:00.000' )
GROUP BY CONVERT(DATE, created_at)
ORDER BY d;