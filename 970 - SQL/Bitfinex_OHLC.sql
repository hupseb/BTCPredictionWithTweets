--CLOSE
SELECT TOP (1000) [symbol], [timestamp], MAX(id) as id
FROM [CryptoCurrTweets].[dbo].[BitfinexOB] 
GROUP BY [symbol], [timestamp]
ORDER BY [timestamp]

--OPEN
SELECT TOP (1000) [symbol], [timestamp], MIN(id) as id
FROM [CryptoCurrTweets].[dbo].[BitfinexOB] 
GROUP BY [symbol], [timestamp]
ORDER BY [timestamp]

--HIGH
SELECT TOP (1000) [symbol], [timestamp], MAX(price) as id
FROM [CryptoCurrTweets].[dbo].[BitfinexOB] 
GROUP BY [symbol], [timestamp]
ORDER BY [timestamp]

--LOW
SELECT TOP (1000) [symbol], [timestamp], MIN(price) as id
FROM [CryptoCurrTweets].[dbo].[BitfinexOB] 
GROUP BY [symbol], [timestamp]
ORDER BY [timestamp]