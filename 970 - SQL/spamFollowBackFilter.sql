/****** Script for SelectTopNRows command from SSMS  ******/

SELECT * FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d] WHERE [user_id] IN
(
SELECT DISTINCT [user_id]
  FROM [CryptoCurrTweets].[dbo].[BitcoinTweets]
    WHERE description LIKE '%#followback%'
)

SELECT DISTINCT [user_id] FROM [CryptoCurrTweets].[dbo].[BitcoinTweets]
WHERE [CryptoCurrTweets].[dbo].[BitcoinTweets].[description] IS  NULL