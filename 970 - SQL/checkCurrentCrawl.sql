/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP(30000) *
  FROM [CryptoCurrTweets].[dbo].[BitcoinTweets]
  ORDER BY status_id DESC
  