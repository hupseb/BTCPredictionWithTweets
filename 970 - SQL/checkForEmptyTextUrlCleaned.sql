/****** Script for SelectTopNRows command from SSMS  ******/
SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweets]
  WHERE [text_url_cleaned] IS NULL

  SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsArchived]
  WHERE [text_url_cleaned] IS NULL

    SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
  WHERE [text_url_cleaned] IS NULL