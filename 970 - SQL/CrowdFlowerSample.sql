/****** Script for SelectTopNRows command from SSMS  ******/
USE [BitcoinTwts]
GO

DECLARE @from datetime2 = '2018-01-28 00:00:00.000';   
DECLARE @to   datetime2 = '2018-06-10 00:00:00.000'; 

SELECT TOP (25000) CONCAT('https://twitter.com/twitterapi/status/', [status_id]) as tweet_url
      ,[text_url_cleaned] as tweet
      ,[status_id]
	  ,[created_at]
      ,[user_id]
      ,[source]
      ,[lang]
      ,[coordinates]
      ,[is_quote]
      ,[favorite_count]
      ,[retweet_count]
      ,[quoted_status_id]
      ,[retweet_status_id]
      ,[hashtags]
      ,[symbols]
  INTO [BitcoinAnalysis].[dbo].[FigureEight_sample_5_d]
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered]
  WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
  ORDER BY NEWID()