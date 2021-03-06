/****** Script for SelectTopNRows command from SSMS  ******/
SELECT CONCAT('https://twitter.com/twitterapi/status/', [status_id]) as tweet_url
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
  FROM [BitcoinTwts].[dbo].[test2]