/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [status_id]
      ,[created_at]
      ,[user_id]
      ,[text]
      ,[text_url_cleaned] 
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
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  WHERE text_url_cleaned = 'Myetherwallet Relaunches as Mycrypto Following a Hostile Twitter Takeover #bitcoin'