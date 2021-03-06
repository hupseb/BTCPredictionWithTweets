/****** Script for SelectTopNRows command from SSMS  ******/
SELECT top 10000 [text_url_cleaned] as tweet
      ,('https://twitter.com/statuses/' + CAST([status_id] AS varchar)) as tweet_url
	  ,[screen_name]
      ,[created_at]
      ,[user_id]
      ,[source]
      ,[reply_to_status_id]
      ,[reply_to_user_id]
      ,[reply_to_screen_name]
      ,[is_quote]
      ,[is_retweet]
      ,[favorite_count]
      ,[retweet_count]
      ,[hashtags]
      ,[symbols]
      ,[urls_url]
      ,[urls_tco]
      ,[urls_expanded_url]
      ,[media_url]
      ,[media_tco]
      ,[media_expanded_url]
      ,[media_type]
      ,[ext_media_url]
      ,[ext_media_tco]
      ,[ext_media_expanded_url]
      ,[ext_media_type]
      ,[mentions_user_id]
      ,[mentions_screen_name]
      ,[lang]
      ,[quoted_status_id]
      ,[quoted_text]
      ,[retweet_status_id]
      ,[retweet_text]
      ,[place_url]
      ,[place_name]
      ,[place_full_name]
      ,[place_type]
      ,[country]
      ,[country_code]
      ,[geo_coords]
      ,[coords_coords]
      ,[bbox_coords]      
  FROM [iPhoneXReleaseTweets].[dbo].[Filtered]
  where created_at > Convert(datetime, '2017-11-04 00:00:00.000' )
  order by newid()