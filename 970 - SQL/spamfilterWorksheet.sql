/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [status_id]
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
  WHERE [user_id] = 3367334171
  ORDER BY text_url_cleaned

	SELECT * FROM [dbo].[f2. ldu_tickeringUserst 00.] WHERE [user_id] = 3367334171
	SELECT * FROM [dbo].[f4. bannedUsers] WHERE [user_id] = 3367334171
	SELECT * FROM [dbo].[f1. lds_tickeringSourcest 00.] WHERE [source] = 'IFTTT'
	SELECT * FROM [dbo].[f3. usersTweetSame2x2t 00.] WHERE [user_id] = 3367334171
    

SELECT        COUNT(user_id) AS user_id_cnt, user_id
FROM            (SELECT COUNT(text_url_cleaned) AS sametweets, text_url_cleaned, user_id, CONVERT(varchar(16), created_at, 105) AS created_at_day
				FROM            dbo.[duplicateTweets 00.]
				GROUP BY text_url_cleaned, user_id, CONVERT(varchar(16), created_at, 105)
				HAVING         (COUNT(text_url_cleaned) >= 2)) AS A
GROUP BY user_id
HAVING (COUNT(user_id) > 2)
ORDER BY user_id

SELECT *
INTO [dbo].[f3. usersTweetSame2x2t 00.]
FROM [dbo].[f3. usersTweetSame2x2 00.]