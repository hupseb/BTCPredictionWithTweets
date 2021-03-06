USE [BitcoinAnalysis]

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
  WHERE [user_id] NOT IN 
  (
	SELECT [user_id] FROM [dbo].[f2. ldu_tickeringUserst 00.] UNION ALL
	SELECT [user_id] FROM [dbo].[f3. usersTweetSame2x2t 00.] UNION ALL
	SELECT [user_id] FROM [dbo].[f4. bannedUserst]
  ) AND [source] NOT IN 
  (
	SELECT [source] FROM [dbo].[f1. lds_tickeringSourcest 00.]
  )
  AND [text_url_cleaned] NOT LIKE '%#presale%'
  AND [text_url_cleaned] NOT LIKE '%#airdrop%'
  AND [text_url_cleaned] NOT LIKE '%#domains%'
  AND [text_url_cleaned] NOT LIKE '%#tokensale%'
  AND [text_url_cleaned] NOT LIKE '%#tokenssale%'
  AND [text_url_cleaned] NOT LIKE '%#porn%'
  AND [text_url_cleaned] NOT LIKE '%#escort%'
  ORDER BY [text_url_cleaned]