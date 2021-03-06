/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [status_id]
	  ,[created_at]
      ,[user_id]
      ,[text_url_cleaned]
      ,[text]
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  WHERE [user_id] = 952840735698468864 AND [text_url_cleaned] LIKE '%ICO%'
  ORDER BY [text_url_cleaned]

SELECT A.[user_id]
      ,[amountTweets]
	  ,B.cnt15leftchars AS distinct15Leftchars
      ,[sumFav]
      ,[sumRetweet]
      ,[Amount2Favs]
      ,[Amount2Retweet]
  FROM [BitcoinAnalysis].[dbo].[f5. lowAmountToFavRate] AS A 
  INNER JOIN [BitcoinAnalysis].[dbo].[ldu_distinctTwts15chars 00.] AS B
  ON A.user_id = B.user_id
  WHERE A.user_id = 183182212
  