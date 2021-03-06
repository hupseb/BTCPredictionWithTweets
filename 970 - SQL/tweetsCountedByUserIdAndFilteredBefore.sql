/****** Script for SelectTopNRows command from SSMS  ******/
SELECT COUNT([user_id]) AS useridcnt
      ,[user_id]
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  WHERE [user_id] NOT IN (SELECT * FROM [dbo].[spamAccounts 00.])
  AND [source] NOT IN (SELECT * FROM [dbo].[lowDistinctSources 00.])
  GROUP BY [user_id]
  ORDER BY useridcnt DESC