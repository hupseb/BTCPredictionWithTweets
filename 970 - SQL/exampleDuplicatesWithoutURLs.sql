/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [Nickname]
  FROM [IphoneTweets].[dbo].[duplicateTweetersURLcleaned]
  WHERE Nickname NOT IN (SELECT * FROM [IphoneTweets].[dbo].[duplicateTweeters])

  /****** Script for SelectTopNRows command from SSMS  ******/
/*SELECT Count([Tweet content]) AS Tweets, [Nickname] AS Nickname FROM [IphoneTweets].[dbo].[Stream$] GROUP BY [Nickname], [Tweet content] ORDER BY Tweets DESC*/
/*SELECT Count([Nickname]) AS Tweets, [Nickname] AS Nickname FROM [IphoneTweets].[dbo].[filtered] GROUP BY [Nickname] ORDER BY Tweets DESC*/

/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [Date],[Hour],[User Name],[Nickname],[Bio],[Tweet content]FROM [IphoneTweets].[dbo].[Stream$]
WHERE [Nickname] = '_Job_Online'

SELECT [Date],[Hour],[User Name],[Nickname],[Bio],[Tweet content URL removed]FROM [IphoneTweets].[dbo].[Stream$]
WHERE [Nickname] = '_Job_Online'