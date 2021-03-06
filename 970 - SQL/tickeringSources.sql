/****** Script for SelectTopNRows command from SSMS  ******/
SELECT COUNT([source]) as cntsources
      ,[source]    
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  GROUP BY [source]
  ORDER BY cntsources DESC

SELECT COUNT(DISTINCT LEFT ([text], 15)) AS cnt15leftchars, [source]
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  GROUP BY [source]

SELECT A.cntsources, B.cnt15leftchars, ((cast(B.cnt15leftchars AS float)) / (cast(A.cntsources AS float))) AS percentage, A.[source], B.[source] FROM [BitcoinAnalysis].[dbo].[tweetsbysource 00.] AS A
INNER JOIN [BitcoinAnalysis].[dbo].[distinctTweets15chars 00.] AS B ON A.[source] = B.[source]
ORDER BY percentage