SELECT [iPhoneXReleaseTweets].[dbo].RgxReplace([text],'(<f0><U\+009F><U\+0098><U\+0081>)','XXXXXXXXX') AS [regexed]
FROM [iPhoneXReleaseTweets].[dbo].[Tweets]

SELECT [text], [status_id]
FROM [iPhoneXReleaseTweets].[dbo].[Tweets]
WHERE [text] LIKE '%&%' AND [status_id] NOT IN (
	SELECT [status_id], [text]
	FROM [iPhoneXReleaseTweets].[dbo].[Tweets]
	WHERE [text] LIKE '%&amp;%'
)

SELECT [status_id], [text]
FROM [iPhoneXReleaseTweets].[dbo].[Tweets]
WHERE [text] LIKE '%&amp;%' OR [text] LIKE '%&gt;%' OR [text] LIKE '%&lt;%'