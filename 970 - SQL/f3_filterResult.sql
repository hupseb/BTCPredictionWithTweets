/****** Script for SelectTopNRows command from SSMS  ******/
SELECT * FROM (
	SELECT * FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
	WHERE [created_at] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
		   [created_at] < Convert(datetime2, '2018-06-10 00:00:00.000' )
	AND [status_id] IN (
		SELECT [status_id] FROM [BitcoinTwts].[dbo].[f3_usersTweetSame2x2_t]
	)
) as A
ORDER BY A.[text_url_cleaned]