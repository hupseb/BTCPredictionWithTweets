SELECT * FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, '2018-01-28 00:00:00.000' ) AND
       [created_at] < Convert(datetime2, '2018-06-10 00:00:00.000' )
AND [status_id] IN (
SELECT [status_id]
  FROM [BitcoinTwts].[dbo].[f4_bannedUsers_t]
)
ORDER BY [user_id]