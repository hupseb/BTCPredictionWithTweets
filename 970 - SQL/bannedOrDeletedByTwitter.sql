SELECT DISTINCT B.[id_str] AS [user_id]
FROM            [BitcoinTwts].[dbo].[f7_usersOutDesc_bannedOrDeleted] AS A INNER JOIN
                         [BitcoinTwts].[dbo].[UsersArchived] AS B ON B.[screen_name] = A.[screen_name]
UNION ALL
SELECT DISTINCT C.[user_id]
FROM            [BitcoinTwts].[dbo].[BitcoinTweets] AS C
WHERE        C.[screen_name] IN
                             (SELECT        [screen_name]
                               FROM            [BitcoinTwts].[dbo].[f7_usersOutDesc_bannedOrDeleted])