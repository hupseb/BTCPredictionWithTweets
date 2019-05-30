USE [BitcoinTwts]

SELECT COUNT([screen_name]) as cnt, screen_name FROM
(SELECT [screen_name], [description] FROM [BitcoinTwts].[dbo].[f0_usersOutDescription]
WHERE [screen_name] NOT IN (SELECT [screen_name] FROM [f0_usersOutDesc_bannedOrDeleted])
AND [screen_name] NOT IN (SELECT [screen_name] FROM [f0_usersOutDesc])) AS A
GROUP BY screen_name
ORDER BY screen_name

--- last count 117548