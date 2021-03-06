USE [BitcoinTwts]
GO 

/****** Object:  Table [dbo].[f1_tickeringSources_t]    Script Date: 7/26/2018 9:27:15 PM ******/
DROP TABLE [dbo].[f1_tickeringSources_t_intr]
DROP TABLE [dbo].[f2_tickeringUsers_t_intr]
DROP TABLE [dbo].[f3_usersTweetSame2x2_t_intr]
DROP TABLE [dbo].[f4_bannedUsers_t_intr]
DROP TABLE [dbo].[f6_spamHashtags_t_intr]
DROP TABLE [dbo].[f7_followbackBots_t_intr]
DROP TABLE [dbo].[f8_bannedOrDeletedAccounts_t_intr]

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f1_tickeringSources_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [source] IN (SELECT [source] FROM [BitcoinTwts].[dbo].[f1_tickeringSourcesIntr])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f2_tickeringUsers_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [user_id_str] IN (SELECT [user_id_str] FROM [BitcoinTwts].[dbo].[f2_tickeringUsersIntr])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f3_usersTweetSame2x2_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [user_id_str] IN (SELECT [user_id_str] FROM [BitcoinTwts].[dbo].[f3_usersTweetSame2x2Intr])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f4_bannedUsers_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [user_id_str] IN (SELECT [user_id] FROM [BitcoinTwts].[dbo].[f4_bannedUsers])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f6_spamHashtags_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [id_str] IN (SELECT [id_str] FROM [BitcoinTwts].[dbo].[f6_spamHashtagsIntr])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f7_followbackBots_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [user_id_str] IN (SELECT [user_id_str] FROM [BitcoinTwts].[dbo].[f7_followbackBotsIntr])

SELECT [id_str]
INTO [BitcoinTwts].[dbo].[f8_bannedOrDeletedAccounts_t_intr]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
WHERE [user_id_str] IN (SELECT [user_id_str] FROM [BitcoinTwts].[dbo].[f8_bannedOrDeletedAccountsIntr])