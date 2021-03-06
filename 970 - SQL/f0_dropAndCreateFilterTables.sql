USE [BitcoinTwts]
GO

DECLARE @from datetime2 = '2018-01-28 00:00:00.000';   
DECLARE @to   datetime2 = '2018-10-21 00:00:00.000'; 

/****** Object:  Table [dbo].[f1_tickeringSources_t]    Script Date: 7/26/2018 9:27:15 PM ******/
DROP TABLE [dbo].[f1_tickeringSources_t]
DROP TABLE [dbo].[f2_tickeringUsers_t]
DROP TABLE [dbo].[f3_usersTweetSame2x2_t]
DROP TABLE [dbo].[f4_bannedUsers_t]
DROP TABLE [dbo].[f6_spamHashtags_t]
DROP TABLE [dbo].[f7_followbackBots_t]
DROP TABLE [dbo].[f8_bannedOrDeletedAccounts_t]

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f1_tickeringSources_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND [source] IN (SELECT [source] FROM [BitcoinTwts].[dbo].[f1_tickeringSources])

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f2_tickeringUsers_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND [user_id] IN (SELECT [user_id] FROM [BitcoinTwts].[dbo].[f2_tickeringUsers])

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f3_usersTweetSame2x2_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND [user_id] IN (
SELECT [user_id]
  FROM [BitcoinTwts].[dbo].[f3_usersTweetSame2x2]
)

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f4_bannedUsers_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND user_id IN (
SELECT [user_id]
  FROM [BitcoinTwts].[dbo].[f4_bannedUsers]
)

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f6_spamHashtags_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND [status_id] IN (
SELECT [status_id]
  FROM [BitcoinTwts].[dbo].[f6_spamHashtags]
)

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f7_followbackBots_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND user_id IN (
SELECT [user_id]
  FROM [BitcoinTwts].[dbo].[f7_followbackBots]
)

SELECT [status_id]
INTO [BitcoinTwts].[dbo].[f8_bannedOrDeletedAccounts_t]
FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
WHERE [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
AND user_id IN (
SELECT [user_id]
  FROM [BitcoinTwts].[dbo].[f8_bannedOrDeletedAccounts]
)