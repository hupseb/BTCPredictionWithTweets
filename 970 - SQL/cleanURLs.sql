/****** Script for SelectTopNRows command from SSMS  ******/
USE [BitcoinTwts]
 

--SELECT [BitcoinTwts].[dbo].RgxReplace([text],'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?','') AS [regexed]
--FROM [BitcoinTwts].[dbo].[BitcoinTweets]


UPDATE [BitcoinTwts].[dbo].[BitcoinTweets]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([text],'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?','')
UPDATE [BitcoinTwts].[dbo].[BitcoinTweets]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([text_url_cleaned],'\s\s',' ')

UPDATE [BitcoinTwts].[dbo].[BitcoinTweetsArchived]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([full_text],'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?','')
UPDATE [BitcoinTwts].[dbo].[BitcoinTweetsArchived]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([text_url_cleaned],'\s\s',' ')

UPDATE [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([full_text],'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?','')
UPDATE [BitcoinTwts].[dbo].[BitcoinTweetsIntr]
SET [text_url_cleaned] = [BitcoinTwts].[dbo].RgxReplace([text_url_cleaned],'\s\s',' ')

