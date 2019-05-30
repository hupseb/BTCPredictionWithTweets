USE [BitcoinTwts]
GO

DELETE FROM [dbo].[BitcoinTweets]
  WHERE [created_at] >= Convert(datetime2, '2018-07-02 00:00:00.000' ) AND
         [created_at] < Convert(datetime2, '2018-07-02 02:00:00.000' )
GO

DELETE FROM [dbo].[BitcoinTweetsArchived]
  WHERE [created_at] >= Convert(datetime2, '2018-07-02 00:00:00.000' ) AND
         [created_at] < Convert(datetime2, '2018-07-02 02:00:00.000' )
GO

--SELECT Btwts.user_id, Btwts.status_id, Btwts.created_at, Btwts.screen_name, Btwts.text,
--BArch.user_id_str, BArch.id_str, BArch.created_at, BArch.full_text
--FROM [dbo].[BitcoinTweets] AS BTwts
--INNER JOIN  [dbo].[BitcoinTweetsArchived] AS BArch ON 
--Btwts.created_at = BArch.created_at
--ORDER BY status_id

--SELECT Btwts.user_id, Btwts.status_id, Btwts.created_at, Btwts.screen_name, Btwts.text,
--BArch.user_id_str, BArch.id_str, BArch.created_at, BArch.full_text
--FROM [dbo].[BitcoinTweets] AS BTwts
--INNER JOIN  [dbo].[BitcoinTweetsArchived] AS BArch ON 
--Btwts.status_id = BArch.id_str

--DELETE FROM [dbo].[BitcoinTweetsArchived]
--      WHERE id_str IN 
--(
--SELECT TOP (162) id_str
--  FROM [BitcoinTwts].[dbo].[BitcoinTweetsArchived]
--   WHERE [created_at] >= Convert(datetime2, '2018-04-06 16:00:00.000' ) AND
--[created_at] < Convert(datetime2, '2018-04-06 17:00:00.000' ) 
--ORDER BY NewId()
--)
--GO


