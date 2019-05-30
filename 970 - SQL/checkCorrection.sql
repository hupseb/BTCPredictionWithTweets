/****** Script for SelectTopNRows command from SSMS  ******/
SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
  WHERE [created_at] >= Convert(datetime2, '2018-06-22 22:00:00.000' ) AND
         [created_at] < Convert(datetime2, '2018-06-23 00:00:00.000' )

SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsArchived]
  WHERE [created_at] >= Convert(datetime2, '2018-06-22 22:00:00.000' ) AND
         [created_at] < Convert(datetime2, '2018-06-23 00:00:00.000' )