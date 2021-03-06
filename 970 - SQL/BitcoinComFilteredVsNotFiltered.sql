USE [BitcoinTwts]
GO

DECLARE @from datetime2 = '2018-01-28 00:00:00.000';   
DECLARE @to   datetime2 = '2018-06-10 00:00:00.000';

SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
  WHERE [user_id] = '380069391' AND
        [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )
	   AND status_id  NOT IN
	   (
	   SELECT status_id
		FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered]
		WHERE [user_id] = '380069391' AND
        [created_at] >= Convert(datetime2, @from ) AND
		[created_at] < Convert(datetime2, @to )
	   )
	   ORDER BY status_id