USE [BitcoinTwts]
GO

DECLARE @from datetime2 = '2018-01-28 00:00:00.000';   
DECLARE @to   datetime2 = '2018-06-10 00:00:00.000'; 

SELECT *
  FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion]
  --WHERE [user_id] = ''
  WHERE [text_url_cleaned] LIKE '% KUCOIN %'
  AND [created_at] >= Convert(datetime2, @from ) AND
       [created_at] < Convert(datetime2, @to )

--                       (text_url_cleaned LIKE '%#CapitalTechnologiesResearch %') OR
--                         (text_url_cleaned LIKE '% KUCOIN %') OR