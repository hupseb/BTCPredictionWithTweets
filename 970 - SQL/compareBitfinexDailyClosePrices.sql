/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [timestamp_m]
      ,[amount]
      ,[price]
      ,[symbol]
  FROM [CryptoCurrTweets].[dbo].[10. BitfinexOB_C_m]
  WHERE timestamp_m LIKE '%23:59:%'