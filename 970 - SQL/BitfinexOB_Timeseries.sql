/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [timestamp_m]
      ,[amount]
      ,[price]
      ,[symbol]
      ,[id]
  FROM [CryptoCurrTweets].[dbo].[BitfinexOB_C_m]
  ORDER BY timestamp_m DESC

Declare @Date1 DATETIME = '2017-12-18 10:33:00.000'
Declare @Date2 DATETIME = '2018-06-07 03:35:00.000'
SELECT DATEDIFF(MINUTE, @Date1, @Date2);


CREATE FUNCTION  dbo.GetNums2(@N AS BIGINT) RETURNS TABLE
AS
RETURN
SELECT TOP (@N) ROW_NUMBER() OVER(ORDER BY (SELECT NULL)) AS I
    FROM SYS.OBJECTS S1
    CROSS JOIN SYS.OBJECTS S2
    CROSS JOIN SYS.OBJECTS S3
    CROSS JOIN SYS.OBJECTS S4
    CROSS JOIN SYS.OBJECTS S5
GO


DECLARE @START_DATE datetime2(7)
SET @START_DATE = '2017-12-18 10:32:00.000'
 
INSERT [dbo].[BitfinexOB_Timeseries]
SELECT
    DATEADD(MINUTE, I, @START_DATE)
FROM
    dbo.GetNums2(245823)




DECLARE @START_DATE datetime2(7)
SET @START_DATE = '2017-12-18 10:32:00.000'
 
SELECT
    DATEADD(MINUTE, I, @START_DATE) AS timestamp_m
    ,Price
    ,(
        SELECT Price
        FROM dbo.BitfinexOB_C_m
        WHERE timestamp_m =
            (SELECT MAX(timestamp_m) FROM dbo.BitfinexOB_C_m P_PREV WHERE P_PREV.timestamp_m <= DATEADD(MINUTE, I, @START_DATE))
    ) AS PriceLastKnown
FROM
    dbo.GetNums2(245823)
LEFT OUTER JOIN
    dbo.BitfinexOB_C_m AS P
ON
    DATEADD(MINUTE, I, @START_DATE) = P.timestamp_m
ORDER BY
    timestamp_m