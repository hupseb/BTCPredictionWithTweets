/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [rownames]
      ,[status_id]
      ,[created_at]
      ,[user_id]
      ,[screen_name]
      ,[text]
      ,[text_url_cleaned]
      ,[source]
      ,[reply_to_status_id]
      ,[reply_to_user_id]
      ,[reply_to_screen_name]
      ,[is_quote]
      ,[is_retweet]
      ,[favorite_count]
      ,[retweet_count]
      ,[hashtags]
      ,[symbols]
      ,[urls_url]
      ,[urls_tco]
      ,[urls_expanded_url]
      ,[media_url]
      ,[media_tco]
      ,[media_expanded_url]
      ,[media_type]
      ,[ext_media_url]
      ,[ext_media_tco]
      ,[ext_media_expanded_url]
      ,[ext_media_type]
      ,[mentions_user_id]
      ,[mentions_screen_name]
      ,[lang]
      ,[quoted_status_id]
      ,[quoted_text]
      ,[retweet_status_id]
      ,[retweet_text]
      ,[place_url]
      ,[place_name]
      ,[place_full_name]
      ,[place_type]
      ,[country]
      ,[country_code]
      ,[geo_coords]
      ,[coords_coords]
      ,[bbox_coords]
  FROM [iPhoneXReleaseTweets].[dbo].[Tweets]
  ORDER BY status_id ASC

  SELECT UNICODE ( 'U+1F601' )  
  SELECT NCHAR(0x1F4A9)

--<U+2665><U+FE0F>
--♥️
--U+2665 U+FE0F
--E2 99 A5 EF B8 8F 0A
SELECT NCHAR(0x2665) + NCHAR(0xFE0F)

--<f0><U+009F><U+0098><U+008D>
--😍
--U+1F60D
--F0 9F 98 8D
SELECT NCHAR(0x01F60D)
SELECT NCHAR(0x009F) + NCHAR(0x0098) + NCHAR(0x008D) 
SELECT NCHAR(0xF0) + NCHAR(0x009F) + NCHAR(0x0098) + NCHAR(0x008D) 
SELECT NCHAR(0xF0) + NCHAR(0x9F) + NCHAR(0x98) + NCHAR(0x8D) 
SELECT NCHAR(0xF09F) + NCHAR(0x988D) 

SELECT NCHAR(0x01F60D)

CREATE FUNCTION dbo.GetSupplementaryCharacterInfo(@CodePoint INT)
RETURNS TABLE
WITH SCHEMABINDING
AS RETURN

WITH calc AS
(
  SELECT 55232 + (@CodePoint / 1024) AS [HighSurrogateINT],
         56320 + (@CodePoint % 1024) AS [LowSurrogateINT]
  WHERE  @CodePoint BETWEEN  65536 AND 1114111
)
SELECT @CodePoint AS [CodePointINT],
       HighSurrogateINT,
       LowSurrogateINT,
       CONVERT(VARBINARY(3), @CodePoint) AS [CodePointBIN],
       CONVERT(BINARY(2), HighSurrogateINT) AS [HighSurrogateBIN],
       CONVERT(BINARY(2), LowSurrogateINT) AS [LowSurrogateBIN],
       CONVERT(binary(4), NCHAR(HighSurrogateINT) + NCHAR(LowSurrogateINT)) AS [UTF-16LE],
       NCHAR(HighSurrogateINT) + NCHAR(LowSurrogateINT) AS [Character]
FROM   calc;
GO

--SELECT * FROM dbo.GetSupplementaryCharacterInfo(128169);
SELECT * FROM dbo.GetSupplementaryCharacterInfo(0x01F60D);

SELECT NCHAR(0xD83D) + NCHAR(0xDE0D)

SELECT CONVERT(VARBINARY(4), 0xF09F869A) 
SELECT CONVERT(VARBINARY(4), 0x009F008500B0FE0F) 

SELECT * FROM dbo.GetSupplementaryCharacterInfo(<f0><U+009F><U+0085><U+00B0><U+FE0F>);

SELECT NCHAR(0x2B55) 

SELECT NCHAR(0x009F) + NCHAR(0x0085) + NCHAR(0x00B0) + NCHAR(0xFE0F) 


-- a unicode string
declare @string nvarchar(100)
set @string=N'😍'

-- convert it to hex
declare @hex varbinary(100)
set @hex=convert(varbinary(100), @string)

-- convert it back to unicode
select @string, @hex, convert(nvarchar(100), @hex)

select rawtohex('F0 9F 98 8D')
