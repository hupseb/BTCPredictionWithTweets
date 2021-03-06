/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [timestamp_m]
      ,[amount]
      ,[price]
      ,[symbol]
  --INTO [CryptoCurrTweets].[dbo].[BitfinexOB_C_m_corrections]
  FROM [CryptoCurrTweets].[dbo].[BitfinexOB_C_m]
  WHERE timestamp_m IN 
  (
SELECT CONVERT(datetime2, '12/30/17 1:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 11:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '12/31/17 12:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 8:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 9:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 10:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 11:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/6/18 12:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 10:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 11:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 12:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 13:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 14:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 22:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/7/18 23:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 0:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 1:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 2:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 5:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 6:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 7:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 8:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/8/18 9:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/12/18 5:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/21/18 1:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '1/21/18 1:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/16/18 23:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 9:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 10:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 11:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/21/18 12:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/22/18 4:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/29/18 9:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/29/18 19:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '3/31/18 23:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/8/18 2:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/8/18 2:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/8/18 7:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/10/18 8:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 4:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 9:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/11/18 10:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/14/18 4:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/14/18 4:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/18/18 10:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 1:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 2:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 3:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 4:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 5:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '4/29/18 6:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/6/18 4:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/6/18 20:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/8/18 22:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/9/18 1:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/9/18 1:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/11/18 3:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/11/18 4:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/15/18 2:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/15/18 4:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/17/18 4:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/17/18 9:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/17/18 9:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/17/18 9:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/17/18 12:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/18/18 0:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/18/18 22:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 2:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 3:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 4:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 4:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 5:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 6:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 7:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 10:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 11:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 11:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 14:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 14:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 17:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 18:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 18:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 18:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 19:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 20:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 20:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/19/18 22:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 4:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 5:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 6:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 6:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 6:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 7:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 7:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/20/18 19:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 0:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 1:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 17:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 19:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 21:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 22:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/21/18 22:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/22/18 3:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/22/18 4:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/24/18 2:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/25/18 12:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/25/18 20:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/25/18 20:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/25/18 22:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 0:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 1:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 1:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 2:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 2:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 2:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 3:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 4:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 5:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 5:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 10:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 10:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 11:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 11:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 12:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 14:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 14:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 14:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 14:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 15:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 17:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 17:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 18:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 18:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 19:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/26/18 20:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 3:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 4:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 4:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 15:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 19:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 20:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 21:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 22:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 23:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/27/18 23:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 0:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 0:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 1:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 2:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 2:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 14:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 18:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/28/18 18:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/29/18 1:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/29/18 4:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/29/18 6:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/29/18 9:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/29/18 10:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/30/18 4:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '5/30/18 5:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/1/18 1:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/1/18 1:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/1/18 2:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/1/18 2:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 0:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 3:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 3:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 3:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 4:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 21:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 22:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 23:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 23:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/2/18 23:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 2:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 3:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 4:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 14:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 20:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 22:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 22:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/3/18 23:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 3:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 4:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 4:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 20:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 21:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/4/18 23:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 0:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 0:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 1:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 5:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 10:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 11:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 12:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 12:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 12:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:41') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:42') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:43') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:44') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:57') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:58') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 13:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:00') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:01') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:02') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:03') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:04') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:05') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:06') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:07') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:08') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:09') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:10') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:11') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:12') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:13') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:14') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:15') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:16') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:17') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:18') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:19') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:20') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:21') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:22') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:23') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:24') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:25') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:26') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:27') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:28') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:29') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:30') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:31') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:32') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:33') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:34') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:35') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:36') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:37') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:38') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:39') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:40') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:45') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:46') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:47') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:48') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:49') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:50') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:51') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:52') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:53') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:54') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:55') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/5/18 14:56') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/6/18 6:59') AS datetime2 UNION ALL
SELECT CONVERT(datetime2, '6/6/18 10:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/30/17 1:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 11:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '12/31/17 12:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 8:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 9:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 10:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 11:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/6/18 12:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 10:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 11:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 12:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 13:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 14:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 22:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/7/18 23:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 0:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 1:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 2:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 5:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 6:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 7:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 8:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/8/18 9:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/12/18 5:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/21/18 1:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '1/21/18 1:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/16/18 23:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 9:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 10:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 11:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/21/18 12:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/22/18 4:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/29/18 9:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/29/18 19:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '3/31/18 23:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/8/18 2:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/8/18 2:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/8/18 7:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/10/18 8:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 4:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 9:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/11/18 10:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/14/18 4:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/14/18 4:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/18/18 10:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 1:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 2:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 3:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 4:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 5:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '4/29/18 6:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/6/18 4:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/6/18 20:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/8/18 22:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/9/18 1:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/9/18 1:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/11/18 3:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/11/18 4:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/15/18 2:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/15/18 4:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/17/18 4:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/17/18 9:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/17/18 9:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/17/18 9:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/17/18 12:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/18/18 0:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/18/18 22:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 2:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 3:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 4:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 4:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 5:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 6:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 7:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 10:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 11:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 11:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 14:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 14:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 17:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 18:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 18:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 18:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 19:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 20:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 20:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/19/18 22:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 4:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 5:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 6:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 6:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 6:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 7:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 7:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/20/18 19:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 0:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 1:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 17:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 19:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 21:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 22:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/21/18 22:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/22/18 3:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/22/18 4:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/24/18 2:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/25/18 12:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/25/18 20:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/25/18 20:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/25/18 22:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 0:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 1:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 1:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 2:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 2:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 2:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 3:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 4:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 5:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 5:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 10:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 10:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 11:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 11:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 12:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 14:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 14:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 14:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 14:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 15:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 17:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 17:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 18:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 18:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 19:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/26/18 20:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 3:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 4:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 4:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 15:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 19:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 20:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 21:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 22:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 23:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/27/18 23:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 0:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 0:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 1:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 2:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 2:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 14:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 18:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/28/18 18:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/29/18 1:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/29/18 4:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/29/18 6:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/29/18 9:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/29/18 10:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/30/18 4:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '5/30/18 5:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/1/18 1:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/1/18 1:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/1/18 2:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/1/18 2:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 0:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 3:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 3:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 3:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 4:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 21:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 22:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 23:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 23:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/2/18 23:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 2:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 3:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 4:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 14:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 20:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 22:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 22:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/3/18 23:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 3:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 4:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 4:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 20:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 21:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/4/18 23:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 0:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 0:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 1:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 5:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 10:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 11:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 12:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 12:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 12:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:41') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:42') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:43') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:44') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:57') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:58') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 13:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:00') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:01') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:02') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:03') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:04') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:05') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:06') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:07') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:08') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:09') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:10') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:11') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:12') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:13') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:14') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:15') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:16') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:17') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:18') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:19') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:20') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:21') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:22') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:23') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:24') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:25') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:26') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:27') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:28') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:29') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:30') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:31') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:32') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:33') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:34') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:35') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:36') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:37') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:38') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:39') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:40') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:45') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:46') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:47') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:48') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:49') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:50') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:51') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:52') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:53') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:54') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:55') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/5/18 14:56') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/6/18 6:59') AS datetime2 UNION ALL
SELECT DATEADD(minute, -1, '6/6/18 10:28') AS datetime2
)