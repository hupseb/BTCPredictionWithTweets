/****** Script for SelectTopNRows command from SSMS  ******/
SELECT *
  FROM [BitcoinAnalysis].[dbo].[00. FigureEight_sample_1_d]

  -- still spam
  -- https://twitter.com/twitterapi/status/1000796721214050304
  -- 

-- Analyze days with gaps
SELECT d = CONVERT(DATE, [created_at] ), c = COUNT(*)
FROM [BitcoinAnalysis].[dbo].[00. FigureEight_sample_1_d]
GROUP BY CONVERT(DATE, [created_at])
ORDER BY d;
