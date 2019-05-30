SELECT [status_id],[tweet],[sentiment]
FROM dbo.CrowdFlower AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative' OR sentiment LIKE 'positive' OR sentiment LIKE 'neutral')

AND TESTDATA.status_id NOT IN
(
SELECT TOP (500) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative') 
UNION ALL
SELECT TOP (500) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'positive')
UNION ALL
SELECT TOP (500) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'neutral')
)