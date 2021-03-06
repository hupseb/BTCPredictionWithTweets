SELECT *
FROM dbo.CrowdFlower AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6)

AND TESTDATA.status_id NOT IN
(
SELECT TOP (1000) status_id FROM dbo.CrowdFlower AS NEG
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative') 
UNION ALL
SELECT TOP (1000) status_id FROM dbo.CrowdFlower AS POS
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'positive')
UNION ALL
SELECT TOP (1000) status_id FROM dbo.CrowdFlower AS NEU
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'neutral') 
)






--SELECT COUNT(*) AS TOTALPOSITIVE
--FROM dbo.CrowdFlower AS TESTDATA
--WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence = 1) AND (sentiment LIKE 'positive')

--SELECT COUNT(*) AS TOTALNEGATIVE
--FROM dbo.CrowdFlower AS TESTDATA
--WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence = 1) AND (sentiment LIKE 'negative')

--SELECT COUNT(*) AS TOTALNEUTRAL
--FROM dbo.CrowdFlower AS TESTDATA
--WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence = 1) AND (sentiment LIKE 'neutral')