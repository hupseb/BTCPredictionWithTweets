SELECT TOP 2790 [tweet], [sentiment] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'positive')
FOR JSON AUTO

SELECT TOP 2790 [tweet], [sentiment] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'negative')
FOR JSON AUTO

SELECT TOP 310 [tweet], [sentiment], [_unit_id] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'positive')
AND [_unit_id] NOT IN
(
SELECT TOP 2790 [_unit_id] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'positive')
)
FOR JSON AUTO

SELECT TOP 310 [tweet], [sentiment], [_unit_id] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'negative')
AND [_unit_id] NOT IN
(
SELECT TOP 2790 [_unit_id] FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66 ) AND (sentiment LIKE 'negative')
)
FOR JSON AUTO