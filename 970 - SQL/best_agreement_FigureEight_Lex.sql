SELECT TOP (1000) [status_id]
                        ,[created_at]
                        ,[user_id]
                        ,[tweet]
                        ,[sentiment] AS fe_sentiment
                        FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw]
                        WHERE (relevant_yn LIKE 'yes' AND relevant_yn_confidence = 1) AND sentiment_confidence = 1 AND source = 'Twitter Web Client'
                        AND [created_at] >= Convert(datetime2, '2018-02-10 00:00:00.000' )
                        ORDER BY [created_at]