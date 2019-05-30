install.packages(c("ggplot2", "RODBC","devtools", "RTextTools"))

library("RODBC")
library("devtools")
library("RTextTools")
library("e1071")
options(scipen = 999)

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=iPhoneXReleaseTweets;trusted_connection=true')

#iphonex.raw <- sqlQuery(conn, "SELECT [status_id]
#                        ,[created_at]
#                        ,[screen_name]
#                        ,[tweet]
#                        ,[sentiment] AS cf_sentiment
#                         FROM [iPhoneXReleaseTweets].[dbo].[CrowdFlower]
#                         WHERE (relevant_yn LIKE 'yes' AND relevant_yn_confidence = 1) AND sentiment_confidence = 1", as.is = TRUE)

pos_tweets = sqlQuery(conn, "SELECT TOP (1000) [status_id],[tweet],[sentiment] FROM dbo.CrowdFlower AS POS
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'positive')", as.is = TRUE)

neg_tweets = sqlQuery(conn, "SELECT TOP (1000) [status_id],[tweet],[sentiment] FROM dbo.CrowdFlower AS NEG
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative')", as.is = TRUE)

neu_tweets = sqlQuery(conn, "SELECT TOP (1000) [status_id],[tweet],[sentiment] FROM dbo.CrowdFlower AS NEU
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'neutral')", as.is = TRUE)

test_tweets = sqlQuery(conn, "SELECT [status_id],[tweet],[sentiment]
FROM dbo.CrowdFlower AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative' OR sentiment LIKE 'positive' OR sentiment LIKE 'neutral')

AND TESTDATA.status_id NOT IN
(
SELECT TOP (1000) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'negative') 
UNION ALL
SELECT TOP (1000) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'positive')
UNION ALL
SELECT TOP (1000) status_id FROM dbo.CrowdFlower
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.6) AND (sentiment LIKE 'neutral')
)
", as.is = TRUE)

tweets = rbind(pos_tweets, neg_tweets, neu_tweets, test_tweets)
matrix = create_matrix(tweets$tweet, language = "english", removeStopwords = FALSE, removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)

# naive bayes
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:3000, ], as.factor(tweets[1:3000, 3]))

predicted = predict(classifier, mat[3001:3051, ])
table(tweets[3001:3051, ], predicted)
recall_accuracy(tweets[3001:3051, 3], predicted)

# the other methods
# container = create_container(matrix, as.numeric(as.factor(tweets[, 3])), trainSize = 1:1500, testSize = 1501:3228, virgin = FALSE)
# models = train_models(container, algorithms = c("SVM", "RF"))
# results = classify_models(container, models)

# train a SVM Model
container <- create_container(matrix, as.factor(tweets[, 3]), trainSize = 1:3000, testSize = 3001:3101, virgin = FALSE)
model <- train_model(container, "SVM")
results <- classify_model(container, model)
View(results)
