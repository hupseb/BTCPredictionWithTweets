install.packages(c("ggplot2", "RODBC","devtools", "RTextTools", "caret"))

library("RODBC")
library("devtools")
library("RTextTools")
library("e1071")
library("caret")
options(scipen = 999)

#######################################################################################
#######################################################################################
#######################################################################################

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=iPhoneXReleaseTweets;trusted_connection=true')

positive_all = sqlQuery(conn, "SELECT TOP 270 [tweet]
FROM dbo.CrowdFlower AS TESTDATA
                 WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence = 1) AND (sentiment LIKE 'positive') 
                 ORDER BY NEWID()"
                 , as.is = TRUE)
positive = positive_all[1:240,]
positive_test = positive_all[241:270,]

negative_all = sqlQuery(conn, "SELECT TOP 270 [tweet]
FROM dbo.CrowdFlower AS TESTDATA
                 WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence  = 1) AND (sentiment LIKE 'negative') 
                 ORDER BY NEWID()"
                       , as.is = TRUE)
negative = negative_all[1:240,]
negative_test = negative_all[241:270,]

#######################################################################################

tweet = c(positive, negative)
tweet_test= c(positive_test, negative_test)
tweet_all = c(tweet, tweet_test)

sentiment = c(rep("positive", length(positive) ), 
              rep("negative", length(negative)))

sentiment_test = c(rep("positive", length(positive_test) ), 
                   rep("negative", length(negative_test)))

sentiment_all = as.factor(c(sentiment, sentiment_test))

########################################################################################

library(RTextTools)

# naive bayes
#mat= create_matrix(tweet_all, language="english", 
#                   removeStopwords=FALSE, removeNumbers=TRUE, 
#                   stemWords=FALSE, tm::weightTfIdf)

#mat = as.matrix(mat)

#classifier = naiveBayes(mat[1:320,], as.factor(sentiment_all[1:320]))
#predicted = predict(classifier, mat[321:360,]); predicted

#table(sentiment_test, predicted)
#recall_accuracy(sentiment_test, predicted)

########################################################################################

# the other methods
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:480, testSize=481:540,virgin=FALSE) #可以设置removeSparseTerms

models = train_models(container, algorithms=c("MAXENT",
                                              "SVM",
                                              #"GLMNET", "BOOSTING", 
                                              "SLDA","BAGGING", 
                                              "RF", # "NNET", 
                                              "TREE"))

# test the model
results = classify_models(container, models)

table(as.numeric(as.numeric(sentiment_all[481:540])), results[,"SVM_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[481:540])), results[,"SVM_LABEL"])
confusionMatrix(table(as.numeric(as.numeric(sentiment_all[481:540])), results[,"SVM_LABEL"]))

# formal tests
analytics = create_analytics(container, results)
summary(analytics)
#head(analytics@algorithm_summary)
#head(analytics@label_summary)
#head(analytics@document_summary)
#analytics@ensemble_summary # Ensemble Agreement

# Cross Validation
#=10
#cross_SVM = cross_validate(container,N,"SVM")
#cross_GLMNET = cross_validate(container,N,"GLMNET")
#cross_MAXENT = cross_validate(container,N,"MAXENT")