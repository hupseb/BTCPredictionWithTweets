options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages(c("ggplot2", "RODBC","devtools", "RTextTools", "caret"))

library("RODBC")
library("devtools")
library("RTextTools")
library("e1071")
library("caret")

#######################################################################################
#######################################################################################
#######################################################################################

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')

#select top(100) ClumnName from Table1 where ClumnName NOT IN (select top (100) ClumnName from Table1 )

positive_all = sqlQuery(conn, "SELECT TOP 3100 [tweet], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66) AND (sentiment LIKE 'positive') 
--AND [_unit_id] NOT IN(
--         SELECT TOP 3100 [_unit_id]
--         FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
--         WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66) AND (sentiment LIKE 'positive')                         
--)
", as.is = TRUE)
positive = positive_all[1:2400,1]
positive_test = positive_all[2401:3100,1]

negative_all = sqlQuery(conn, "SELECT TOP 3100 [tweet], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66) AND (sentiment LIKE 'negative') 
--AND [_unit_id] NOT IN(
--         SELECT TOP 3100 [_unit_id]
--        FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
--         WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence > 0.66) AND (sentiment LIKE 'negative')                         
--)
", as.is = TRUE)
negative = negative_all[1:2400,1]
negative_test = negative_all[2401:3100,1]

#######################################################################################

tweet = c(positive, negative)
tweet_test= c(positive_test, negative_test)
tweet_all = c(tweet, tweet_test)

sentiment = c(rep("positive", length(positive)), 
              rep("negative", length(negative)))

sentiment_test = c(rep("positive", length(positive_test) ), 
                   rep("negative", length(negative_test)))

sentiment_all = as.factor(c(sentiment, sentiment_test))

all_in_one = data.frame(sentiment_all, tweet_all)

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
                   removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE, toLower=TRUE, removeSparseTerms=0.999,
                   stemWords=TRUE, tm::weightTf)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:4800, testSize=4801:6200,virgin=FALSE)

#models = train_models(container, algorithms=c("MAXENT","SVM", "RF","TREE"))

models = train_models(container, algorithms=c("MAXENT","SVM", "RF"),
                      method = "eps-classification", #SVM
                      cross = 20, #SVM
                      cost = 100, #SVM
                      kernel= "radial", #SVM
                      #l1_regularizer= 0, #MAXENT
                      l2_regularizer = 1, #MAXENT
                      #use_sgd = FALSE, #MAXENT
                      #set_heldout = 0, #MAXENT
                      verbose = FALSE) #MAXENT


maxentmodel = train_models(container, algorithms=c("MAXENT"), nfold=50,
                      l2_regularizer = 2, #MAXENT
                      verbose = TRUE)

svm_pol = train_models(container, algorithms=c("SVM"),
                        method = "C-classification ",
                        cost = 1, 
                        degree = 3, 
                        gamma = 0.0001629195, 
                        coef.0 = 0, 
                        kernel= "polynomial"
                        )
svm_pol = train_models(container, algorithms=c("SVM"),
                       method = "C-classification ",
                       cost = 1, 
                       gamma = 0.0001629195, 
                       kernel= "radial"
)
svm_lin = train_models(container, algorithms=c("SVM"),
                       method = "C-classification ",
                       cost = 1, 
                       gamma = 0.0001629195, 
                       kernel= "linear"
)
svm_sig = train_models(container, algorithms=c("SVM"),
                       method = "C-classification ",
                       cost = 1, 
                       gamma = 0.0001629195, 
                       coef.0 = 0,
                       kernel= "sigmoid"
)

#obj <- tune(svm, sentiment_all~., data = all_in_one, kernel="radial",
#            #ranges = list(gamma = 2^(-8:4), cost = 2^(-8:8)),
#            ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)),
#            tunecontrol = tune.control(sampling = "fix")
#)

polytuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "polynomial") 
radtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "radial")
lintuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "linear")
sigtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "sigmoid")

rfmodel = train_model(container, algorithm=c("RF"))

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(nrow(all_in_one))
rf_random <- train(sentiment_all~., data=all_in_one, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# test the model
results = classify_models(container, models)

table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"SVM_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"SVM_LABEL"])
confusionMatrix(table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"SVM_LABEL"]))

table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"MAXENTROPY_LABEL"])
confusionMatrix(table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"MAXENTROPY_LABEL"]))

table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"FORESTS_LABEL"])
confusionMatrix(table(as.numeric(as.numeric(sentiment_all[4801:6200])), results[,"FORESTS_LABEL"]))

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary

# Cross Validation
# N=10
# cross_SVM = cross_validate(container,N,"SVM")
# cross_MAXENT = cross_validate(container,N,"MAXENT")


