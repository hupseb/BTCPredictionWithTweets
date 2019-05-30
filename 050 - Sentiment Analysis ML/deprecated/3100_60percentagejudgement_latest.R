options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages(c("ggplot2", "RODBC","devtools", "RTextTools", "caret", "doParallel"))

library("RODBC")
library("devtools")
library("RTextTools")
library("e1071")
library("caret")
library("randomForest")
library("doParallel")
library("RTextTools")

#######################################################################################
#######################################################################################
#######################################################################################

tst_size <- 240
fll_size <- 310
snt_conf <- 0.66
sparsity <- 0.997

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')

#select top(100) ClumnName from Table1 where ClumnName NOT IN (select top (100) ClumnName from Table1 )

positive_all = sqlQuery(conn, paste("SELECT TOP", fll_size, "[tweet], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'positive') 
--AND [_unit_id] NOT IN(
--         SELECT TOP", fll_size, "[_unit_id]
--         FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
--         WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'positive'))
"), as.is = TRUE)
positive <- positive_all[1:tst_size,1]
positive_test <- positive_all[(tst_size+1):fll_size,1]

negative_all = sqlQuery(conn, paste("SELECT TOP", fll_size, "[tweet], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'negative') 
--AND [_unit_id] NOT IN(
--         SELECT TOP", fll_size, "[_unit_id]
--         FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
--         WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'negative')                         
--)
"), as.is = TRUE)
negative <- negative_all[1:tst_size,1]
negative_test <- negative_all[(tst_size+1):fll_size,1]

#######################################################################################

tweet <- c(positive, negative)
tweet_test <- c(positive_test, negative_test)
tweet_all <- c(tweet, tweet_test)

sentiment <- c(rep("positive", length(positive)), 
              rep("negative", length(negative)))

sentiment_test <- c(rep("positive", length(positive_test) ), 
                   rep("negative", length(negative_test)))

sentiment_all <- as.factor(c(sentiment, sentiment_test))

all_in_one = data.frame(sentiment_all, tweet_all)

########################################################################################


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
mat <- create_matrix(tweet_all, language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE, toLower=TRUE, removeSparseTerms=sparsity,
                   stemWords=TRUE, tm::weightTf)

# examine mat
mat2 <- as.matrix(mat)
word_vector <- colnames(mat2)
df_words <- data.frame(words = colnames(mat2), frequency = colSums(mat2), row.names = NULL)
View(df_words)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:(2*tst_size), testSize=(2*tst_size+1):(fll_size*2),virgin=FALSE)

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

obj <- tune.svm(x = container@training_matrix,
                y = container@training_codes,
                kernel="radial",
                cost = 10^(-1:2),
                gamma = 10^(-6:-1)
                )

obj <- tune.randomForest(x = container@training_matrix,
                         y = container@training_codes,
                         mtry = 80,
                         nodesize = 2,
                         ntree = 300
)

polytuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "polynomial") 
radtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "radial")
lintuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "linear")
sigtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "sigmoid")

rfmodel = train_models(container, algorithm=c("RF"),
                       mtry = 80,
                       nodesize = 2,
                       ntree = 300
)

# Create model with default paramters
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  mtry <- sqrt(ncol(as.matrix(mat)))
  #mtry <- 5
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(x = as.matrix(mat), y = as.matrix(sentiment_all), method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  print(rf_default)
stopCluster(cl)


# Random Search
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random", allowParallel = TRUE)
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  rf_random <- train(x = as.matrix(mat), y = as.matrix(sentiment_all), method="rf", metric=metric, tuneLength=15, trControl=control)
  print(rf_random)
  plot(rf_random)
stopCluster(cl)

set.seed(seed)
bestmtry <- tuneRF(x = as.matrix(mat), y = as.factor(sentiment_all), improve=1e-5, ntree=500)
print(bestmtry)

# test the model
results = classify_models(container, models)

table(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"SVM_LABEL"])
recall_accuracy(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"SVM_LABEL"])
confusionMatrix(table(as.numeric(sentiment_all[(2*tst_size+1):(fll_size*2)]), as.numeric(results[,"SVM_LABEL"])))

table(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"MAXENTROPY_LABEL"])
recall_accuracy(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"MAXENTROPY_LABEL"])
confusionMatrix(table(as.numeric(sentiment_all[(2*tst_size+1):(fll_size*2)]), as.numeric(results[,"MAXENTROPY_LABEL"])))

table(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"FORESTS_LABEL"])
recall_accuracy(sentiment_all[(2*tst_size+1):(fll_size*2)], results[,"FORESTS_LABEL"])
confusionMatrix(table(as.numeric(sentiment_all[(2*tst_size+1):(fll_size*2)]), as.numeric(results[,"FORESTS_LABEL"])))

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


