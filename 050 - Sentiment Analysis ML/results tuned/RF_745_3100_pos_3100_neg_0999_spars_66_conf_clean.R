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
library("foreach")
library("maxent")

################################################################################################################

# snt_conf <- 0.66
# sparsity <- 0.9999
# tr_start <- 1
# tr_end <- 500
# tst_start <- 501
# tst_end <- 600
# fll_size <- 3100
snt_conf <- 0.66
sparsity <- 0.9996
tr_start <- 1
tr_end <- 2790
tst_start <- 2791
tst_end <- 3100
fll_size <- 3100

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')

positive_all = sqlQuery(conn, paste("SELECT TOP", fll_size, "[tweet], [sentiment], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'positive')"), as.is = TRUE)
positive <- positive_all[tr_start:tr_end,1:2]
positive_test <- positive_all[tst_start:tst_end,1:2]

negative_all = sqlQuery(conn, paste("SELECT TOP", fll_size, "[tweet], [sentiment], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'negative')"), as.is = TRUE)
negative <- negative_all[tr_start:tr_end,1:2]
negative_test <- negative_all[tst_start:tst_end,1:2]

tweet_all <- rbind(positive[,1:2], negative[,1:2], positive_test[,1:2], negative_test[,1:2])
rownames(tweet_all) <- 1:nrow(tweet_all)

################################################################################################################

# the other methods
mat <- create_matrix(tweet_all[,1], language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE, toLower=TRUE, removeSparseTerms=sparsity,
                   stemWords=TRUE, tm::weightTf)

# examine mat
mat2 <- as.matrix(mat)
word_vector <- colnames(mat2)
df_words <- data.frame(words = colnames(mat2), frequency = colSums(mat2), row.names = NULL)
View(df_words)

container = create_container(mat, as.numeric(as.factor(tweet_all[,2])),
                             trainSize=(1:((tr_end-tr_start+1)*2)),
                             testSize=((((tr_end-tr_start+1)*2)+1):(nrow(tweet_all))),
                             virgin=FALSE)

start_time <- Sys.time()
models = train_models(container, algorithms=c("RF"),
                      #method = "eps-classification", #SVM
                      #cross = 20, #SVM
                      #cost = 100, #SVM
                      #kernel= "radial", #SVM
                      #l1_regularizer= 0, #MAXENT
                      #l2_regularizer = 1, #MAXENT
                      #use_sgd = FALSE, #MAXENT
                      #set_heldout = 0, #MAXENT
                      verbose = TRUE
                      ) #MAXENT
end_time <- Sys.time()
end_time - start_time

################################################################################################################
#Tune with e1071
################################################################################################################

obj <- tune.randomForest(x = container@training_matrix,
                         y = container@training_codes,
                         mtry = 114,
                         ntree = 200
)

################################################################################################################
#Train with caret: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
################################################################################################################

# Random Search
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
  control <- trainControl(method="repeatedcv", search="random", allowParallel = TRUE)
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)
  rf_random <- train(x = as.matrix(mat), y = as.matrix(as.factor(tweet_all[,2])), method="rf", metric=metric, tuneLength=15, trControl=control)
  print(rf_random)
  plot(rf_random)
stopCluster(cl)

#GridSearch
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
  control <- trainControl(method="repeatedcv", number=3, repeats=1, search="grid")
  seed <- 7
  metric <- "Accuracy"
  tunegrid <- expand.grid(.mtry=c(109:119))
  rf_gridsearch <- train(x = as.matrix(mat), y = as.matrix(as.factor(tweet_all[,2])), method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  print(rf_gridsearch)
  plot(rf_gridsearch)
stopCluster(cl)

mtryStart <- 29
bestmtry <- tuneRF(x = as.matrix(mat), y = as.factor(tweet_all[,2]), mtryStart, ntreeTry=400, stepFactor=2, improve=0.01, plot=TRUE, trace=TRUE)
print(bestmtry)

mtryStart <- 114
bestmtry <- tuneRF(x = as.matrix(container@training_matrix), y = container@training_codes, mtryStart, ntreeTry=200, stepFactor=2, improve=0.01, plot=TRUE, trace=TRUE)
print(bestmtry)

################################################################################################################


# test the model
results <- classify_models(container, models)

# accuracy
analytics = create_analytics(container, results)
summary(analytics)

################################################################################################################

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
newdata = sqlQuery(conn, paste("SELECT * FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered_FinalInterval] ORDER BY [status_id]"), as.is = TRUE)

n <- 1000
nr <- nrow(newdata)
newdata_sp = split(newdata, rep(1:ceiling(nr/n), each=n, length.out=nr))

svm_model <- models$SVM

for(i in 1:length(newdata_sp)){
  nd <- newdata_sp[[i]]
  print(nd[1,3])
  
  #use original matrix, but with new tweets
  newmat <- create_matrix(nd$tweet, originalMatrix = mat)
  #predict for rest of all tweets
  rfprobs = predict(bestmtryFull, newmat, type="prob")
  rfresp = predict(bestmtryFull, newmat, type="response")
  res = data.frame(nd$status_id, nd$tweet, rfresp, rfprobs[,1], rfprobs[,2], rfprobs[,2] - rfprobs[,1])
  colnames(res) <- c("status_id","tweet", "rf_sentiment", "neg_prob", "pos_prob", "pos-neg")
  sqlSave(conn, res, tablename = "Final_RF_sentiment_RF", rownames = FALSE, append = TRUE)
}

