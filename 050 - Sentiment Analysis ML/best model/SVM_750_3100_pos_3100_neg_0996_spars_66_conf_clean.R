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

# create matrix
mat <- create_matrix(tweet_all[,1], language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE, toLower=TRUE, removeSparseTerms=sparsity,
                   stemWords=TRUE, tm::weightTf)

# examine matrix
mat2 <- as.matrix(mat)
word_vector <- colnames(mat2)
df_words <- data.frame(words = colnames(mat2), frequency = colSums(mat2), row.names = NULL)
View(df_words)

container = create_container(mat, as.numeric(as.factor(tweet_all[,2])),
                             trainSize=(1:((tr_end-tr_start+1)*2)),
                             testSize=((((tr_end-tr_start+1)*2)+1):(nrow(tweet_all))),
                             virgin=FALSE)

start_time <- Sys.time()
models = train_models(container, algorithms=c("SVM"),
                      verbose = TRUE
)
end_time <- Sys.time()
end_time - start_time

################################################################################################################
#Tune with e1071
################################################################################################################

obj <- tune.svm(x = container@training_matrix,
                y = container@training_codes,
                kernel="radial",
                cost = 10^(-1:2),
                gamma = 10^(-6:-1)
                )

polytuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "polynomial") 
radtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "radial")
lintuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "linear")
sigtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "sigmoid")

################################################################################################################

# test the model
results <- classify_models(container, models)

# accuracy
analytics = create_analytics(container, results)
summary(analytics)

table(as.numeric(as.factor(tweet_all[(((tr_end-tr_start+1)*2)+1):(nrow(tweet_all)),2])),results[,"SVM_LABEL"])
recall_accuracy(as.numeric(as.factor(tweet_all[(((tr_end-tr_start+1)*2)+1):(nrow(tweet_all)),2])),results[,"SVM_LABEL"])
confusionMatrix(as.numeric(as.factor(tweet_all[(((tr_end-tr_start+1)*2)+1):(nrow(tweet_all)),2])),results[,"SVM_LABEL"])

################################################################################################################
#use bet SVM for whole dataset
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
  svmpred <- predict(svm_model, newmat, decision.values = TRUE, probability = TRUE)
  levels(svmpred)[levels(svmpred)=="2"] <- "positive"
  levels(svmpred)[levels(svmpred)=="1"] <- "negative"
  res = data.frame(nd$status_id, nd$tweet, svmpred, attr(svmpred, "decision.values"), attr(svmpred, "probabilities"))
  colnames(res) <- c("status_id","tweet", "svm_sentiment", "decisionValue", "pos_prob", "neg_prob")
  sqlSave(conn, res, tablename = "Final_SVM_sentiment", rownames = FALSE, append = TRUE)
}

