options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages(c("ggplot2", "RODBC","devtools", "caret", "doParallel"))

devtools::install_github("timjurka/RTextTools/RTextTools", force = TRUE, ref="d09a339")

library("RODBC")
library("devtools")
library("RTextTools")
library("e1071")
library("caret")
library("randomForest")
library("doParallel")
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

models = train_models(container, algorithms=c("SVM"),
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

models = train_models(container, algorithms=c("SVM"))

################################################################################################################
#Tune with e1071
################################################################################################################

svm_pol = train_models(container, algorithms=c("SVM"),
                        method = "C-classification ",
                        kernel= "polynomial"
                        )
svm_pol = train_models(container, algorithms=c("SVM"),
                       method = "C-classification ",
                       cost = 10, 
                       gamma = 0.01, 
                       kernel= "radial"
)
svm_lin = train_models(container, algorithms=c("SVM"),
                       #method = "C-classification ",
                       cost = 0.1, 
                       gamma = 0.000001, 
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
                         mtry = 114,
                         ntree = 200
)

obj <- tune.maxent(as.matrix(mat),
                   as.factor(tweet_all[,2]),
                   nfold=3,
                   showall=TRUE
)

polytuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "polynomial") 
radtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "radial")
lintuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "linear")
sigtuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "sigmoid")

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
newdata = sqlQuery(conn, paste("SELECT * FROM [BitcoinTwts].[dbo].[BitcoinTweetsUnion_Filtered_OOSInterval] ORDER BY [status_id]"), as.is = TRUE)

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
  colnames(res) <- c("status_id","tweet", "sentiment", "decisionValue", "pos_prob", "neg_prob")
  sqlSave(conn, res, tablename = "Final_SVM_sentiment_OOS", rownames = FALSE, append = TRUE)

}

