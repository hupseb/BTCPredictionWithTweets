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
sparsity <- 0.9999
#tr_start <- 1
#tr_end <- 2790
#tst_start <- 2791
#tst_end <- 3100
#fll_size <- 3100

conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinAnalysis;trusted_connection=true')

positive_all = sqlQuery(conn, paste("SELECT TOP 5200 [tweet], [sentiment], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'positive')"), as.is = TRUE)
positive <- positive_all[1:4900,1:2]
positive_test <- positive_all[4901:5200,1:2]

negative_all = sqlQuery(conn, paste("SELECT TOP 3100 [tweet], [sentiment], [_unit_id], [status_id]
FROM [BitcoinAnalysis].[dbo].[05. FigureEight_raw] AS TESTDATA
WHERE (relevant_yn LIKE 'yes') AND (relevant_yn_confidence = 1) AND (sentiment_confidence >", snt_conf, ") AND (sentiment LIKE 'negative')"), as.is = TRUE)
negative <- negative_all[1:2800,1:2]
negative_test <- negative_all[2801:3100,1:2]

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
                             trainSize=(1:7700),
                             testSize=(7701:8300),
                             virgin=FALSE)

models = train_models(container, algorithms=c("RF"),
                      #cross = 20,
                      #use_sgd = FALSE, #MAXENT
                      #set_heldout = 0, #MAXENT
                      verbose = TRUE)

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

obj <- tune(svm, train.x = as.matrix(mat),
                 train.y = as.factor(tweet_all[,2]),
                 #gamma = 2^(-1:1),
                 #cost = 2^(2:4),
                 #sampling = "fix",
                 kernel="radial")

obj2 <- best.tune(svm,
             train.x = container@training_matrix,
             train.y = container@training_codes,
             probability = TRUE,
             cost = 100
             #gamma = 2^(-1:1),
             #cost = 2^(2:4),
             #sampling = "fix",
             )

obj <- tune.svm(Species~., data = iris, sampling = "fix",
                gamma = 2^c(-8,-4,0,4), cost = 2^c(-8,-4,-2,0))


obj <- tune.randomForest(x = container@training_matrix,
                         y = container@training_codes,
                         mtry = 80,
                         nodesize = 2,
                         ntree = 300
)

obj <- tune.maxent(as.matrix(mat),
                   as.factor(tweet_all[,2]),
                   nfold=3,
                   showall=TRUE
)

polytuneres <- best.tune(svm, sentiment_all~., data = all_in_one, kernel = "polynomial") 
radtuneres <- best.tune(svm, as.matrix(mat), data = as.factor(tweet_all[,2]), kernel = "radial", tunecontrol = tune.control(sampling = "fix"))
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

################################################################################################################
#Train with randomForest package
################################################################################################################

set.seed(seed)
mtryStart <- 54
bestmtry <- tuneRF(x = as.matrix(mat), y = as.factor(tweet_all[,2]), mtryStart, ntreeTry=200, stepFactor=2, improve=0.01, plot=TRUE, trace=TRUE)
print(bestmtry)

mtryStart <- 54
bestmtry <- tuneRF(doBest=TRUE, x = as.matrix(container@training_matrix), y = container@training_codes, mtryStart, ntreeTry=200, stepFactor=2, improve=0.01, plot=TRUE, trace=TRUE)
print(bestmtry)

mtryStart <- 54
bestmtryFull <- tuneRF(doBest=TRUE, x = as.matrix(mat), y = as.factor(tweet_all[,2]), ntreeTry=200, stepFactor=2, improve=0.01, plot=TRUE, trace=TRUE)
print(bestmtryFull)

################################################################################################################


# test the model
results <- classify_model(container, obj2)
results <- classify_models(container, models)

# accuracy
analytics = create_analytics(container, results, b=1)
summary(analytics)

table(as.numeric(as.factor(tweet_all[7701:8300,2])),results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweet_all[7701:8300,2])),results[,"FORESTS_LABEL"])
confusionMatrix(as.numeric(as.factor(tweet_all[7701:8300,2])),results[,"FORESTS_LABEL"])


