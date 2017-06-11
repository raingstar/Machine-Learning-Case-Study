rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
library(feather)


train <- fread("/home/ubuntu/data/training_new_0604.csv", header=T)    ###Read training set. Fread is good for now.
train<- train[train[, .I[which.max(orderlabel)], by=.(orderid,basicroomid)]$V1]
nms = names(train)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
training <- train[ ,! names(train) %in% columns_excluded, with=FALSE]
train %>% select(orderid, uid, orderdate, hotelid, basicroomid, roomid) ->train
training<-training %>% mutate_each_(funs(as.numeric), names(training))  ##Change all to float type.
gc() ## Save memory
target_train=training$orderlabel
training$orderlabel=NULL
gc()
training=as.matrix(training)
gc()
dtrain = xgb.DMatrix(data=training, label=target_train, missing = NA)
gc()



######Test set
test  <- fread("/home/ubuntu/data/validate_new_0604.csv", header=T) ###Load validate data.
test<- test[test[, .I[which.max(orderlabel)], by=.(orderid,basicroomid)]$V1]
testing <- test[ , !nms %in% columns_excluded, with=FALSE]
test %>% select(orderid, uid, orderdate, hotelid, basicroomid, roomid) ->test
gc()
testing<-testing %>%
  mutate_each_(funs(as.numeric), names(testing))  ##Change all to float type
target_test=testing$orderlabel
testing$orderlabel=NULL
gc()
testing=as.matrix(testing)
gc()
dtest = xgb.DMatrix(data=testing, label= target_test, missing = NA)
gc()
watchlist <- list(train = dtrain, test = dtest)


###############################User customized objective and error#######
logregobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}
orderid_test<-test$orderid  ###Global variable
orderid_train<-train$orderid ###
len_test=length(orderid_test)
len_train=length(orderid_train)

calculate_error_rate <-function(orderdata){
  a = orderdata[orderdata[, .I[which.max(predictedorderlabel)], by=orderid]$V1]
  gc()
  return (a[,sum(orderlabel)]/nrow(a))
}
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  if (length(labels)==len_test) {orderid=orderid_test}
  if (length(labels)==len_train) {orderid=orderid_train}
  orderdata=data.table(orderid=orderid, orderlabel=labels, predictedorderlabel=preds)
  err <- calculate_error_rate(orderdata)
  rm(orderdata)
  gc()
  return(list(metric = "error", value = err))
}

################################xgboost traning##################
set.seed(123)
xgbm <- xgb.train(
  missing = NA,
  data = dtrain,
  eta = 0.03,
  max_depth = 8,
  nround=2000,
  subsample = 0.75,
  colsample_bytree = 0.75,
  eval_metric = evalerror ###"auc"
  ,objective = logregobj ###"binary:logistic"
  ,watchlist=watchlist
  ,early_stopping_rounds = 200
  ,maximize = TRUE      ####Customized object should set maximize or minimize.
)

set.seed(123)
xgbm_1 <- xgb.train(
  missing = NA,
  data = dtrain,
  eta = 0.03,
  max_depth = 8,
  nround=2000,
  subsample = 0.75,
  colsample_bytree = 0.75,
  eval_metric = "auc"
  ,objective = "binary:logistic"
  ,watchlist=watchlist
  ,early_stopping_rounds = 200
)

xgb.importance(names(training), model = xgbm)
ptest<- predict(xgbm, dtest, outputmargin=TRUE,ntreelimit=xgbm$best_iteration)
ptrain<- predict(xgbm, dtrain, outputmargin=TRUE,ntreelimit=xgbm$best_iteration)
ptest_auc<- predict(xgbm_1, dtest, outputmargin=TRUE,ntreelimit=xgbm_1$best_iteration)
ptrain_auc<- predict(xgbm_1, dtrain, outputmargin=TRUE,ntreelimit=xgbm_1$best_iteration)

basicroom_valid=data.table(orderid=test$orderid,basicroomid=test$basicroomid, predicted=ptest)
fwrite(basicroom_valid, "basicroom_valid.csv" )

basicroom_training=data.table(orderid=train$orderid,basicroomid=train$basicroomid, predicted=ptrain)
fwrite(basicroom_training, "basicroom_training.csv" )

basicroom_valid_auc=data.table(orderid=test$orderid,basicroomid=test$basicroomid, predicted=ptest_auc)
fwrite(basicroom_valid_auc, "basicroom_valid_auc.csv" )

basicroom_training_auc=data.table(orderid=train$orderid,basicroomid=train$basicroomid, predicted=ptrain_auc)
fwrite(basicroom_training_auc, "basicroom_training_auc.csv" )


######please manually record xgbm$bestInd for each subgroup train set !!!!################

data_test <- fread("/home/ubuntu/data/data_test_0604.txt", header=T)    ###Read test set. Fread is good for now.
data_test<- data_test %>% group_by(orderid, basicroomid) %>% filter(row_number(roomid) == 1)

nms = names(data_test)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
data_testing <- data_test[ ,!names(data_test) %in% columns_excluded]
data_test %>% select(orderid, uid, orderdate, hotelid, basicroomid, roomid) ->data_test
data_testing<-data_testing %>% mutate_each_(funs(as.numeric), names(data_testing))  ##Change all to float type.
gc() ## Save memory
gc()
data_testing=as.matrix(data_testing)
gc()
dtest_final = xgb.DMatrix(data=data_testing, missing = NA)
rm(data_testing)
gc()
##xgbm<-xgb.load(model_name)   ### Model_saved_groupid_1 to 10, based on previous groupid=1 - 10
ptest_final  <- predict(xgbm, dtest_final, outputmargin=TRUE, ntreelimit=xgbm$best_iteration)
final_test=data.table(orderid=data_test$orderid,basicroomid=data_test$basicroomid, predicted=ptest_final)
fwrite(final_test, "basicroom_final_test.csv" )
ptest_final_auc  <- predict(xgbm_1, dtest_final, outputmargin=TRUE, ntreelimit=xgbm$best_iteration)
final_test_auc=data.table(orderid=data_test$orderid,basicroomid=data_test$basicroomid, predicted=ptest_final_auc)
fwrite(final_test_auc, "basicroom_final_test_auc.csv" )

