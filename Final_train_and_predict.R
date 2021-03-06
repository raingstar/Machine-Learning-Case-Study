rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
train <- fread("training_new_0608.csv", header=T)    ###Read training set. Fread is good for now.

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
test  <- fread("validate_new_0608.csv", header=T) ###Load validate data.
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
calculate_error_rate <-function(orderdata){
  a = orderdata[orderdata[, .I[which.max(predictedorderlabel)], by=orderid]$V1]
  gc()
  return (a[,sum(orderlabel)]/nrow(a))
}
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  if (length(labels)==1074769) {orderid=orderid_test}
  if (length(labels)==6650106) {orderid=orderid_train}
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
  nround=10000,
  subsample = 0.75,
  colsample_bytree = 0.75,
  scale_pos_weight = 30, # data skewed, 35:1
  eval_metric = evalerror ###"auc"
  ,objective = logregobj ###"binary:logistic"
  ,watchlist=watchlist
  ,early_stopping_rounds = 200
  ,maximize = TRUE      ####Customized object should set maximize or minimize.
)

xgb.save(xgbm, "Model_0608") ###Save the model


data_test <- fread("data_test_new_0608.csv", header=T)    ###Read test set. Fread is good for now.

nms = names(data_test)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
data_testing <- data_test[ ,!names(data_test) %in% columns_excluded, with=FALSE]
data_test %>% select(orderid, uid, orderdate, hotelid, basicroomid, roomid) ->data_test
data_testing<-data_testing %>% mutate_each_(funs(as.numeric), names(data_testing))  ##Change all to float type.
gc() ## Save memory
gc()
data_testing=as.matrix(data_testing)
gc()
dtest_final = xgb.DMatrix(data=data_testing, missing = NA)
gc()
#xgbm<-xgb.load("Model_0608_296feature")
ptest_final  <- predict(xgbm, dtest_final, outputmargin=TRUE,ntreelimit=xgbm$best_iteration)
final_test=data.table(orderid=data_test$orderid,roomid=data_test$roomid, predicted=ptest_final)
final_test=final_test[final_test[, .I[which.max(predicted)], by=orderid]$V1]
final_test_write=final_test %>% mutate(predicted=NULL)
final_test_write<- final_test_write %>% mutate(predict_roomid=roomid) %>% select(orderid, predict_roomid)
fwrite(final_test_write, 'submission_sample_0608.csv')

