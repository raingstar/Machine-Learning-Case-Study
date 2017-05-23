rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
library(feather)


train <- fread("/users/knie/training_new.csv", header=T)    ###Read training set. Fread is good for now.
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
rm(training)
gc()



######Test set
test  <- fread("/users/knie/validate_new.csv", header=T) ###Load validate data.
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
rm(testing)
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

xgbm <- xgb.train(
  missing = NA,
  data = dtrain,
  eta = 0.03,
  max_depth = 8,
  nround=2000,
  subsample = 0.75,
  colsample_bytree = 0.75,
  scale_pos_weight = 30, # data skewed, 35:1
  eval_metric = evalerror ###"auc"
  ,objective = logregobj ###"binary:logistic"
  ,watchlist=watchlist
  ,early_stopping_rounds = 200
  ,maximize = TRUE      ####Customized object should set maximize or minimize.
)

xgb.save(xgbm, "Model_saved_with_new_feature") ###Save the model


xgbm<-xgb.load("Model_saved_with_new_feature")
xgb.importance(names(training), model = xgbm)
ptest<- predict(xgbm, dtest, outputmargin=TRUE,ntreelimit=xgbm$bestInd)
ptrain<- predict(xgbm, dtrain, outputmargin=TRUE,ntreelimit=xgbm$bestInd)
setinfo(dtrain, "base_margin", ptrain)
setinfo(dtest, "base_margin", ptest)

xgb.DMatrix.save(dtrain, "dtrain")
xgb.DMatrix.save(dtest, "dtest")
