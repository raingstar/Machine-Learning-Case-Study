rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
library(feather)
group_id=1   ###global variable, set it from 1 to 10
train <- fread("~/training_new.csv", header=T)    ###Read training set. Fread is good for now.

########down sample negative part, choose only buck_id==group_id or positive sample
train<- train %>% 
       mutate(buck_id=sample(seq(1,10),dim(train)[1],replace = T)) %>%
       filter((buck_id==group_id & orderlabel==0) | ( orderlabel==1) ) %>%
       mutate(buck_id=NULL)
############################################################################
nms = names(train)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
training <- train %>% select_( .dots=nms[!nms %in% columns_excluded])
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
test  <- fread("~/validate_new.csv", header=T)    ###Load validate data.
testing <- test %>% select_( .dots=nms[!nms %in% columns_excluded]) %>% mutate(buck_id=NULL)
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
len_test=length(test$orderid)
len_train=length(train$orderid)
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

xgbm <- xgb.train(
  missing = NA,
  data = dtrain,
  eta = 0.03,
  max_depth = 8,
  nround=2000,
  subsample = 0.75,
  colsample_bytree = 0.75,
 ## scale_pos_weight = 30, # data skewed, 35:1
  eval_metric = evalerror ###"auc"
  ,objective = logregobj ###"binary:logistic"
  ,watchlist=watchlist
  ,early_stopping_rounds = 200
  ,maximize = TRUE      ####Customized object should set maximize or minimize.
)

ptest<- predict(xgbm, dtest, outputmargin=TRUE,ntreelimit=xgbm$bestInd)
######Reload the whole train set to do the prediction
rm(train, dtrain)
gc()
train <- fread("~/training_new.csv", header=T)    ###Read training set. Fread is good for now.
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
ptrain<- predict(xgbm, dtrain, outputmargin=TRUE, ntreelimit=xgbm$bestInd)

final=data.table(orderid=c(train$orderid, test$orderid),roomid=c(train$roomid,test$roomid ), predicted=c(ptrain,ptest))
fwrite(final, paste("group_id",group_id,sep = "_") )
