rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
group_id=1
train <- fread("~/training_new_sub_1.csv", header=T)    ###Read down sampled training set. Please manually changed it from 1-10.
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

######Validate set ####################
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
set.seed(123)
xgbm <- xgb.train( ##skiped scale_pos_weight
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

#######################Save the model, Important !!!!!!##########
model_name=paste("Model_saved_groupid",group_id,sep = "_")
xgb.save(xgbm, model_name)

#######################Prediction on validate set##########
ptest<- predict(xgbm, dtest, outputmargin=TRUE, ntreelimit=xgbm$bestInd) ## It is actually the validate set

#########Prediction on train (need to reload the whole train set, not train_sub)### 
rm(train, dtrain)
gc()
train <- fread("~/training_new.csv", header=T)    ###Read whole training set. I resue the same parameter name to save efforts
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

##################################################################################
rm(dtrain, dtest, target_test,target_train)
gc()


#########################Combine everything togethre######################
final=data.table(orderid=c(train$orderid, test$orderid),roomid=c(train$roomid,test$roomid ), predicted=c(ptrain,ptest))
fwrite(final, paste("group_id",group_id,sep = "_") )

######Prediction on whole Test set can be done later !!!!!!!!!################
######please manually record xgbm$bestInd for each subgroup train set !!!!################

data_test <- fread("~/data_test.txt", header=T)    ###Read test set. Fread is good for now.
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
rm(data_testing)
gc()
xgbm<-xgb.load(model_name)   ### Model_saved_groupid_1 to 10, based on previous groupid=1 - 10
ptest_final  <- predict(xgbm, dtest_final, outputmargin=TRUE, ntreelimit=xgbm$bestInd)
final_test=data.table(orderid=data_test$orderid,roomid=data_test$roomid, predicted=ptest_final)
fwrite(final_test, paste("final_test_group_id",group_id,sep = "_") )



