rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)
data_test <- fread("/home/knie/data/data_test.txt", header=T)    ###Read test set. Fread is good for now.

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
xgbm<-xgb.load("Model_saved_with_new_feature")
ptest_final  <- predict(xgbm, dtest_final, outputmargin=TRUE)

final_test=data.table(orderid=data_test$orderid,roomid=data_test$roomid, predicted=ptest_final)
final_test=final_test[final_test[, .I[which.max(predicted)], by=orderid]$V1]
final_test_write=final_test %>% mutate(predicted=NULL)
final_test_write<- final_test_write %>% mutate(predict_roomid=roomid) %>% select(orderid, predict_roomid)
fwrite(final_test_write, 'submission_sample.csv')