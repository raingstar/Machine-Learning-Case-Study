rm(list=ls())
gc()
source("/home/knie/ML_case_study/Machine-Learning-Case-Study/data_test_engineering.R")
library(xgboost)
library(data.table)
library(dplyr)
data_test <- fread(file=('/home/knie/data/competition_test.txt'), sep="\t", quote="", na.strings="NULL", header=T)

nms = names(data_test)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
data_test<- data_test %>% mutate_each_(funs(as.numeric), nms[! nms %in% columns_excluded]) ##change cols type as numeric
data_test<-feature_clean_0(data_test)
gc()
data_test<- feature_clean_1(data_test)
gc() ##free memory
data_test<- feature_clean_2(data_test)
gc() ##free memory
data_test<- feature_clean_3(data_test)
gc() ##free memory
data_test<- feature_clean_4(data_test)
gc() ##free memory
gc() ##free memory
data_test_1<- feature_clean_rnd2_1(data_test)  ###group by orderid
gc() ##free memory
data_test_2<- feature_clean_rnd2_2(data_test) ###group by orderid and basciroomid
gc() ##free memory

data_test<- data_test %>% left_join(data_test_1) ####add new feature to original table
rm(data_test_1)
gc()
data_test<- data_test %>% left_join(data_test_2) ####add new feature to original table
rm(data_test_2)
gc()
data_test<- feature_clean_rnd2_3(data_test) ####new feature generated.
gc()
fwrite(data_test, 'data_test.txt')
