####Data cleaning####
rm(list=ls())
gc()
source("/home/knie/ML_case_study/Machine-Learning-Case-Study/Data_engineering.R")
library(xgboost)
library(data.table)
library(dplyr)
data<-fread("/Users/knie/Downloads/competition_train.txt", sep="\t", na.strings="NULL", header=T)###, nrows=10000) ##Load original data
nms = names(data)
columns_excluded = c('orderid', 'uid', 'orderdate', 'hotelid', 'basicroomid', 'roomid', nms[grep("lastord",nms)])
data<- data %>% mutate_each_(funs(as.numeric), nms[! nms %in% columns_excluded]) ##change cols type as numeric
data<-feature_clean_0(data)
gc()
data<- feature_clean_1(data)
gc() ##free memory
data<- feature_clean_2(data)
gc() ##free memory
data<- feature_clean_3(data)
gc() ##free memory
data<- feature_clean_4(data)
gc() ##free memory
gc() ##free memory
data_1<- feature_clean_rnd2_1(data)  ###group by orderid
gc() ##free memory
data_2<- feature_clean_rnd2_2(data) ###group by orderid and basciroomid
gc() ##free memory

data<- data %>% left_join(data_1) ####add new feature to original table
rm(data_1)
gc()
data<- data %>% left_join(data_2) ####add new feature to original table
rm(data_2)
gc()
data<- feature_clean_rnd2_3(data) ####new feature generated.
gc()
sub<-c(data$orderdate=="2013-04-20") ### split based on date, use the last day as test set.
training<-data[!sub,]
validating<-data[sub,]
fwrite(training, 'training_new.csv')
fwrite(validating, 'validate_new.csv')

