source('library_package.R')
source('feature_engineer.R')
rm(list=ls())
library(xgboost)
library(caret)
library(data.table)
library(dplyr)
data <- read.csv("/home/knie/ctrip/competition_train.txt", sep="\t", na.strings="NULL", header=T)
data_test <- read.csv(file=('/home/knie/ctrip/competition_test.txt.bz2'), sep="\t", quote="", na.strings="NULL", header=T)

#######Divide data into three parts:##############
# 1. Order_table.
# 2. User_table.
# 3. Hotel_table.
##################################################
#######Order_table:
#######cols=c("orderid","uid","orderdate","hotelid","basicroomid","roomid" )
#######Discussion: should I include (rank	return	price	roomservice_2	roomservice_3	roomservice_4	roomservice_5	roomservice_6	roomservice_8	roomtag_2	roomtag_3	roomtag_4	roomtag_5	roomtag_6	star	hotel_minprice	basic_minprice)
##############################################################
###############################################################
cols=c("orderid",     
       "uid",         
       "orderdate",   
       "hotelid",     
       "basicroomid", 
       "roomid" )
data=as.data.table(data)
data_test=as.data.table(data_test)
########order in current train set####
order_TABLE_part1= fetch_order_info(data, cols)  ##order_TABLE_part1=data[(!sub)&(orderlabel==1),cols,with=F] IF THERE IS SAMPLING.
########last order in train set#######
order_TABLE_part2<- fetch_lastord_info(data)
##############last order in test set###########
order_TABLE_part3<- fetch_lastord_info(data_test)
######################Combine three table together
order_TABLE=rbind(order_TABLE_part1, order_TABLE_part2[,cols], order_TABLE_part3[,cols])
###or bind_rows

################################################################
###############################################################
################################################################
######User_table##########For inspiration, not finished yet###
##############################################################
user_table_train<- fetch_user_table_with_newfeature(data)
################################################################
###############################################################
################################################################
###Hotel_table########

hotel_col=c("hotelid",
            "basicroomid",
            "roomid",
            "star",
            "rank",
            "returnvalue",
            "price_deduct",
            "basic_minarea",
            "basic_maxarea",
            "roomservice_1",
            "roomservice_2",
            "roomservice_3",
            "roomservice_4",
            "roomservice_5",
            "roomservice_6",
            "roomservice_7",
            "roomservice_8",
            "roomtag_1",
            "roomtag_2",
            "roomtag_3",
            "roomtag_4",
            "roomtag_5",
            "roomtag_6")

hotel_TABLE_train<- fetch_hotel_info(data, hotel_col)
       
