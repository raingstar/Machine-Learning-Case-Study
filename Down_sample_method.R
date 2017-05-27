rm(list=ls())
gc()
library(data.table)
library(dplyr)
for i

train <- fread("~/training_new.csv", header=T)    ###Read training set. Fread is good for now.

########down sample negative part, choose only buck_id==group_id or positive sample
train <- train %>%
       mutate(buck_id=sample(seq(1,10),dim(train)[1],replace = T))
for (group_id in 1:10){ ###global variable, set it from 1 to 10
train_save<- train %>% 
       filter((buck_id==group_id & orderlabel==0) | ( orderlabel==1) ) %>%
       mutate(buck_id=NULL)
fwrite(train_save, paste("training_new_sub",group_id,sep = "_") )
}
############################################################################
