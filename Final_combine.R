rm(list=ls())
gc()
library(xgboost)
library(data.table)
library(dplyr)


refine_feature<-function(df){
  df %>%
    group_by(orderid) %>%
    summarise(max_predict_group_1=max(predict_group_1),
              max_predict_group_2=max(predict_group_2),
              max_predict_group_3=max(predict_group_3),
              max_predict_group_4=max(predict_group_4),
              max_predict_group_5=max(predict_group_5),
              max_predict_group_6=max(predict_group_6),
              max_predict_group_7=max(predict_group_7),
              max_predict_group_8=max(predict_group_8),
              max_predict_group_9=max(predict_group_9),
              max_predict_group_10=max(predict_group_10),
              max_basic_predicted=max(basic_predicted),
              max_basic_predicted_auc=max(basic_predicted_auc)) %>%
    ungroup()
}

combine_1<-function(df, group, basic, basic_auc){
  for (k in 1:10){
    df <- df %>% 
      left_join(group[[k]]) %>%
      rename_(.dots=setNames("predicted", paste("predict_group",k,sep = "_")))
  }
  df <- df %>% 
    left_join(basic) %>%
    rename_(.dots=setNames("predicted", "basic_predicted")) 
  
  df <- df %>% 
    left_join(basic_auc) %>%
    rename_(.dots=setNames("predicted", "basic_predicted_auc")) 
  return(df)
}

combine_2<-function(df, df_refine){
  df %>% left_join(df_refine) %>%
    mutate(predict_group_1_cutoff_1=if_else(predict_group_1==max_predict_group_1,1,0,NA_real_),
           predict_group_2_cutoff_1=if_else(predict_group_2==max_predict_group_2,1,0,NA_real_),
           predict_group_3_cutoff_1=if_else(predict_group_3==max_predict_group_3,1,0,NA_real_),
           predict_group_4_cutoff_1=if_else(predict_group_4==max_predict_group_4,1,0,NA_real_),
           predict_group_5_cutoff_1=if_else(predict_group_5==max_predict_group_5,1,0,NA_real_),
           predict_group_6_cutoff_1=if_else(predict_group_6==max_predict_group_6,1,0,NA_real_),
           predict_group_7_cutoff_1=if_else(predict_group_7==max_predict_group_7,1,0,NA_real_),
           predict_group_8_cutoff_1=if_else(predict_group_8==max_predict_group_8,1,0,NA_real_),
           predict_group_9_cutoff_1=if_else(predict_group_9==max_predict_group_9,1,0,NA_real_),
           predict_group_10_cutoff_1=if_else(predict_group_10==max_predict_group_10,1,0,NA_real_),
           max_basic_predicted_cutoff_1=if_else(basic_predicted==max_basic_predicted,1,0,NA_real_),
           max_basic_predicted_auc_cutoff_1=if_else(basic_predicted_auc==max_basic_predicted_auc,1,0,NA_real_)
           )
}


train <- fread("/home/ubuntu/data/training_new_0604.csv", header=T)    ###Read training set. Fread is good for now.
test  <- fread("/home/ubuntu/data/validate_new_0604.csv", header=T)    ###Load validate data.
id_list_files=Sys.glob('/home/ubuntu/data/group_id*')
groups <- list()
for (k in 1:length(id_list_files)){
  groups[[k]] <- fread(id_list_files[k],header=T)
}
basicroom_training= fread("basicroom_training.csv", header=T) 
basicroom_training_auc= fread("basicroom_training_auc.csv", header=T) 
basicroom_valid=fread("basicroom_valid.csv", header=T)
basicroom_valid_auc=fread("basicroom_valid_auc.csv", header=T)
gc()
gc()
train=combine_1(train, groups, basicroom_training, basicroom_training_auc)
test=combine_1(test,groups,basicroom_valid,basicroom_valid_auc)

train_refine=refine_feature(train)
test_refine=refine_feature(test)
train=combine_2(train,train_refine)
test=combine_2(test,test_refine)
fwrite(train, "training_new_0608.csv" )
fwrite(test,  "validate_new_0608.csv" )

##############################################Final_test
data_test <- fread("/home/ubuntu/data/data_test_0604.txt", header=T)    ###Read test set. Fread is good for now.
basicroom_final_test <- fread("basicroom_final_test.csv", header=T)    ###Read test set. Fread is good for now.
basicroom_final_test_auc <- fread("basicroom_final_test_auc.csv", header=T)    ###Read test set. Fread is good for now.id_list_files_final_test=Sys.glob('/home/ubuntu/data/final_test_group_id*')
id_list_files_final_test=Sys.glob('/home/ubuntu/data/final_test_group_id*')
groups_final_test <- list()
for (k in 1:length(id_list_files_final_test)){
  groups_final_test[[k]] <- fread(id_list_files_final_test[k],header=T)
}
gc()
data_test=combine_1(data_test, groups_final_test, basicroom_final_test, basicroom_final_test_auc)
data_test_refine=refine_feature(data_test)
data_test=combine_2(data_test,data_test_refine)
fwrite(data_test, "data_test_new_0608.csv" )


