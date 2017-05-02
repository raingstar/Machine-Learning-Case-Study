fetch_order_info<-function(df, fetched_cols){
  df=as.data.table(df)
  tmp=df[orderlabel==1, fetched_cols, with=F]
  return(tmp)
}


fetch_lastord_info <- function(df) { ####Return all cols end with "lastord" 
 tmp <- df %>% 
    select(ends_with('lastord'), uid) %>%
    rename_(.dots=setNames(names(.), tolower(gsub("_lastord", "", names(.))))) %>%
    filter(!is.na(orderid)) %>%  ##makesure orderid_lastord is not NA.
    distinct(orderid, uid, .keep_all = TRUE) ####%>%
    ###select_(.dots=cols)
 return(tmp)
}

fetch_hotel_info<-function(df, fetched_cols) {
  tmp<- df %>% 
    select_(.dots=fetched_cols) %>%
    distinct(hotelid, basicroomid, roomid, .keep_all = TRUE)
  return(tmp)
}

fetch_user_table_with_newfeature <-function(df){
  
 tmp<- df %>% 
    select(orderid, orderdate, uid, starts_with('user')) %>%
    group_by(uid) %>%
    arrange(desc(orderdate)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(user_roomservice_8_345ratio=user_roomservice_5_345ratio,
           user_roomservice_5_345ratio=NULL,
           user_roomservice_8_2ratio=1-user_roomservice_8_345ratio-user_roomservice_8_1ratio
    ) %>% ########First define some missed cols, correct typos 
    mutate(
      user_roomservice_2_prefer=if_else(user_roomservice_2_1ratio <=0.5, 0, 1, NA_real_),
      user_roomservice_3_prefer=if_else(user_roomservice_3_123ratio <=0.5, 0, 1, NA_real_),
      user_roomservice_4_prefer=case_when(
        .$user_roomservice_4_0ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 0,
        .$user_roomservice_4_1ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 1,
        .$user_roomservice_4_2ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 2,
        .$user_roomservice_4_3ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 3,
        .$user_roomservice_4_4ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 4,
        .$user_roomservice_4_5ratio==max(.$user_roomservice_4_0ratio, 
                                         .$user_roomservice_4_1ratio, 
                                         .$user_roomservice_4_2ratio,
                                         .$user_roomservice_4_3ratio, 
                                         .$user_roomservice_4_4ratio,
                                         .$user_roomservice_4_5ratio) ~ 5,
        TRUE                         ~ NA_real_),
      user_roomservice_5_prefer=if_else(user_roomservice_5_1ratio <=0.5, 0, 1, NA_real_),
      user_roomservice_6_prefer=case_when(
        .$user_roomservice_6_0ratio==max(.$user_roomservice_6_0ratio, 
                                         .$user_roomservice_6_1ratio, 
                                         .$user_roomservice_6_2ratio) ~ 0,
        .$user_roomservice_6_1ratio==max(.$user_roomservice_6_0ratio, 
                                         .$user_roomservice_6_1ratio, 
                                         .$user_roomservice_6_2ratio) ~ 1,
        .$user_roomservice_6_2ratio==max(.$user_roomservice_6_0ratio, 
                                         .$user_roomservice_6_1ratio, 
                                         .$user_roomservice_6_2ratio) ~ 2,
        TRUE                         ~ NA_real_),
      user_roomservice_7_prefer=if_else(user_roomservice_7_0ratio <=0.5, 1, 0, NA_real_),
      user_roomservice_8_prefer=case_when(
        .$user_roomservice_8_1ratio==max(.$user_roomservice_8_1ratio, 
                                         .$user_roomservice_8_2ratio, 
                                         .$user_roomservice_8_345ratio) ~ 1,
        .$user_roomservice_8_2ratio==max(.$user_roomservice_8_1ratio, 
                                         .$user_roomservice_8_2ratio, 
                                         .$user_roomservice_8_345ratio) ~ 2,
        .$user_roomservice_8_345ratio==max(.$user_roomservice_8_1ratio, 
                                           .$user_roomservice_8_2ratio, 
                                           .$user_roomservice_8_345ratio) ~ 3, ###It means actually >=3
        TRUE                           ~ NA_real_)
      # ,user_roomservice_2_1ratio=NULL,
      # user_roomservice_3_123ratio=NULL,
      # user_roomservice_4_0ratio=NULL,
      # user_roomservice_4_1ratio=NULL,
      # user_roomservice_4_2ratio=NULL,
      # user_roomservice_4_3ratio=NULL,
      # user_roomservice_4_4ratio=NULL,
      # user_roomservice_4_5ratio=NULL,
      # user_roomservice_5_1ratio=NULL,
      # user_roomservice_6_0ratio=NULL,
      # user_roomservice_6_1ratio=NULL,
      # user_roomservice_6_2ratio=NULL,
      # user_roomservice_7_0ratio=NULL,
      # user_roomservice_8_1ratio=NULL,
      # user_roomservice_8_2ratio=NULL,
      # user_roomservice_8_345ratio=NULL,
      ##user_consistent_service_3=if_else((user_roomservice_3_123ratio_1week>= 0.5) &(user_roomservice_3_123ratio_1month>=0.5), 1, 0, NA_real_),
      ##user_consistent_service_7=if_else((user_roomservice_7_1ratio_1week>= 0.5) &(user_roomservice_7_1ratio_1month>=0.5), 1, 0, NA_real_),
      ## user_consistent_service_3=if_else(user_roomservice_3_123ratio_1week==user_roomservice_3_123ratio_1month, 1, 0, NA_real_),
    )
  return(tmp)
}