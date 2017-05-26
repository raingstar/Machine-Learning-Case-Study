library(dplyr)
#####feature_clean_0: Sanity Check, change all negative numbers as the mean.
feature_clean_0 <-function(df){  
  df %>% mutate(user_confirmtime=-user_confirmtime) %>%  ####Majority of user_confirmtime are negative, so I switch the sign.
    mutate_each(funs(replace(., (.<0) & (is.numeric(.)), mean(., na.rm=T))))
}

####feature_clean_1: Ted's suggested features.
feature_clean_1 <- function(df)
{
  df %>%
    mutate(
      lo_hotel_match = ifelse(as.character(hotelid) == as.character(hotelid_lastord),1, 0),
      lo_roomid_match = ifelse(as.character(roomid) == as.character(roomid_lastord), 1, 0),
      lo_basicroomid_match = ifelse(as.character(basicroomid) == as.character(basicroomid_lastord), 1, 0),
      lo_star_match = ifelse(star == star_lastord, 1, 0),
      lo_star_diff = star - star_lastord,
      lo_rank_match = ifelse(rank == rank_lastord, 1, 0),
      lo_rank_diff = rank -rank_lastord,
      lo_return_diff = returnvalue - return_lastord,
      lo_price_diff = price_deduct - price_last_lastord,
      lo_price1_diff = price_deduct - hotel_minprice_lastord,
      lo_price2_diff = price_deduct - basic_minprice_lastord,
      lo_roomservice_2_diff = roomservice_2 - roomservice_2_lastord,
      lo_roomservice_3_diff = roomservice_3 - roomservice_3_lastord,
      lo_roomservice_4_diff = roomservice_4 - roomservice_4_lastord,
      lo_roomservice_5_diff = roomservice_5 - roomservice_5_lastord,
      lo_roomservice_6_diff = roomservice_6 - roomservice_6_lastord,
      lo_roomservice_8_diff = roomservice_8 - roomservice_8_lastord,
      lo_roomtag_2_diff = roomtag_2 - roomtag_2_lastord,
      lo_roomtag_3_diff = roomtag_3 - roomtag_3_lastord,
      lo_roomtag_4_diff = roomtag_4 - roomtag_4_lastord,
      lo_roomtag_5_diff = roomtag_5 - roomtag_5_lastord,
      lo_roomtag_6_diff = roomtag_6 - roomtag_6_lastord,
      discount_ratio=returnvalue/price_deduct,
      order_gap=as.numeric(difftime(as.Date.character(orderdate), 
                                    as.Date.character(orderdate_lastord),units = c("days")))
    )
}

####feature_clean_2: Generate the max of several cols.  
feature_clean_2 <-function(df) {
  df %>%
    mutate(user_roomservice_8_345ratio=user_roomservice_5_345ratio,
           user_roomservice_5_345ratio=NULL,
           user_roomservice_8_2ratio=1-user_roomservice_8_345ratio-user_roomservice_8_1ratio,
           ##          user_roomservice_4_5ratio=1-user_roomservice_4_0ratio-user_roomservice_4_1ratio-user_roomservice_4_2ratio-user_roomservice_4_3ratio-user_roomservice_4_4ratio,
           user_roomservice_4_1ratio_3month=1-user_roomservice_4_0ratio_3month-user_roomservice_4_2ratio_3month-user_roomservice_4_3ratio_3month-user_roomservice_4_4ratio_3month-user_roomservice_4_5ratio_3month,
           user_roomservice_4_1ratio_1month=1-user_roomservice_4_0ratio_1month-user_roomservice_4_2ratio_1month-user_roomservice_4_3ratio_1month-user_roomservice_4_4ratio_1month-user_roomservice_4_5ratio_1month,
           user_roomservice_4_1ratio_1week =1-user_roomservice_4_0ratio_1week -user_roomservice_4_2ratio_1week -user_roomservice_4_3ratio_1week -user_roomservice_4_4ratio_1week -user_roomservice_4_5ratio_1week
    ) %>%
    rowwise() %>% 
    mutate(user_roomservice_4_max= max(user_roomservice_4_0ratio, 
                                       user_roomservice_4_1ratio, 
                                       user_roomservice_4_2ratio,
                                       user_roomservice_4_3ratio, 
                                       user_roomservice_4_4ratio,
                                       user_roomservice_4_5ratio)
           ,user_roomservice_6_max= max(user_roomservice_6_0ratio, 
                                        user_roomservice_6_1ratio, 
                                        user_roomservice_6_2ratio)
           ,user_roomservice_8_max= max(user_roomservice_8_1ratio, 
                                        user_roomservice_8_2ratio, 
                                        user_roomservice_8_345ratio)
           ,user_roomservice_4_max_1week=max(user_roomservice_4_0ratio_1week, 
                                             user_roomservice_4_1ratio_1week, 
                                             user_roomservice_4_2ratio_1week,
                                             user_roomservice_4_3ratio_1week, 
                                             user_roomservice_4_4ratio_1week,
                                             user_roomservice_4_5ratio_1week)
           ,user_roomservice_4_max_1month=max(user_roomservice_4_0ratio_1month, 
                                              user_roomservice_4_1ratio_1month, 
                                              user_roomservice_4_2ratio_1month,
                                              user_roomservice_4_3ratio_1month, 
                                              user_roomservice_4_4ratio_1month,
                                              user_roomservice_4_5ratio_1month)
           ,user_roomservice_4_max_3month=max(user_roomservice_4_0ratio_3month, 
                                              user_roomservice_4_1ratio_3month, 
                                              user_roomservice_4_2ratio_3month,
                                              user_roomservice_4_3ratio_3month, 
                                              user_roomservice_4_4ratio_3month,
                                              user_roomservice_4_5ratio_3month)
    ) %>%
    ungroup()
}

####feature_clean_3: Generate labels to see if current col is the max of several related cols.  
feature_clean_3 <-function(df) {
  df %>% 
    mutate(
      user_roomservice_2_prefer=if_else(user_roomservice_2_1ratio <=0.5, 0, 1, NA_real_),
      user_roomservice_3_prefer=if_else(user_roomservice_3_123ratio<=0.5, 0, 1, NA_real_),
      user_roomservice_4_prefer=case_when(
        .$user_roomservice_4_0ratio==.$user_roomservice_4_max ~ 0,
        .$user_roomservice_4_1ratio==.$user_roomservice_4_max ~ 1,
        .$user_roomservice_4_2ratio==.$user_roomservice_4_max ~ 2,
        .$user_roomservice_4_3ratio==.$user_roomservice_4_max ~ 3,
        .$user_roomservice_4_4ratio==.$user_roomservice_4_max ~ 4,
        .$user_roomservice_4_5ratio==.$user_roomservice_4_max ~ 5,
        TRUE                         ~ NA_real_),
      user_roomservice_5_prefer=if_else(user_roomservice_5_1ratio <=0.5, 0, 1, NA_real_),
      user_roomservice_6_prefer=case_when(
        .$user_roomservice_6_0ratio==.$user_roomservice_6_max ~ 0,
        .$user_roomservice_6_1ratio==.$user_roomservice_6_max ~ 1,
        .$user_roomservice_6_2ratio==.$user_roomservice_6_max ~ 2,
        TRUE                         ~ NA_real_),
      user_roomservice_7_prefer=if_else(user_roomservice_7_0ratio <=0.5, 1, 0, NA_real_),
      user_roomservice_8_prefer=case_when(
        .$user_roomservice_8_1ratio==.$user_roomservice_8_max ~ 1,
        .$user_roomservice_8_2ratio==.$user_roomservice_8_max ~ 2,
        .$user_roomservice_8_345ratio==.$user_roomservice_8_max ~ 3, ###It means actually >=3
        TRUE                           ~ NA_real_),
      user_roomservice_3_prefer_1week=if_else(user_roomservice_3_123ratio_1week <=0.5, 0, 1, NA_real_), 
      user_roomservice_3_prefer_1month=if_else(user_roomservice_3_123ratio_1month <=0.5, 0, 1, NA_real_),
      user_roomservice_3_prefer_3month=if_else(user_roomservice_3_123ratio_3month <=0.5, 0, 1, NA_real_),
      user_roomservice_7_prefer_1week=if_else(user_roomservice_7_0ratio_1week <=0.5, 1, 0, NA_real_),
      user_roomservice_7_prefer_1month=if_else(user_roomservice_7_0ratio_1month <=0.5, 1, 0, NA_real_),
      user_roomservice_7_prefer_3month=if_else(user_roomservice_7_0ratio_3month <=0.5, 1, 0, NA_real_),
      user_roomservice_4_prefer_1week =case_when(
        .$user_roomservice_4_0ratio_1week==.$user_roomservice_4_max_1week ~ 0,
        .$user_roomservice_4_1ratio_1week==.$user_roomservice_4_max_1week ~ 1,
        .$user_roomservice_4_2ratio_1week==.$user_roomservice_4_max_1week ~ 2,
        .$user_roomservice_4_3ratio_1week==.$user_roomservice_4_max_1week ~ 3,
        .$user_roomservice_4_4ratio_1week==.$user_roomservice_4_max_1week ~ 4,
        .$user_roomservice_4_5ratio_1week==.$user_roomservice_4_max_1week ~ 5,
        TRUE                         ~ NA_real_),
      user_roomservice_4_prefer_1month=case_when(
        .$user_roomservice_4_0ratio_1month==.$user_roomservice_4_max_1month ~ 0,
        .$user_roomservice_4_1ratio_1month==.$user_roomservice_4_max_1month ~ 1,
        .$user_roomservice_4_2ratio_1month==.$user_roomservice_4_max_1month ~ 2,
        .$user_roomservice_4_3ratio_1month==.$user_roomservice_4_max_1month ~ 3,
        .$user_roomservice_4_4ratio_1month==.$user_roomservice_4_max_1month ~ 4,
        .$user_roomservice_4_5ratio_1month==.$user_roomservice_4_max_1month ~ 5,
        TRUE                         ~ NA_real_),
      user_roomservice_4_prefer_3month=case_when(
        .$user_roomservice_4_0ratio_3month==.$user_roomservice_4_max_3month ~ 0,
        .$user_roomservice_4_1ratio_3month==.$user_roomservice_4_max_3month ~ 1,
        .$user_roomservice_4_2ratio_3month==.$user_roomservice_4_max_3month ~ 2,
        .$user_roomservice_4_3ratio_3month==.$user_roomservice_4_max_3month ~ 3,
        .$user_roomservice_4_4ratio_3month==.$user_roomservice_4_max_3month ~ 4,
        .$user_roomservice_4_5ratio_3month==.$user_roomservice_4_max_3month ~ 5,
        TRUE                         ~ NA_real_)
    )
}

####feature_clean_4: Feature generation continued.  
feature_clean_4 <-function(df){
  df %>%
    mutate (roomservice_3_new=if_else(roomservice_3==0,0,1,NA_real_ ),
            roomservice_8_new=case_when(.$roomservice_8==0 ~ 1,
                                        .$roomservice_8==1 ~ 2,
                                        .$roomservice_8 >1 ~ 3,
                                        TRUE             ~ NA_real_ )
    ) %>%
    mutate (lo_match_roomservice_2=if_else(roomservice_2==user_roomservice_2_prefer,1, 0, NA_real_),
            lo_diff_roomservice_2 = roomservice_2-user_roomservice_2_prefer,
            lo_match_roomservice_3=if_else(roomservice_3_new==user_roomservice_3_prefer,1, 0, NA_real_),
            lo_diff_roomservice_3 = roomservice_3_new-user_roomservice_3_prefer,
            lo_match_roomservice_4=if_else(roomservice_4==user_roomservice_4_prefer,1, 0, NA_real_),
            lo_diff_roomservice_4 = roomservice_4-user_roomservice_4_prefer,
            lo_match_roomservice_5=if_else(roomservice_5==user_roomservice_5_prefer,1, 0, NA_real_),
            lo_diff_roomservice_5 = roomservice_5-user_roomservice_5_prefer,
            lo_match_roomservice_6=if_else(roomservice_6==user_roomservice_6_prefer,1, 0, NA_real_),
            lo_diff_roomservice_6 = roomservice_6-user_roomservice_6_prefer,
            lo_match_roomservice_7=if_else(roomservice_7==user_roomservice_7_prefer,1, 0, NA_real_),
            lo_diff_roomservice_7 = roomservice_7-user_roomservice_7_prefer,
            lo_match_roomservice_8=if_else(roomservice_8_new==user_roomservice_8_prefer,1, 0, NA_real_),
            lo_diff_roomservice_8 = roomservice_8_new-user_roomservice_8_prefer,
            lo_match_roomservice_3_1week=if_else(roomservice_3_new==user_roomservice_3_prefer_1week,1, 0, NA_real_),
            lo_diff_roomservice_3_1week = roomservice_3_new-user_roomservice_3_prefer_1week,
            lo_match_roomservice_4_1week=if_else(roomservice_4==user_roomservice_4_prefer_1week,1, 0, NA_real_),
            lo_diff_roomservice_4_1week = roomservice_4-user_roomservice_4_prefer_1week,
            lo_match_roomservice_7_1week=if_else(roomservice_7==user_roomservice_7_prefer_1week,1, 0, NA_real_),
            lo_diff_roomservice_7_1week = roomservice_7-user_roomservice_7_prefer_1week,
            lo_match_roomservice_3_1month=if_else(roomservice_3_new==user_roomservice_3_prefer_1month,1, 0, NA_real_),
            lo_diff_roomservice_3_1month = roomservice_3_new-user_roomservice_3_prefer_1month,
            lo_match_roomservice_4_1month=if_else(roomservice_4==user_roomservice_4_prefer_1month,1, 0, NA_real_),
            lo_diff_roomservice_4_1month = roomservice_4-user_roomservice_4_prefer_1month,
            lo_match_roomservice_7_1month=if_else(roomservice_7==user_roomservice_7_prefer_1month,1, 0, NA_real_),
            lo_diff_roomservice_7_1month = roomservice_7-user_roomservice_7_prefer_1month,
            lo_match_roomservice_3_3month=if_else(roomservice_3_new==user_roomservice_3_prefer_3month,1, 0, NA_real_),
            lo_diff_roomservice_3_3month = roomservice_3_new-user_roomservice_3_prefer_3month,
            lo_match_roomservice_4_3month=if_else(roomservice_4==user_roomservice_4_prefer_3month,1, 0, NA_real_),
            lo_diff_roomservice_4_3month = roomservice_4-user_roomservice_4_prefer_3month,
            lo_match_roomservice_7_3month=if_else(roomservice_7==user_roomservice_7_prefer_3month,1, 0, NA_real_),
            lo_diff_roomservice_7_3month = roomservice_7-user_roomservice_7_prefer_3month
    )
}
####feature_clean_rnd2_1: Group by orderid, find the min price of each order, max of basic_week_ordernum_rati and etc. 

feature_clean_rnd2_1 <- function(df)
{
  df %>% group_by(orderid) %>%
    summarise(min_price_order=min(price_deduct),
              max_basic_week_ordernum_ratio=max(basic_week_ordernum_ratio),
              max_basic_recent3_ordernum_ratio=max(basic_recent3_ordernum_ratio),
              max_basic_comment_ratio=max(basic_comment_ratio),
              max_basic_30days_ordnumratio=max(basic_30days_ordnumratio),
              max_basic_30days_realratio=max(basic_30days_realratio)
    ) %>%
    ungroup()
}
####feature_clean_rnd2_2: Group by orderid and basicroomid, find the min price of each order, max of basic_week_ordernum_rati and etc. 

feature_clean_rnd2_2 <- function(df)
{
  df %>% group_by(orderid, basicroomid) %>%
    summarise(min_price_order_basic=min(price_deduct),
              max_room_30days_ordnumratio=max(room_30days_ordnumratio),
              max_room_30days_realratio=max(room_30days_realratio)) %>%
    ungroup()
}

####feature_clean_rnd2_3: Feature generation continued. Using new features when left join with the resutls by feature_clean_rnd2_1 and feature_clean_rnd2_2.  
feature_clean_rnd2_3 <- function(df)
{
  df %>% mutate(price_diff_order_min=price_deduct-min_price_order,
                price_diff_order_basic_min=price_deduct-min_price_order_basic,
                price_diff_user_holiday=price_deduct-user_avgdealpriceholiday,
                price_diff_user_workday=price_deduct-user_avgdealpriceworkday,
                price_diff_user_deal=price_deduct-user_avgdealprice,
                price_diff_user_avg=price_deduct-user_avgprice,
                price_diff_user_max=price_deduct-user_maxprice,
                price_diff_user_min=price_deduct-user_minprice,
                price_diff_user_avg_1week=price_deduct-user_avgprice_1week,
                price_diff_user_max_1week=price_deduct-user_maxprice_1week,
                price_diff_user_min_1week=price_deduct-user_minprice_1week,
                price_diff_user_med_1week=price_deduct-user_medprice_1week,
                price_diff_user_avg_1month=price_deduct-user_avgprice_1month,
                price_diff_user_max_1month=price_deduct-user_maxprice_1month,
                price_diff_user_min_1month=price_deduct-user_minprice_1month,
                price_diff_user_med_1month=price_deduct-user_medprice_1month,
                price_diff_user_avg_3month=price_deduct-user_avgprice_3month,
                price_diff_user_max_3month=price_deduct-user_maxprice_3month,
                price_diff_user_min_3month=price_deduct-user_minprice_3month,
                price_diff_user_med_3month=price_deduct-user_medprice_3month,
                price_diff_user_up2std=price_deduct-user_avgprice-2*user_stdprice,
                price_diff_user_down2std=price_deduct-user_avgprice+2*user_stdprice
  ) %>%
    mutate(      price_diff_user_max_yn=if_else(price_diff_user_max>0, 1, 0, NA_real_),
                 price_diff_user_min_yn=if_else(price_diff_user_min>0, 1, 0, NA_real_),
                 price_diff_user_up2std_yn=if_else(price_diff_user_up2std>0, 1, 0, NA_real_),
                 price_diff_user_down2std_yn=if_else(price_diff_user_down2std<0, 1, 0, NA_real_),
                 price_diff_user_max_yn_1week=if_else(price_diff_user_max_1week>0,1,0,NA_real_),
                 price_diff_user_min_yn_1week=if_else(price_diff_user_min_1week>0,1,0,NA_real_),
                 price_diff_user_max_yn_1month=if_else(price_diff_user_max_1month>0,1,0,NA_real_),
                 price_diff_user_min_yn_1month=if_else(price_diff_user_min_1month>0,1,0,NA_real_),
                 price_diff_user_max_yn_3month=if_else(price_diff_user_max_3month>0,1,0,NA_real_),
                 price_diff_user_min_yn_3month=if_else(price_diff_user_min_3month>0,1,0,NA_real_),
                 max_basic_week_ordernum_ratio_yn=if_else(max_basic_week_ordernum_ratio==basic_week_ordernum_ratio,1,0,NA_real_),
                 max_basic_recent3_ordernum_ratio_yn=if_else(max_basic_recent3_ordernum_ratio==basic_recent3_ordernum_ratio,1,0,NA_real_),
                 max_basic_comment_ratio_yn=if_else(max_basic_comment_ratio==basic_comment_ratio,1,0,NA_real_),
                 max_basic_30days_ordnumratio_yn=if_else(max_basic_30days_ordnumratio==basic_30days_ordnumratio,1,0,NA_real_),
                 max_basic_30days_realratio_yn=if_else(max_basic_30days_realratio==basic_30days_realratio,1,0,NA_real_),
                 max_room_30days_ordnumratio_yn=if_else(max_room_30days_ordnumratio==room_30days_ordnumratio,1,0,NA_real_),
                 max_room_30days_realratio_yn=if_else(max_room_30days_realratio==room_30days_realratio,1,0,NA_real_)
                 
    )
}