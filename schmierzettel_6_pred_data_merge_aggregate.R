



model_pred_list=all_test_eval%>%
  filter(type=="cubist")%>%
  group_by(variable)%>%
  filter(rmse==min(rmse))%>%
  filter(variable%in%(str_split_fixed(ref_list," \\[",2)[,1]))%>%
  transmute(x=paste0(type,"_",set,"-",trans,"-",variable))%>%pull(x)




  
  # Extract pred from each nested column, return a data frame of vectors
  pred_df <- model_pred_list %>%
    set_names() %>%
    map_dfc(~ all_data[[.x]]$pred)
  
  # Optional: rename columns with a prefix
  #colnames(pred_df) <- paste0("pred_", names(pred_df))
  
  # Bind back with other columns of all_data (excluding nested columns)
  all_data_clean <- all_data %>%
    select(-all_of(model_pred_list)) %>%
    bind_cols(pred_df)


vertical_profile_cubist_data_agg=cm_aggregate(
  all_data_clean%>%
  filter(str_detect(Campaign,"field"))%>%
  filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
  depth_top_col = "Depth_top",
  depth_bottom_col = "Depth_bottom",
  aggregate_list = c(ref_list,
                     model_pred_list
                     ),
  group_list = c("Device","site_id"),
  res_out = .05)








view(vertical_profile_cubist_data_agg)



























