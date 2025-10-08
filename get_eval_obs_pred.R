

# TEST PRED OBSPRED plt FETCHER



 get_eval_obspred=function(model_id){
     
   if (str_starts(model_id,"pls")){
     test=pls_eval$Obs_Pred_data[[model_id]]$test
     zr=tibble(obs=filter(all_data,str_starts(LabelEvent,"LC"))[[variable_lookup[str_split_fixed(model_id,"-",3)[,3]]%>%unlist]],
               pred=filter(all_data,str_starts(LabelEvent,"LC"))[[model_id]]$pred
               )
     train=bind_rows(tibble(train=pls_eval$Obs_Pred_data[[model_id]]$train,set="test"),
                     tibble(train=pls_eval$Obs_Pred_data[[model_id]]$train,set="zr"))
   }else if (str_starts(model_id,"cubist")){
     test=cubist_eval$Obs_Pred_data[[model_id]]$test
     zr=tibble(obs=filter(all_data,str_starts(LabelEvent,"LC"))[[variable_lookup[str_split_fixed(model_id,"-",3)[,3]]%>%unlist]],
               pred=filter(all_data,str_starts(LabelEvent,"LC"))[[model_id]]$pred
     )
     train=bind_rows(tibble(train=cubist_eval$Obs_Pred_data[[model_id]]$train,set="test"),
                     tibble(train=cubist_eval$Obs_Pred_data[[model_id]]$train,set="zr"))
   }else if (str_starts(model_id,"mbl")){
     test=mbl_eval$Obs_Pred_data[[model_id]]$test
     zr=tibble(obs=filter(all_data,str_starts(LabelEvent,"LC"))[[variable_lookup[str_split_fixed(model_id,"-",3)[,3]]%>%unlist]],
               pred=filter(all_data,str_starts(LabelEvent,"LC"))[[model_id]]$pred)
     train=bind_rows(tibble(train=mbl_eval$Obs_Pred_data[[model_id]]$train,set="test"),
                     tibble(train=mbl_eval$Obs_Pred_data[[model_id]]$train,set="zr"))
     if(str_detect(model_id,"log1p")){
                zr$pred=expm1(zr$pred)
     }
     
   }else{
     warning("Model type not found or implemented")
   }
   
   # all train need un-logging
   if(str_detect(model_id,"log1p")){
     train$train=expm1(train$train)
   }
 
   out_data=bind_rows(
     tibble(model=model_id,
            set="test",
            test),
     tibble(model=model_id,
            set="zr",
            zr),
     tibble(model=model_id,
            train
     )
   )
   
   return(out_data)
 }


 
