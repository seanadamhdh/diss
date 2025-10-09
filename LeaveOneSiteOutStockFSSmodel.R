
require(caret)

left_join(BDF_SSL%>%select(-spc_rs),spc_tmp)%>%
  filter(!is.na(`dB [g/cm3]`)|
           (!is.na(`dB_105 [g/cm3]`)&Device!="Profilspaten")&
           !is.na(`TOC [wt-%]`))->stock_data


for (run in c(1:2)){

los_FSSstock_svm=c()
los_dBstock_svm=c()
los_TOC_svm=c()
los_dB_svm=c()
los_FSS_svm=c()
### parameter grid ####
trainGrid<-expand.grid(C=10**seq(-4,0,.1))

### cross-validation ####
fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10,
  ## print progress
  verboseIter = TRUE,
  returnData = F,
  allowParallel = T)

for (i in unique(stock_data%>%filter(Campaign%>%str_detect("field"))%>%pull(site_id))){
  
  #FSS105stock
  stock_data%>%transmute(LabelEvent,site_id,
                         Y=`FSS_105 [g/cm3]`*`TOC [wt-%]`,
                         X=spc_sg_snv_rs4)%>%
    na.omit()->FSS105stock_data
  
  
  FSS105stock_train=FSS105stock_data%>%filter(site_id!=i)
  FSS105stock_test=FSS105stock_data%>%filter(site_id==i)
  
  
  
  
  los_FSSstock_svm[[i]]=train(y=FSS105stock_train$Y,
                              x=as.matrix(FSS105stock_train$X),
                              method="svmLinear",
                              tuneGrid = trainGrid,
                              trControl = fitControl,
  )
  los_FSSstock_svm[[i]]$documentation$test=FSS105stock_test
  
  
  
  #dB105stock
  stock_data%>%transmute(LabelEvent,site_id,
                         Y=`dB_105 [g/cm3]`*`TOC [wt-%]`,
                         X=spc_sg_snv_rs4)%>%
    na.omit()->dB105stock_data
  
  
  dB105stock_train=dB105stock_data%>%filter(site_id!=i)
  dB105stock_test=dB105stock_data%>%filter(site_id==i)
  
  
  
  
  
  los_dBstock_svm[[i]]=train(y=dB105stock_train$Y,
                             x=as.matrix(dB105stock_train$X),
                             method="svmLinear",
                             tuneGrid = trainGrid,
                             trControl = fitControl,
  )
  los_dBstock_svm[[i]]$documentation$test=dB105stock_test
  
  
  
  
  #dB105
  stock_data%>%transmute(LabelEvent,site_id,Y=`dB_105 [g/cm3]`,X=spc_sg_snv_rs4)%>%
    na.omit()->dB105_data
  
  
  dB105_train=dB105_data%>%filter(site_id!=i)
  dB105_test=dB105_data%>%filter(site_id==i)
  
  
  
  
  
  los_dB_svm[[i]]=train(y=dB105_train$Y,
                        x=as.matrix(dB105_train$X),
                        method="svmLinear",
                        tuneGrid = trainGrid,
                        trControl = fitControl,
  )
  los_dB_svm[[i]]$documentation$test=dB105_test
  
  
  
  #FSS105
  stock_data%>%transmute(LabelEvent,site_id,Y=`FSS_105 [g/cm3]`,X=spc_sg_snv_rs4)%>%
    na.omit()->FSS105_data
  
  
  FSS105_train=FSS105_data%>%filter(site_id!=i)
  FSS105_test=FSS105_data%>%filter(site_id==i)
  
  
  
  
  los_FSS_svm[[i]]=train(y=FSS105_train$Y,
                         x=as.matrix(FSS105_train$X),
                         method="svmLinear",
                         tuneGrid = trainGrid,
                         trControl = fitControl,
  )
  los_FSS_svm[[i]]$documentation$test=FSS105_test
  
  
  
  #TOC
  stock_data%>%transmute(LabelEvent,site_id,Y=`TOC [wt-%]`,X=spc_sg_snv_rs4)%>%
    na.omit()->TOC_data
  
  
  TOC_train=TOC_data%>%filter(site_id!=i)
  TOC_test=TOC_data%>%filter(site_id==i)
  
  
  
  
  
  los_TOC_svm[[i]]=train(y=TOC_train$Y,
                         x=as.matrix(TOC_train$X),
                         method="svmLinear",
                         tuneGrid = trainGrid,
                         trControl = fitControl,
  )
  los_TOC_svm[[i]]$documentation$test=TOC_test
  
  
  
}

saveRDS(los_dB_svm,
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dB_svm",run))
saveRDS(los_dBstock_svm,
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dBstock_svm",run))
saveRDS(los_FSS_svm,
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dBFSS_svm",run))
saveRDS(los_FSSstock_svm,
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_FSSstock_svm",run))
saveRDS(los_TOC_svm,
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_TOC_svm",run))
}


los_dB_svm=readRDS(
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dB_svm",1))
los_dBstock_svm=readRDS(
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dBstock_svm",1))
los_FSS_svm=readRDS(
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_dBFSS_svm",1))
los_FSSstock_svm=readRDS(
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_FSSstock_svm",1))
los_TOC_svm=readRDS(
        paste0(data_dir,"Sean_Environment/R_main/models/SVM_LOS/",
               "los_TOC_svm",1))


{
  los_FSSstock_pred=c()
  los_dBstock_pred=c()
  los_TOC_pred=c()
  los_dB_pred=c()
  los_FSS_pred=c()
  
  
  los_FSSstock_eval=c()
  los_dBstock_eval=c()
  los_TOC_eval=c()
  los_dB_eval=c()
  los_FSS_eval=c()
  
  for (i in names(los_FSS_svm)){
    print(i)
    
    
    ##### fssstock ####
    obspred_FSSstock=tibble(
      site=i,
      LabelEvent=los_FSSstock_svm[[i]]$documentation$test$LabelEvent,
      obs=los_FSSstock_svm[[i]]$documentation$test$Y,
      pred=predict(los_FSSstock_svm[[i]],los_FSSstock_svm[[i]]$documentation$test$X)
    )
    
    los_FSSstock_pred=bind_rows(
      los_FSSstock_pred,
      obspred_FSSstock
    )
    los_FSSstock_eval=bind_rows(los_FSSstock_eval,
                                tibble(site=i,
                                       evaluate_model_adjusted(obspred_FSSstock))
    )
    
    
    
    #### dbstock ####
    obspred_dBstock=tibble(
      site=i,
      LabelEvent=los_dBstock_svm[[i]]$documentation$test$LabelEvent,
      obs=los_dBstock_svm[[i]]$documentation$test$Y,
      pred=predict(los_dBstock_svm[[i]],los_dBstock_svm[[i]]$documentation$test$X)
    )
    
    los_dBstock_pred=bind_rows(
      los_dBstock_pred,
      obspred_dBstock
    )
    los_dBstock_eval=bind_rows(los_dBstock_eval,
                               tibble(site=i,
                                      evaluate_model_adjusted(obspred_dBstock))
    )  
    
    
    #### fss ####
    obspred_FSS=tibble(site=i,
                       LabelEvent=los_FSS_svm[[i]]$documentation$test$LabelEvent,
                       obs=los_FSS_svm[[i]]$documentation$test$Y,
                       pred=predict(los_FSS_svm[[i]],los_FSS_svm[[i]]$documentation$test$X)
    )
    
    los_FSS_pred=bind_rows(
      los_FSS_pred,
      obspred_FSS
    )
    los_FSS_eval=bind_rows(los_FSS_eval,
                           tibble(site=i,
                                  evaluate_model_adjusted(obspred_FSS))
    )
    
    
    #### db ####
    obspred_dB=tibble(site=i,
                      LabelEvent=los_dB_svm[[i]]$documentation$test$LabelEvent,
                      obs=los_dB_svm[[i]]$documentation$test$Y,
                      pred=predict(los_dB_svm[[i]],los_dB_svm[[i]]$documentation$test$X)
    )
    
    los_dB_pred=bind_rows(
      los_dB_pred,
      obspred_dB
    )
    los_dB_eval=bind_rows(los_dB_eval,
                          tibble(site=i,
                                 evaluate_model_adjusted(obspred_dB))
    ) 
    
    #### TOC ####
    obspred_TOC=tibble(site=i,
                       LabelEvent=los_TOC_svm[[i]]$documentation$test$LabelEvent,
                       obs=los_TOC_svm[[i]]$documentation$test$Y,
                       pred=predict(los_TOC_svm[[i]],los_TOC_svm[[i]]$documentation$test$X)
    )
    
    los_TOC_pred=bind_rows(
      los_TOC_pred,
      obspred_TOC
    )
    los_TOC_eval=bind_rows(los_TOC_eval,
                           tibble(site=i,
                                  evaluate_model_adjusted(obspred_TOC))
    ) 
    
    
  }
  
  
  
  left_join(
    los_dB_pred%>%rename_with(.cols = c(obs,pred),~paste0("dB_",.x)),
    los_dBstock_pred%>%rename_with(.cols = c(obs,pred),~paste0("dBstock_",.x)),
    by=c("LabelEvent","site")
  )%>%
    left_join(
      los_FSS_pred%>%rename_with(.cols = c(obs,pred),~paste0("FSS_",.x)),
      by=c("LabelEvent","site")
    )%>%
    left_join(
      los_FSSstock_pred%>%rename_with(.cols = c(obs,pred),~paste0("FSSstock_",.x)),
      by=c("LabelEvent","site")
    )%>%
    left_join(
      los_TOC_pred%>%rename_with(.cols = c(obs,pred),~paste0("TOC_",.x)),
      by=c("LabelEvent","site")
    )->los_all_pred
  
  
  
}




bind_rows(
  tibble(site="BDF02",method="direct",
         los_all_pred%>%filter(site=="BDF02")%>%
           mutate(obs=dBstock_obs,pred=dBstock_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF23",method="direct",
         los_all_pred%>%filter(site=="BDF23")%>%
           mutate(obs=dBstock_obs,pred=dBstock_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF30",method="direct",
         los_all_pred%>%filter(site=="BDF30")%>%
           mutate(obs=dBstock_obs,pred=dBstock_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF35",method="direct",
         los_all_pred%>%filter(site=="BDF35")%>%
           mutate(obs=dBstock_obs,pred=dBstock_pred)%>%evaluate_model_adjusted),
  
  
  tibble(site="BDF02",method="TOC_dB",
         los_all_pred%>%filter(site=="BDF02")%>%
           mutate(obs=dBstock_obs,pred=dB_pred*TOC_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF23",method="TOC_dB",
         los_all_pred%>%filter(site=="BDF23")%>%
           mutate(obs=dBstock_obs,pred=dB_pred*TOC_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF30",method="TOC_dB",
         los_all_pred%>%filter(site=="BDF30")%>%
           mutate(obs=dBstock_obs,pred=dB_pred*TOC_pred)%>%evaluate_model_adjusted),
  
  tibble(site="BDF35",method="TOC_dB",
         los_all_pred%>%filter(site=="BDF35")%>%
           mutate(obs=dBstock_obs,pred=dB_pred*TOC_pred)%>%evaluate_model_adjusted)
  
)->stock_pred_eval


write_excel_csv(stock_pred_eval%>%
select("site","method","n","min","mean","median","max","rmse","rpd","R2","linsCCC","b","bias"),
                "C:/Users/adam/Desktop/UNI/PhD/DISS/tables/stock_pred_eval.csv")


# only doing dB
# ggarrange(
  # ggplot(los_all_pred2,aes(x=FSSstock_obs,y=FSSstock_pred,col=site,shape=site))+
  #   geom_abline(slope = 1)+
  #   geom_point()+
  #   theme_pubr()+
  #   xlab(expression("Observed soil TOC stock [T/(ha c"*m^-1*")]"))+
  #   ylab(expression("Predicted soil TOC stock [T/(ha c"*m^-1*")]"))+
  #   scale_shape_manual("Left out (Test set site)",
  #                      values = c(15,16,17,8))+
  #   scale_color_manual("Left out (Test set site)",
  #                      values=colorblind_safe_colors()[c(4,6,7,8)])+
  #   ggtitle("TOC stock using FSS105"),#->FSSstockpred
#   
#   ggplot(los_all_pred,aes(x=dBstock_obs,y=dB_pred*TOC_pred,col=site,shape=site))+
#     geom_abline(slope = 1)+
#     geom_point()+
#     theme_pubr()+
#     xlab(expression("Observed soil TOC stock [T/(ha c"*m^-1*")]"))+
#     ylab(expression("Predicted soil TOC stock [T/(ha c"*m^-1*")]"))+
#     scale_shape_manual("Left out (Test set site)",
#                        values = c(15,16,17,8))+
#     scale_color_manual("Left out (Test set site)",
#                        values=colorblind_safe_colors()[c(4,6,7,8)])+
#     ggtitle("TOC stock using dB105"),
#   common.legend = T)


  ggarrange(
  ggplot(los_all_pred,aes(x=dBstock_obs,y=(dB_pred*TOC_pred)#if_else((dB_pred*TOC_pred)<0,0,(dB_pred*TOC_pred))
                          ,col=site,shape=site))+
    geom_abline(slope = 1)+
    geom_point()+
    theme_pubr()+
    coord_fixed(xlim=c(-1,4.5),ylim = c(-1,4.5),expand=F)+
    xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
    ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
    scale_shape_manual("Left out site (Test set)",
                       values = c(15,16,17,8))+
    scale_color_manual("Left out site (Test set)",
                       values=colorblind_safe_colors()[c(4,6,7,8)])+
    ggtitle("LOSO prediction (TOC*dB105)"),
  ggplot(los_all_pred,aes(x=dBstock_obs,y=dBstock_pred#if_else(dBstock_pred<0,0,dBstock_pred)
                          ,col=site,shape=site))+
    geom_abline(slope = 1)+
    geom_point()+
    theme_pubr()+
    coord_fixed(xlim=c(-1,4.5),ylim = c(-1,4.5),expand=F)+
    xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
    ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
    scale_shape_manual("Left out site (Test set)",
                       values = c(15,16,17,8))+
    scale_color_manual("Left out site (Test set)",
                       values=colorblind_safe_colors()[c(4,6,7,8)])+
    ggtitle("LOSO prediction (direct)"),
 # SVM models calibrated from archive samples
   # ggplot(pred_tmp,aes(x=obs,y=pred_dB_pred_TOC#if_else(dBstock_pred<0,0,dBstock_pred)
   #                        ,col=site,shape=site))+
   #  geom_abline(slope = 1)+
   #  geom_point()+
   #  theme_pubr()+
   #  coord_fixed(xlim=c(-1,4.5),ylim = c(-1,4.5),expand=F)+
   #  xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
   #  ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
   #  scale_shape_manual("Left out (Test set site)",
   #                     values = c(15,16,17,8))+
   #  scale_color_manual("Left out (Test set site)",
   #                     values=colorblind_safe_colors()[c(4,6,7,8)])+
   #  ggtitle("Archive prediction"),
   # 
  common.legend = T,widths = c(4,4))->stockpred

stockpred

ggsave(plot=stockpred,filename="dBstock_pred.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 8,height = 4)

ggsave(plot=FSSstockpred+theme(legend.position = "right"),filename="FSSstock_pred_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 4,height = 4)


bind_rows(los_dB_eval%>%mutate(var="dB"),
          los_dBstock_eval%>%mutate(var="dBstock"),
          los_FSS_eval%>%mutate(var="FSS"),
          los_FSSstock_eval%>%mutate(var="FSSstock"),
          los_TOC_eval%>%mutate(var="TOC")
)%>%write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/loseval.csv")







# aggregated Stocks ####


## 0-30 ####
cm_aggregate(
  dataset = left_join(los_all_pred2,allBDF_OSSL_data,by="LabelEvent")%>%
    # svm can produce negative values; set = 0 to reduce error when summarising
    mutate(obs=dBstock_obs,
           OSSLpred=if_else((bd_usda.a4_g.cm3_pred*oc_usda.c729_w.pct_pred)<0,0,(bd_usda.a4_g.cm3_pred*oc_usda.c729_w.pct_pred)),
           saxSSLpred=if_else((dB_pred*TOC_pred)<0,0,(dB_pred*TOC_pred)),
           saxSSLpred_dir=if_else(dBstock_pred<0,0,dBstock_pred)),
  depth_top_col = "Depth_top",
  depth_bottom_col = "Depth_bottom",
  aggregate_list = c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir"),
  group_list = c("site","Device"),res_out = 0.05 ,add_funs = list(min=~min(.,na.rm = T),max=~max(.,na.rm = T)) 
  
)%>%
pivot_longer(cols=c("obs_mean","obs_min","obs_max",
                    "OSSLpred_mean","OSSLpred_min","OSSLpred_max",
                    "saxSSLpred_mean","saxSSLpred_min","saxSSLpred_max",
                    "saxSSLpred_dir_mean","saxSSLpred_dir_min","saxSSLpred_dir_max"))%>%
  mutate(metric=paste0("m",str_split_fixed(name,"_m",2)[,2]),
         name=str_split_fixed(name,"_m",2)[,1],
         site_id=site)%>%
  filter(u3<=.3)%>%
  pivot_wider(names_from = "metric",values_from = "value")%>%
  group_by(site_id,Device,name)%>%
  summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(mean)*5)%>% #5 cm incr
  mutate(name=factor(name,levels=c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir")))%>%
  ggplot()+
  geom_col(aes(x=Device,y=STOCK,fill=name,group=name),
           position=position_dodge(),col="black")+
  geom_errorbar(aes(x=Device,y=STOCK,ymin = minSTOCK,ymax=maxSTOCK,group=name),width=.25,position=position_dodge(width = .91)
  )+
  facet_wrap(~site_id,ncol=4)+
  theme_pubr()+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45,hjust=1,vjust=1),
        legend.position = "inside",legend.position.inside = c(.2,.8))+
  ylab(expression("TOC [T C h"*a^-1*"]"))+
  scale_fill_manual("Soil TOC stock (0-30 cm) ",
                    breaks = c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir"),
                    values = c(colorblind_safe_colors()[c(2,4,6,7)]),
                    labels = c("Measured","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)"))+
  scale_x_discrete(breaks=c("Quicksampler","Rammkern"),labels=c("Quicksampler","Push core"))->predStock30


ggsave(plot=predStock30,filename="dBstock_pred30.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 10,height = 5)
  
# better version in schmierzettel 7
## 0-50 ####
  cm_aggregate(
    dataset = left_join(los_all_pred2,allBDF_OSSL_data,by="LabelEvent")%>%
      # svm can produce negative values; set = 0 to reduce error when summarising
      mutate(obs=dBstock_obs,
             OSSLpred=if_else((bd_usda.a4_g.cm3_pred*oc_usda.c729_w.pct_pred)<0,0,(bd_usda.a4_g.cm3_pred*oc_usda.c729_w.pct_pred)),
             saxSSLpred=if_else((dB_pred*TOC_pred)<0,0,(dB_pred*TOC_pred)),
             saxSSLpred_dir=if_else(dBstock_pred<0,0,dBstock_pred)),
    depth_top_col = "Depth_top",
    depth_bottom_col = "Depth_bottom",
    aggregate_list = c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir"),
    group_list = c("site","Device"),res_out = 0.05 ,add_funs = list(min=~min(.,na.rm = T),max=~max(.,na.rm = T)) 
    
  )%>%
    pivot_longer(cols=c("obs_mean","obs_min","obs_max",
                        "OSSLpred_mean","OSSLpred_min","OSSLpred_max",
                        "saxSSLpred_mean","saxSSLpred_min","saxSSLpred_max",
                        "saxSSLpred_dir_mean","saxSSLpred_dir_min","saxSSLpred_dir_max"))%>%
    mutate(metric=paste0("m",str_split_fixed(name,"_m",2)[,2]),
           name=str_split_fixed(name,"_m",2)[,1],
           site_id=site)%>%
    pivot_wider(names_from = "metric",values_from = "value")%>%
    #group_by(site,u3,name,Device)%>%summarise(n=n())%>%pull(n)%>%unique # check data
    filter(u3<=.5)%>% #----------------------------------------------set depth
    group_by(site_id,Device,name)%>%
    summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(mean)*5)%>% #5 cm incr
    mutate(name=factor(name,levels=c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir")))%>%
    ggplot()+
    geom_col(aes(x=Device,y=STOCK,fill=name,group=name),
             position=position_dodge(),col="black")+
    geom_errorbar(aes(x=Device,y=STOCK,ymin = minSTOCK,ymax=maxSTOCK,group=name),width=.25,position=position_dodge(width = .91)
    )+
    facet_wrap(~site_id,ncol=4)+
    theme_pubr()+
    theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          legend.position = "inside",legend.position.inside = c(.2,.8))+
    ylab(expression("TOC [T C h"*a^-1*"]"))+
    scale_fill_manual("Soil TOC stock (0-50 cm) ",
                      breaks = c("obs","OSSLpred","saxSSLpred","saxSSLpred_dir"),
                      values = c(colorblind_safe_colors()[c(2,4,6,7)]),
                      labels = c("Measured","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)"))+
    scale_x_discrete(breaks=c("Quicksampler","Rammkern"),labels=c("Quicksampler","Push core"))->predStock50

ggsave(plot=predStock50,filename="dBstock_pred50.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 10,height = 5)

