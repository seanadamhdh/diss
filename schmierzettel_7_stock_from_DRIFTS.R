


# load data in load-and-eval_BDF-SSL.R
# load OSSL pred in predict_all_BDF.R

allBDF_OSSL=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/DataTable_OSSLpred")

allBDF_OSSL_data=left_join(BDF_SSL,allBDF_OSSL,by="LabelEvent")


# load LOS SVM model data

{
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
  }


svm_eval=read_rds(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_all/evaluation"))
svm_eval$eval%>%group_by(variable)%>%filter(rmse==min(rmse))%>%view

svm_TOC=read_rds(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_all/svmLinear_spc_sg_rs4_none_TOC"))
# very few obs
svm_dB=read_rds(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_all/svmLinear_spc_sg_snv_rs4_none_dB"))


# field data
svm_eval_field=read_rds(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_physics_field/evaluation"))
svm_eval_field%>%group_by(variable)%>%filter(rmse==min(rmse))%>%view
svm_dB105=read_rds(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_physics_field/svmLinear_spc_sg_rs4_log1p_dB_105"))

require(caret)
{
left_join(BDF_SSL%>%select(-spc_rs),spc_tmp)%>%
  filter(!is.na(`dB [g/cm3]`)|
           (!is.na(`dB_105 [g/cm3]`)&Device!="Profilspaten")&
           !is.na(`TOC [wt-%]`))->stock_data


#FSS105
stock_data%>%transmute(Y=`FSS_105 [g/cm3]`*`TOC [wt-%]`,X=spc_sg_snv_rs4)%>%na.omit()->FSS105_data

if(F){
  inTrain=naes(FSS105_data$X,k = .75*nrow(FSS105_data))
  
  FSS105_train=FSS105_data[inTrain$model,]
  FSS105_test=FSS105_data[inTrain$test,]
  
  
  ## set svm tuning framework ####
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
  
  
  FSS_svm=train(y=train$Y,
                x=as.matrix(train$X),
                method="svmLinear",
                tuneGrid = trainGrid,
                trControl = fitControl,
  )
  saveRDS(FSS_svm,"//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/FSS_TOC")
}

plot(FSS105_test$Y,predict(FSS_svm,FSS105_test$X))
abline(0,1)




#db105
stock_data%>%transmute(Y=`dB_105 [g/cm3]`*`TOC [wt-%]`,X=spc_sg_snv_rs4)%>%na.omit()->dB105_data

# kmeans, 75% of field data excl. Profilspaten because dB is shit
  inTrain=naes(dB105_data$X,k = .75*nrow(dB105_data))
  
  dB105_train=dB105_data[inTrain$model,]
  dB105_test=dB105_data[inTrain$test,]
  
  
  ## set cubist tuning framework ####
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
  
  
  dB105_svm=train(y=dB105_train$Y,
                x=as.matrix(dB105_train$X),
                method="svmLinear",
                tuneGrid = trainGrid,
                trControl = fitControl,
  )
  saveRDS(dB105_svm,"//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/dB105_TOC")
  

  
plot(dB105_test$Y,predict(FSS_svm,dB105_test$X))
abline(0,1)

dB105_svm=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/dB105_TOC")


# DIRECT Archive ####

BDF_SSL%>%select(-spc_rs,-OptionalLabel.x,-OptionalLabel.y)%>%left_join(spc_tmp,by="LabelEvent")%>%
  mutate(dBTOCstock=`dB [g/cm3]`*`TOC [wt-%]`)%>%filter(!is.na(dBTOCstock)&!is.na(spc_sg_snv_rs4[,1]))%>%
  transmute(Y=dBTOCstock,X=spc_sg_snv_rs4)->archiveStockData



# kmeans, 75% of field data excl. Profilspaten because dB is shit
inTrain=naes(archiveStockData$X,k = .75*nrow(archiveStockData))

archiveDirect_train=archiveStockData[inTrain$model,]
archiveDirect_test=archiveStockData[inTrain$test,]


## set cubist tuning framework ####
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


archiveDirect_svm=train(y=archiveDirect_train$Y,
                x=as.matrix(archiveDirect_train$X),
                method="svmLinear",
                tuneGrid = trainGrid,
                trControl = fitControl,
)
saveRDS(archiveDirect_svm,"//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/archive_direct")



plot(archiveDirect_test$Y,predict(archiveDirect_svm,archiveDirect_test$X))
abline(0,1)
}



archiveDirect_svm=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/archive_direct")
dB105_svm=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/SVM_models_stock/dB105_TOC")










# Predicting stock using dBStock model directly and by OSSL dB * OSSL OC #### 
allBDF_OSSL_data%>%
  
  # old field svm (no LOS)
  # left_join(spc_tmp%>%
  #             transmute(LabelEvent,pred_dB105_TOC=predict(dB105_svm,spc_sg_snv_rs4)))%>%
  # left_join(spc_tmp%>%
  #             transmute(LabelEvent,pred_dB105_pred_TOC=expm1(predict(svm_dB105,spc_sg_rs4))*
  #                         predict(svm_TOC,spc_sg_rs4)))%>%
  
  left_join(los_all_pred)%>%
  mutate(pred_dB105_TOC=dBstock_pred,
         pred_dB105_pred_TOC=dB_pred*TOC_pred)%>%
  
  # archive models
  # db*TOC
  left_join(spc_tmp%>%
                  transmute(LabelEvent,pred_dB_pred_TOC=predict(svm_dB,spc_sg_snv_rs4)*
                                    predict(svm_TOC,spc_sg_rs4))
  )%>%
  # direct
  left_join(spc_tmp%>%
              transmute(LabelEvent,pred_dB_TOC=predict(archiveDirect_svm,spc_sg_snv_rs4))
  )%>%
  mutate(obs=`TOC [wt-%]`*`dB_105 [g/cm3]`,
         predOSSL_dB105_TOC=oc_usda.c729_w.pct_pred*bd_usda.a4_g.cm3_pred)->pred_tmp





pred_tmp%>%filter(Campaign%>%str_detect("field")&Device!="Profilspaten")%>%
                    ggplot(aes(x=obs))+
  geom_abline(slope = 1)+
  geom_point(aes(y=pred_dB_pred_TOC),col="red")+
  geom_point(aes(y=predOSSL_dB105_TOC),col="blue")+
  geom_point(aes(y=pred_dB_TOC),col="green")+
  geom_point(data=los_all_pred,aes(x=dBstock_obs,y=dBstock_pred),shape=4,stroke=1)
  



#5 cm aggregate of preds ####
tmp=TUBAFsoilFunctions::cm_aggregate(
  dataset = pred_tmp%>%
    filter(LabelEvent%>%substr(2,2)%in%c("R","Q")&Depth_bottom>0),
  depth_top_col = "Depth_top",
  depth_bottom_col = "Depth_bottom",
  aggregate_list = c("obs","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC","pred_dB_pred_TOC","pred_dB_TOC"),
  group_list = c("Device","site_id"),
  add_funs = list(min=~min(.,na.rm = T),max=~max(.,na.rm = T)),
  res_out = .05)%>%pivot_longer(cols = c(obs_mean,predOSSL_dB105_TOC_mean,pred_dB105_TOC_mean,pred_dB105_pred_TOC_mean,pred_dB_pred_TOC_mean,pred_dB_TOC_mean,
                                         obs_min,predOSSL_dB105_TOC_min,pred_dB105_TOC_min,pred_dB105_pred_TOC_min,pred_dB_pred_TOC_min,pred_dB_TOC_min,
                                         obs_max,predOSSL_dB105_TOC_max,pred_dB105_TOC_max,pred_dB105_pred_TOC_max,pred_dB_pred_TOC_max,pred_dB_TOC_max))%>%
  mutate(metric=paste0("m",str_split_fixed(name,"_m",2)[,2]),name=str_split_fixed(name,"_m",2)[,1])



# allBDF_OSSL_data%>%left_join(spc_tmp%>%transmute(LabelEvent,pred_dB105_TOC=predict(dB105_svm,spc_sg_snv_rs4)))%>%
#   filter(LabelEvent%in%stock_data$LabelEvent[inTrain$test])%>%
#   filter(Device!="Profilspaten")%>%
#   mutate(obs=`TOC [wt-%]`*`dB_105 [g/cm3]`,pred=oc_usda.c729_w.pct_pred*bd_usda.a4_g.cm3_pred)%>%
#   ggplot(aes(x=obs,y=pred,shape=Device))+geom_point(col="black")+geom_abline(slope = 1)+
#   geom_point(aes(y=pred_dB105_TOC),col="red")



# obspred OSSL and archive pred ####


ggarrange(
  

#SVM models calibrated from archive samples TOC*dB
  ggplot(pred_tmp%>%filter(Campaign%>%str_detect("field")),aes(x=obs,y=pred_dB_pred_TOC#if_else(dBstock_pred<0,0,dBstock_pred)
                         ,col=site,shape=site))+
   geom_abline(slope = 1)+
   geom_point()+
   theme_pubr()+
   coord_fixed(xlim=c(-.5,6),ylim = c(-.5,6),expand=F)+
   xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
   ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
   scale_shape_manual("BDF site",
                      breaks=c("BDF02","BDF23","BDF30","BDF35"),
                      values = c(15,16,17,8))+
   scale_color_manual("BDF site",
                      breaks=c("BDF02","BDF23","BDF30","BDF35"),
                      values=colorblind_safe_colors()[c(4,6,7,8)])+
   ggtitle("Archive prediction (TOC*dB)"),

#SVM models calibrated from archive samples TOC*dB
ggplot(pred_tmp%>%filter(Campaign%>%str_detect("field")),aes(x=obs,y=pred_dB_TOC#if_else(dBstock_pred<0,0,dBstock_pred)
                                                             ,col=site,shape=site))+
  geom_abline(slope = 1)+
  geom_point()+
  theme_pubr()+
  coord_fixed(xlim=c(-.5,6),ylim = c(-.5,6),expand=F)+
  xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
  ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
  scale_shape_manual("BDF site",
                     breaks=c("BDF02","BDF23","BDF30","BDF35"),
                     values = c(15,16,17,8))+
  scale_color_manual("BDF site",
                     breaks=c("BDF02","BDF23","BDF30","BDF35"),
                     values=colorblind_safe_colors()[c(4,6,7,8)])+
  ggtitle("Archive prediction (direct)"),

ggplot(pred_tmp%>%filter(Campaign%>%str_detect("field")),aes(x=obs,y=predOSSL_dB105_TOC
                                                             ,col=site,shape=site))+
  geom_abline(slope = 1)+
  geom_point()+
  theme_pubr()+
  coord_fixed(xlim= c(-.5,6),ylim = c(-.5,6),expand=F)+
  xlab(expression("Observed TOC stock [T/(ha c"*m^-1*")]"))+
  ylab(expression("Predicted TOC stock [T/(ha c"*m^-1*")]"))+
  scale_shape_manual("BDF site",
                     breaks=c("BDF02","BDF23","BDF30","BDF35"),
                     values = c(15,16,17,8))+
  scale_color_manual("BDF site",
                     breaks=c("BDF02","BDF23","BDF30","BDF35"),
                     values=colorblind_safe_colors()[c(4,6,7,8)])+
  ggtitle("OSSL prediction"),

  common.legend = T,widths = c(4,4))->stockpred2

stockpred2


ggsave(plot=stockpred2,
       ,filename="stock_pred_OSSL_archvie.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 8,height = 8)

## stats ####


bind_rows(
  tibble(site="BDF02",method="OSSL",
         pred_tmp%>%filter(site=="BDF02")%>%
           mutate(pred=predOSSL_dB105_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF23",method="OSSL",
         pred_tmp%>%filter(site=="BDF23")%>%
           mutate(pred=predOSSL_dB105_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF30",method="OSSL",
         pred_tmp%>%filter(site=="BDF30")%>%
           mutate(pred=predOSSL_dB105_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF35",method="OSSL",
         pred_tmp%>%filter(site=="BDF35")%>%
           mutate(pred=predOSSL_dB105_TOC)%>%evaluate_model_adjusted),
  
  
  tibble(site="BDF02",method="archive",
         pred_tmp%>%filter(site=="BDF02")%>%
           mutate(pred=pred_dB_pred_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF23",method="archive",
         pred_tmp%>%filter(site=="BDF23")%>%
           mutate(pred=pred_dB_pred_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF30",method="archive",
         pred_tmp%>%filter(site=="BDF30")%>%
           mutate(pred=pred_dB_pred_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF35",method="archive",
         pred_tmp%>%filter(site=="BDF35")%>%
           mutate(pred=pred_dB_pred_TOC)%>%evaluate_model_adjusted),
  
  
  tibble(site="BDF02",method="archiveDirect",
         pred_tmp%>%filter(site=="BDF02")%>%
           mutate(pred=pred_dB_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF23",method="archiveDirect",
         pred_tmp%>%filter(site=="BDF23")%>%
           mutate(pred=pred_dB_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF30",method="archiveDirect",
         pred_tmp%>%filter(site=="BDF30")%>%
           mutate(pred=pred_dB_TOC)%>%evaluate_model_adjusted),
  
  tibble(site="BDF35",method="archiveDirect",
         pred_tmp%>%filter(site=="BDF35")%>%
           mutate(pred=pred_dB_TOC)%>%evaluate_model_adjusted)
  
)->stock_pred_eval2
write_excel_csv(stock_pred_eval2,"C:/Users/adam/Desktop/UNI/PhD/DISS/tables/stockpred_archive_OSSL_stats.csv")

# compare stock profiles


ggplot(tmp%>%filter(metric=="mean"),aes(x=(o3+u3)/2,y=value,col=factor(name,levels=c("obs","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC","pred_dB_pred_TOC"))))+
  geom_line(aes(linetype=Device),linewidth=1)+
  geom_ribbon(data=pivot_wider(tmp,names_from = metric,values_from = value),
              aes(ymin = min,
                  ymax=max,
                  y=mean,
                  linetype=Device,
                  fill=factor(name,levels=c("obs","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC","pred_dB_pred_TOC"))),
              alpha=.1,
              color=NA)+
  facet_wrap(~site_id)+
  coord_flip()+
  
  scale_x_reverse("Depth [m]",
                  breaks=seq(0,1.50,.50),
                  minor_breaks=seq(0,1.50,.10))+
  #scale_color_manual(breaks=c("02","23","30","35"),values=colorblind_safe_colors()[c(2:4,8)],labels=paste("BDF",c("02","23","30","35")))+
  scale_linetype_manual("",
                        breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                        values=c("solid","solid","dotdash","dashed","dotted"),
                        labels=c("Soil rings (reference)","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_shape_manual("",
                     breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                     values=c(4,1,1,1,1),
                     labels=c("Soil rings (reference)","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  
  # scale_alpha_manual("",
  #                      breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
  #                      values=c(1,1,1,1,1),
  #                      labels=c("Disturbed samples","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_color_manual("",
                     breaks=c( "obs" ,"predOSSL_dB105_TOC","pred_dB105_pred_TOC", "pred_dB105_TOC","pred_dB_pred_TOC"),
                     values = c(colorblind_safe_colors()[c(4,6,7,8,1)]),
                     labels=c("Measured stock","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)","pred_dB_pred_TOC"))+
  scale_fill_manual("",
                     breaks=c( "obs" ,"predOSSL_dB105_TOC","pred_dB105_pred_TOC", "pred_dB105_TOC","pred_dB_pred_TOC"),
                     values = c(colorblind_safe_colors()[c(4,6,7,8,1)]),
                     labels=c("Measured stock","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)","pred_dB_pred_TOC"))+
  
  scale_y_continuous(expression("Soil TOC stock [T/(ha c"*m^-1*")]"))+
  theme_pubr()+
  geom_vline(xintercept = 0,color="grey50")+
  theme(legend.box="vertical",
        legend.position = "inside",
        legend.position.inside = c(.8,.2),
        #panel.grid = element_line(colour = "grey"),
        panel.grid.major = element_line(colour = "grey",linewidth = .05),
        panel.grid.minor = element_line(colour = "grey",linewidth = .05,linetype = "dotted")
  )+
  ggh4x::facet_wrap2(facets = vars(site_id),
                     labeller = BDF_labeller,
                     scales = "fixed",nrow = 2,
                     axes="all",
                     remove_labels = "all")->vertical_stock_pred

vertical_stock_pred


ggsave(plot=vertical_stock_pred
       ,filename="stock_pred_profile_all5cm.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 8,height = 8)



## Stock calc ####
# make look like stocks in ch4
### FOR REPORT ####
tmp%>%filter(Device!="Profilspaten"&u3<=.5)%>%
  pivot_wider(names_from = "metric",values_from = "value")%>%
  group_by(site_id,Device,name)%>%
  summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(mean)*5)%>% #5 cm incr
  bind_rows(
    (Lagerungsdichte_Referenz_neu%>%
       left_join(select(TRD_neu%>%filter(!is.na(BDF)),
                        all_of(c("BDF-Fläche"="BDF","Horizont","soliTOC"))),
                 by=c("BDF-Fläche","Horizont","Tiefe von", "Tiefe bis"))%>%
       mutate(TOC_budget=soliTOC$TOC/100 #gC/g
              *`TRD g/cm3`# gC/cm3
              *1000#kg/m³
              *10#t/ha*m
              *(abs(Hz_bis-Hz_von)/100))%>%
       #aggregating budget to maxDepth
       mutate(TOC_budget=case_when(
         Hz_bis<=50~TOC_budget,
         Hz_bis>50&Hz_von<50~
           TOC_budget/(Hz_bis-Hz_von)*(50-Hz_von),
         Hz_bis>50~0
       )
       )%>%
       
       group_by(BDF=`BDF-Fläche`,Position=rep(c(1:5),13))%>%
       summarise(TOC_budget_sum=sum(TOC_budget))%>%
       summarise(
         STOCK=mean(TOC_budget_sum),
         minSTOCK=min(TOC_budget_sum),
         maxSTOCK=max(TOC_budget_sum),
       ))%>%
      transmute(
        site_id=paste0("BDF",BDF),
        STOCK=STOCK,
        minSTOCK=minSTOCK,
        maxSTOCK=maxSTOCK,
        Device="Soil rings",
        name="obs2"
      )
  )%>% #                          OSSL                field direct      field                archive direct archvie           meas   ref   
mutate(name=factor(name,levels=c("predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC","pred_dB_TOC","pred_dB_pred_TOC","obs","obs2")),
         Device=factor(Device,levels=c("Quicksampler","Rammkern","Soil rings")))->stock_50_prep
stock_50_prep%>%
  ggplot()+
  geom_col(aes(x=Device,y=STOCK,fill=name,group=name),
           position=position_dodge(preserve = "single"),col="black")+
  geom_errorbar(aes(x=Device,y=STOCK,ymin = minSTOCK,ymax=maxSTOCK,group=name),width=.25,position=position_dodge(width = .91,preserve = "single")
                )+
  facet_wrap(~site_id,ncol=4)+
  theme_pubr()+
  theme(text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        axis.ticks.x = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.25,.8))+
  ggtitle("Soil TOC stocks 0-50 cm")+
  guides(fill=guide_legend(ncol=2,title = NULL))+
  ylab(expression("TOC stock [T C h"*a^-1*"]"))+
  scale_x_discrete(breaks=c("Quicksampler","Rammkern","Soil rings"),labels=c("Quicksampler","Push core",""))+
  scale_fill_manual(breaks = c("predOSSL_dB105_TOC",  "pred_dB105_TOC",                 "pred_dB105_pred_TOC",   "pred_dB_TOC",                              "pred_dB_pred_TOC",                "obs",      "obs2"),
                     values = c(colorblind_safe_colors()[c(4,3,6,8,5,7,2)]),
                     labels = c("Predicted (OSSL)","Predicted (LOSO, direct)","Predicted (LOSO)","Predicted (Archive, direct)","Predicted (Archive)","Measured","Measured (soil rings)")
                    )->barplot_pred_stock
barplot_pred_stock


ggsave(plot=barplot_pred_stock
       ,filename="stock_pred_barplot50.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       width = 10,height = 6)


### table ####
stock_50_prep%>%pivot_wider(names_from = name,values_from = c("minSTOCK","maxSTOCK","STOCK"))%>%write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/stock50pred.csv")
  
# FAIL LAgerungsdichte_ref_neu does not interpolate well

Lagerungsdichte_Referenz_neu%>%
  left_join(BDF_SSL,by=c("LG_sample"="LabelEvent"))%>%
  mutate(Device="VZ",Depth_top=`Tiefe von`/100,Depth_bottom=`Tiefe bis`/100,
         stock=`TOC [wt-%]`*`TRD g/cm3`,TOC=`TOC [wt-%]`,dB=`TRD g/cm3`)%>%
  group_by(site_id,Device,Depth_top,Depth_bottom)%>%
  summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(stock)*Depth_bottom)%>%
  
 
  
   pivot_longer(cols=c("stock_mean","stock_min","stock_max"))%>%
  mutate(metric=paste0("m",str_split_fixed(name,"_m",2)[,2]),name="ref")%>%
  filter(Device!="Profilspaten"&u3<=.5)%>%
  pivot_wider(names_from = "metric",values_from = "value")%>%
  group_by(site_id,Device,name)%>%
  summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(mean)*5)%>%
  bind_rows(tmp%>%
  filter(Device!="Profilspaten"&u3<=.5)%>%
  pivot_wider(names_from = "metric",values_from = "value")%>%
  group_by(site_id,Device,name)%>%
  summarise(minSTOCK=sum(min)*5,maxSTOCK=sum(max)*5,STOCK=sum(mean)*5))%>% #5 cm incr
 mutate(name=factor(name,levels=c("obs","ref","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC")))#%>%

  ggplot()+
    geom_col(aes(x=Device,y=STOCK,fill=name,group=name),
             position=position_dodge(),col="black")+
    geom_errorbar(aes(x=Device,y=STOCK,ymin = minSTOCK,ymax=maxSTOCK,group=name),width=.25,position=position_dodge(width = .91)
    )+
    facet_wrap(~site_id,ncol=4)+
    theme_pubr()+
    theme(axis.title.x = element_blank())+
    ylab(expression("Soil TOC stock (0-50 cm) [T C h"*a^-1*"]"))#+
    scale_fill_manual("",
                      breaks = c("obs","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC"),
                      values = c(colorblind_safe_colors()[c(2,4,6,7)]),
                      labels = c("Measured","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)"))




    ggplot()+
  geom_col(aes(x=Device,y=STOCK,fill=name,group=name),
           position=position_dodge(),col="black")+
  geom_errorbar(aes(x=Device,y=STOCK,ymin = minSTOCK,ymax=maxSTOCK,group=name),width=.25,position=position_dodge(width = .91)
  )+
  facet_wrap(~site_id,ncol=4)+
  theme_pubr()+
  theme(axis.title.x = element_blank())+
  ylab(expression("Soil TOC stock (0-50 cm) [T C h"*a^-1*"]"))+
  scale_fill_manual("",
                    breaks = c("obs","predOSSL_dB105_TOC","pred_dB105_TOC","pred_dB105_pred_TOC"),
                    values = c(colorblind_safe_colors()[c(2,4,6,7)]),
                    labels = c("Measured","Predicted stock OSSL","Predicted stock saxSSL","Predicted stock saxSSL (direct)"))


