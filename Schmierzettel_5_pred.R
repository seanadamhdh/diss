
# TOC stock model direct


BDF_SSL_prep%>%filter(!str_detect(Campaign,"field"))%>%transmute(X=spc_sg_snv_rs4,y=`dB [g/cm3]`*`TOC [wt-%]`)%>%na.omit->train
BDF_SSL_prep%>%filter(str_detect(Campaign,"field"))%>%transmute(X=spc_sg_snv_rs4,y=`dB [g/cm3]`*`TOC [wt-%]`)%>%na.omit->test

stock_pls=pls(x = train$X,y=train$y,cv=list("rand",10,10))
stock_pls%>%summary
predict(stock_pls,x = test$X,y=test$y)->stock_pls_pred
stock_pls_pred%>%summary




stock_pls=pls(x = train$X,y=train$y,cv=list("rand",10,10))
stock_pls%>%summary
predict(stock_pls,x = test$X,y=test$y)->stock_pls_pred
stock_pls_pred%>%summary





BDF_SSL_prep%>%filter(str_detect(Campaign,"field"))%>%
  transmute(LabelEvent,X=spc_sg_snv_rs4,y=`dB_105 [g/cm3]`*`TOC [wt-%]`)%>%na.omit%>%
  bind_cols(stock_pls_pred$y.pred%>%as_tibble%>%
              rename_with(.fn = function(x){paste0("ypred_PC_",str_remove(x,"Comp ")%>%str_remove(".1"))}))%>%
  left_join(BDF_SSL%>%select(!contains("spc")))->pred_data











eval=c()
testdata=c()
all=list.files(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_all/"),full.names = T)[-1]
pb=progress::progress_bar$new(total=length(all))
for (i in list.files(paste0(data_dir,"Sean_Environment/R_main/models/SVM_models_all/"),full.names = T)[-1]){
  mod=readRDS(i)
  testdata[[basename(i)]]=mod$documentation$evaluation
  eval=bind_rows(eval,tibble(name=basename(i),mod$documentation$evaluation))
  pb$tick()
  #print(basename(i))
}

eval%>%mutate(
  set=str_split_fixed(str_split_fixed(name,"_",2)[,2],"_rs4_",2)[,1],
  trans=str_split_fixed(str_split_fixed(name,"_rs4_",2)[,2],"_",2)[,1],
  var_=str_split_fixed(str_remove(name,trans),"__",2)[,2],
)->eval


eval%>%group_by(var_)%>%filter(rmse==min(rmse))%>%select(var_,set,trans,rmse,R2,rpd,b,bias)%>%view

eval%>%group_by(var_)%>%filter(rmse==min(rmse))%>%
  select(min,max,mean,median,sdev,var_,set,trans,rmse,R2,rpd,b,bias)%>%
  filter(R2>.85&rpd>2&abs(b-1)<.05)%>%
  view


saveRDS(eval,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/svm_all_eval")
saveRDS(testdata,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/svm_all_testdata")

