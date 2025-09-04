## run models ####
if(F){
allBDF_OSSL=read.csv("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/DataTable_SPC.csv")%>%
  select(sample_id)%>%rename("LabelEvent"=sample_id)

for (model in
  paste0(data_dir,"/OSSL/OSSL Local/mnt-ossl/ossl_models")%>%
  list.files()%>%
  tibble(x=.)%>%filter(!str_detect(x,"pca"))%>%pull(x) #rm pca model
){
  print(model)
    out_i=predict_ossl_wrapper(input_type="csv",
                             multiple_files=F,
                             path="//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/DataTable_SPC.csv",
                             individual=T,
                             model=model,
                             model_dir=paste0(data_dir,"OSSL/OSSL Local/mnt-ossl/ossl_models/"),
                             str_start = 1,
                             str_end = 4
                             )

  print(out_i)
  #rename for join
  names(out_i)=c("LabelEvent",paste0(names(out_i[2]),c("_pred","_stderr","_lower","_upper","_underrep")))
  allBDF_OSSL=left_join(allBDF_OSSL,out_i,by="LabelEvent")
  }


saveRDS(allBDF_OSSL,"//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/DataTable_OSSLpred")
}


## reload ####

allBDF_OSSL=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/DataTable_OSSLpred")

allBDF_OSSL_data=left_join(BDF_SSL,allBDF_OSSL,by="LabelEvent")
if(F){
(allBDF_OSSL%>%
    names()%>%
    str_remove("_lower")%>%
    str_remove("_upper")%>%
    str_remove("_pred")%>%
    str_remove("_underrep")%>%
    str_remove("_stderr")%>%
    unique())[-1]%>%write.table("OSSL_var.txt",row.names = F,col.names = F)
names(reference_data)[-c(1:6)]%>%write.table("BDF_var.txt",row.names = F,col.names = F)

}


all_ossl_model_names=list.files("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/OSSL Local/ossl-models/ossl_models/")
ossl_mod_path=read.csv("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/OSSL/OSSL Local/mnt-ossl/out/ossl_models_directory_tree.csv")




### OSSL vars avail in BDF ####
OSSL_var_lookup=
c(
"T [wt-%]"="clay.tot_usda.a334_w.pct",
#"H_Wert_pot [cmolc/kg]"="acidity_usda.a795_cmolc.kg",
##### "aggstb_usda.a1_w.pct"
#"Al_d [mg/g]"="al.dith_usda.a65_w.pct",
##### "al.ext_usda.a1056_mg.kg"
##### "al.ext_usda.a69_cmolc.kg"
#"Al_o [mg/g]"="al.ox_usda.a59_w.pct",
##### "awc.33.1500kPa_usda.c80_w.frac"  #pf2.5-pf4.2
##### "b.ext_mel3_mg.kg"
"dB [g/cm3]"="bd_usda.a4_g.cm3",
"Ct [wt-%]"="c.tot_usda.a622_w.pct",
##### "ca.ext_usda.a1059_mg.kg"
##### "ca.ext_usda.a722_cmolc.kg
"CaCO3 [wt-%]"="caco3_usda.a54_w.pct",
"KAKeff [cmolc/kg]"="cec_usda.a723_cmolc.kg",
"Gr_G_SUM [wt-%]"="cf_usda.c236_w.pct",
##### "cu.ext_usda.a1063_mg.kg"
##### "ec_usda.a364_ds.m"
#"Fe_d [mg/g]"="fe.dith_usda.a66_w.pct",
##### "fe.ext_usda.a1064_mg.kg"
#"Fe_o [mg/g]"="fe.ox_usda.a60_w.pct",
##### "k.ext_usda.a1065_mg.kg"
#"K_Ake [cmolc/kg]"="k.ext_usda.a725_cmolc.kg",
##### "mg.ext_usda.a1066_mg.kg"
#"Mg_Ake [cmolc/kg]"="mg.ext_usda.a724_cmolc.kg",
##### "mn.ext_usda.a1067_mg.kg"
##### "mn.ext_usda.a70_mg.kg"
"Nt [wt-%]"="n.tot_usda.a623_w.pct",
##### "na.ext_usda.a1068_mg.kg"
#"Na_Ake [cmolc/kg]"="na.ext_usda.a726_cmolc.kg",
"CORG  [wt-%]"="oc_usda.c729_w.pct",
##### "p.ext_usda.a1070_mg.kg"
##### "p.ext_usda.a270_mg.kg"
##### "p.ext_usda.a274_mg.kg"
##### "s.tot_usda.a624_w.pct"
##### "wr.1500kPa_usda.a417_w.pct" #differnt steps in usda
##### "wr.33kPa_usda.a415_w.pct"
##### "zn.ext_usda.a1073_mg.kg"
"pH_CaCl2"="ph.cacl2_usda.a481_index",
##### "ph.h2o_usda.a268_index"
"S [wt-%]"="sand.tot_usda.c60_w.pct",
"U [wt-%]"="silt.tot_usda.c62_w.pct",

"TOC [wt-%]"="oc_usda.c729_w.pct",
"TC [wt-%]"="c.tot_usda.a622_w.pct",
"Size fraction >2 mm [wt-%]"="cf_usda.c236_w.pct",
#"Size fraction <2 mm [wt-%]"
#"FSS_40 [g/cm3]"
#"dB_40FB [g/cm3]"
#"dB_40 [g/cm3]"
#"FSS_105 [g/cm3]"
#"dB_105FB [g/cm3]"
"dB_105 [g/cm3]"="bd_usda.a4_g.cm3"
)

#### ossl model names and path list ####
ossl_mod_path%>%
  filter(str_detect(file_path,str_c(all_ossl_model_names,collapse = "|")))%>%
  filter(str_detect(file_path,"mir")&str_detect(file_path,"ossl")&str_detect(file_path,"model")&!str_detect(file_path,"error"))%>%
  mutate(path=paste(public_path,file_path,sep="/"))->model_path_ossl

model_path_ossl$OSSL=((model_path_ossl$file_path%>%str_split_fixed("/",2))[,1])%>%str_remove("log..")
write.csv(
  tibble(BDF=OSSL_var_lookup%>%names,
         OSSL=OSSL_var_lookup)%>%
    left_join(model_path_ossl)%>%
    mutate(OSSL_model=file_path%>%str_remove(".qs")%>%str_replace("/","_")),
  "C:/Users/adam/Desktop/UNI/PhD/DISS/tables/ossl_path.csv",
  row.names = F
)

## quick and dirty OSSL asessment ####
out=c()
out_zr=c()
plt=c()
for (i in names(OSSL_var_lookup)){

  out=bind_rows(out,
    tibble(variable=i,
           ossl_model=OSSL_var_lookup[i],
           evaluate_model_adjusted(allBDF_OSSL_data%>%transmute(x=.data[[i]],y=.data[[paste0(OSSL_var_lookup[i],"_pred")]])%>%na.omit(),
                          "x","y")
    )
  )
  
  out_zr=bind_rows(out_zr,
                   tibble(variable=i,
                          ossl_model=OSSL_var_lookup[i],
                          evaluate_model_adjusted(allBDF_OSSL_data%>%filter(LabelEvent%>%str_starts("LC"))%>%transmute(x=.data[[i]],y=.data[[paste0(OSSL_var_lookup[i],"_pred")]])%>%na.omit(),
                                                  "x","y")
                   )
  )
  
  
  plt[[i]]=allBDF_OSSL_data%>%transmute(x=.data[[i]],y=.data[[paste0(OSSL_var_lookup[i],"_pred")]])%>%na.omit()%>%
    ggplot(aes(x=x,y=y))+
    ggtitle(i)+
    geom_point()+
    geom_abline(slope = 1)
}

write_excel_csv(out,"C:/Users/adam/Desktop/UNI/PhD/DISS/tables/OSSLeval.csv")






### RELOAD model eval ####
{
  {
    all_data=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))
    pls_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/evaluation"))
    cubist_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/evaluation"))
    mbl_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Mbl_models_better-split/evaluation"))
    
    bind_rows(
      tibble(type="pls",pls_eval$new_eval_table),
      tibble(type="cubist",cubist_eval$new_eval_table),
      tibble(type="mbl",mbl_eval$new_eval_table)
    )->all_test_eval
    
    
    #tmp=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/cubist_spc_sg1d_rs4-log1p-Mg_t"))
    eval_all=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_eval"))
    eval_zeitreihe=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_zeitreihe"))
    eval_frisch=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_frisch"))
    
  }
  #### predictions load partial ####
  pls_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/pls_pred"))
  
  cubist_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/cubist_pred"))
  
  mbl_frisch=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_frisch"))
  
  mbl_zeitreihe=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_zeitreihe"))
  
}

all_pred=left_join(pls_pred,cubist_pred)%>%left_join(bind_rows(mbl_frisch,mbl_zeitreihe))%>%
  # unlog mbl
  mutate(across(contains("mbl")&contains("log1p"),.fns = ~ .x %>% mutate(pred = expm1(pred))))
ossl_bdf_pred=left_join(all_pred,allBDF_OSSL_data,by=c("ID"="LabelEvent"))

# cursed but works
avail_mod=str_split_fixed((tibble(x=all_pred%>%names())%>%filter(x%>%str_detect("cubist")))%>%pull(x),"-",3)[,3]%>%unique()
plt2=c()
for (i in names(OSSL_var_lookup)){
  
  ii=str_split_fixed(str_split_fixed(i,"  \\[",2)[,1], " \\[",2)[,1]
  
  
  if(ii %in% avail_mod){
  
  in_data=ossl_bdf_pred%>%filter(substr(ID,2,2)!="A")%>%
    mutate(y=.data[[i]],
           y_BDF_pls=.data[[
             get_best(all_test_eval,
                      type_="pls",
                      variable_=ii,
                      metric="rmse",
                      maximise=F)]]$pred,
           y_BDF_cubist=.data[[
             get_best(all_test_eval,
                      type_="cubist",
                      variable_=ii,
                      metric="rmse",
                      maximise=F)]]$pred,
           y_BDF_mbl=.data[[
             get_best(all_test_eval,
                      type_="mbl",
                      variable_=ii,
                      metric="rmse",
                      maximise=F)]]$pred,
           y_OSSL=.data[[paste0(OSSL_var_lookup[[i]],"_pred")]],
  )

 
  
  plt2[[ii]]=ggplot(in_data,aes(x=y))+
    geom_abline(slope = 1)+
    geom_point(aes(y=y_BDF_pls,col="BDF_pls",shape="BDF_pls"),alpha=.5)+
    geom_point(aes(y=y_BDF_cubist,col="BDF_cubist",shape="BDF_cubist"),alpha=.5)+
    geom_point(aes(y=y_BDF_mbl,col="BDF_mbl",shape="BDF_mbl"),alpha=.5)+
    geom_point(aes(y=y_OSSL,col="OSSL",shape="OSSL"),alpha=.5)+
    scale_color_manual("",breaks=c("BDF_pls","BDF_cubist","BDF_mbl","OSSL"),
                       values = colorblind_safe_colors()[c(5,7,2,4)])+
    scale_shape_manual("",breaks=c("BDF_pls","BDF_cubist","BDF_mbl","OSSL"),
                       values=c(15:18))+
    theme_pubr()+
    xlab("Observed")+
    ylab("Predicted")+
    ggtitle(i)
    }
  
}

for (p in names(plt2)){
  ggsave(plot=plt2[[p]],
           filename=paste0(p,"_OSSLvsBDF.png"),
         path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 5,height = 5)
         
  }


### merge ossl and bdf ssl eval ####
eval_OSSL_zr=out_zr%>%
  mutate(variable=str_split_fixed(
    str_split_fixed(variable,"  \\[",2)[,1],
    " \\[",2)[,1],
    type="OSSL")




eval_zeitreihe%>%rowwise()%>%
  mutate(best=get_best(model_eval = all_test_eval,
                  type_ = type,
                  prefix = paste0(type,"_"),
                  variable_ = variable,
                  metric = "rmse",
                  maximise = F
  ))%>%group_by(type,variable)%>%
  filter(set==str_split_fixed(str_split_fixed(best,"-",2)[,1],"_",2)[,2],
         trans==str_split_fixed(best,"-",3)[,2])%>%
  select(-subset)%>%
  #pivot_wider(names_from = type,values_from = names(eval_zeitreihe)[c(3:4,6:37)])%>%
  bind_rows(eval_OSSL_zr)%>%
  # sel. only var avail for both
  filter(variable%in%eval_zeitreihe$variable&
           variable%in%eval_OSSL_zr$variable)->combined_eval

####linsccc####
(
  combined_eval%>%
    ggplot()+
    
    # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
    geom_rect(data=tibble(
      ymin=c(-1,.65,.8,.9,.95,.99),
      ymax=c(.65,.8,.9,.95,.99,1),
      group=c("vp","p","f","g","vg","e"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    
    geom_col(aes(x=variable,y=linsCCC,fill=type),
             position = position_dodge(width = .7),width=.7,col="black")+
    
    coord_cartesian(ylim = c(0,1))+
    
    theme_pubr()+
    theme(#axis.text.x = element_text(angle=45,hjust=1,vjust=1),
      axis.title.x = element_blank())+
    scale_x_discrete(breaks=names(variable_lookup), labels = unlist(variable_lookup)%>%str_replace("  "," "))+
    scale_y_continuous("Lin's CCC",expand = c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),c("pls","cubist","mbl","OSSL")),#"test","gt300"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor (< 0.65)" ,"Poor  (0.65 - 0.80)","Fair (0.80 - 0.90)","Good (0.90 0.95)", "Very Good (0.95 - 0.99)","Excellent (> 0.99)"
        ,c("BDF PLS","BDF Cubist","BDF MBL","OSSL")),
      values = c(unlist(bg_colors),setNames(colorblind_safe_colors()[c(5,2,7,4)],c("pls","cubist","mbl","OSSL"))))
)->plt_ossl_vs_bdf_linccc

plt_ossl_vs_bdf_linccc


ggsave(plot=plt_ossl_vs_bdf_linccc #+theme(legend.position = "none")
       ,filename="plt_ossl_vs_bdf_linccc.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 10,height = 5)
# legend
ggsave(plot=get_legend(plt_ossl_vs_bdf_linccc),filename="plt_ossl_vs_bdf_linccc_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)

####rpd####
(
  combined_eval%>%
    ggplot()+
    
    # williams 2014 (though some authors are far less rigourous with rpd>2 as very good)
    geom_rect(data=tibble(
      ymin=c(-Inf,2,2.5,3,3.5,4),
      ymax=c(2,2.5,3,3.5,4,Inf),
      group=c("vp","p","f","g","vg","e"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_col(aes(x=variable,y=rpd,fill=type),
             position = position_dodge(width = .7),width=.7,col="black")+
    
 #   coord_cartesian(ylim = c(0,1))+
    
    theme_pubr()+
    theme(#axis.text.x = element_text(angle=45,hjust=1,vjust=1),
      axis.title.x = element_blank())+
    scale_x_discrete(breaks=names(variable_lookup), labels = unlist(variable_lookup)%>%str_replace("  "," "))+
    scale_y_continuous("RPD",expand = c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),c("pls","cubist","mbl","OSSL")),#"test","gt300"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor (< 2.0)" ,"Poor (2.0 - 2.5)","Fair (2.5 - 3.0)","Good (3.0 - 3-5)", "Very Good (3.5 - 4.0)","Excellent (> 4.0)"
        ,c("BDF PLS","BDF Cubist","BDF MBL","OSSL")),
      values = c(unlist(bg_colors),setNames(colorblind_safe_colors()[c(5,2,7,4)],c("pls","cubist","mbl","OSSL"))))
)->plt_ossl_vs_bdf_rpd

plt_ossl_vs_bdf_rpd


ggsave(plot=plt_ossl_vs_bdf_linccc #+theme(legend.position = "none")
       ,filename="plt_ossl_vs_bdf_linccc.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 10,height = 5)
# legend
ggsave(plot=get_legend(plt_ossl_vs_bdf_linccc),filename="plt_ossl_vs_bdf_linccc_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)



#' 
#' Merge evaluation tables of OSSL and BDF
#' compare evaluation stats, add plots to diss
#' DONE-ish
#' 
#' Add range or boxplot of evaluation for BDF (best zr != best test (to vis range of accuracy of model runs))
#'  





