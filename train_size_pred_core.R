


# LOAD ####
{
  ## paths ####
  if(stringr::str_detect(osVersion,"Windows")){
    #workpc
    data_dir="//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/"
    code_dir="C:/Users/adam/Documents/"
  }else{
    #ubuntu                       
    data_dir="/run/user/1000/gvfs/smb-share:server=zfs1.hrz.tu-freiberg.de,share=fak3ibf/Hydropedo/"
    code_dir="/home/hydropedo/Documents/"
  }
  
  ## packages ####
  
  
  if(!require(remotes)){
    install.packages("remotes")
    require(remotes)
  }
  
  
  if(!require(tidyverse)){
    install.packages("tidyverse")
    require(tidyverse)
  }
  # despite beeing listed in tidyverse_packages() readxl needs to be loaded explicity for some reason
  if(!require(readxl)){
    install.packages("readxl")
    require(readxl)
  }
  
  if(!require(TUBAFsoilFunctions)){
    install_github("seanadamhdh/TUBAFsoilFunctions")
    require(TUBAFsoilFunctions)
  }
  
  if(!require(simplerspec)){
    install_github("https://github.com/philipp-baumann/simplerspec.git")
    require(simplerspec)}
  
  if(!require(ggpubr)){
    install.packages("ggpubr")
    require(ggpubr)
  }
  
  if(!require(ggstatsplot)){
    install.packages("ggstatsplot")
    require(ggstatsplot)
  }
  
  
  if(!require(prospectr)){
    install.packages("prospectr")
    require(prospectr)
  }
  
  
  if(!require(resemble)){
    install.packages("resemble")
    require(resemble)
  }
  
  
  if(!require(ggh4x)){
    install.packages("ggh4x")
    require(ggh4x)
  }
  
  
  if(!require(mdatools)){
    install.packages("mdatools")
    require(mdatools)
  }
  
  # if(!require(pcv)){
  #   install.packages("pcv")
  #   require(pcv)
  # }
  
  if(!require(kohonen)){
    install.packages("kohonen")
  }
  
  if(!require(ChemometricsWithR)){
    install_github("rwehrens/ChemometricsWithR")
    require(ChemometricsWithR)
  }
  if(!require(corrplot)){
    install.packages(corrplot)
    require(corrplot)
  }
  
  ## local paths work pc (backup in diss folder)
  if(stringr::str_detect(osVersion,"Windows")){
    # meta
    Campaign=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Campaign")
    Event_table=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Event table")
    
    # not perfectly machine-readable but ok for quick lookup
    Parameter_table=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Parameter table")
    
    Data_table_soliTOC=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Data table soliTOC")
    Data_table_DRIFTS=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Data table DRIFTS")
    Data_table_reference=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Data table Reference data")
    Data_table_field_analysis=read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/data/Kopie von Pangaea data.xlsx",sheet = "Data table 2023_field analysis")
  }  else{
    ## Ubuntu paths to main Pangea repo
    # meta
    Campaign=read_excel( paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Campaign")
    Event_table=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Event table")
    
    # not perfectly machine-readable but ok for quick lookup
    Parameter_table=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Parameter table")
    
    Data_table_soliTOC=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Data table soliTOC")
    Data_table_DRIFTS=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Data table DRIFTS")
    Data_table_reference=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Data table Reference data")
    Data_table_field_analysis=read_excel(paste0(data_dir,"/projects/Vorstudie C-Monitoring/ESSD/Pangaea/Pangaea data.xlsx"),sheet = "Data table 2023_field analysis")
  }
  
  
  BDF_SSL=
    left_join(Campaign%>%rename(Campaign=`Campaign Label`),Event_table)%>%
    left_join(Data_table_reference)%>%
    left_join(Data_table_soliTOC)%>%
    left_join(Data_table_field_analysis)%>%
    left_join(tibble(LabelEvent=Data_table_DRIFTS$LabelEvent,
                     spc_rs=Data_table_DRIFTS[-1]))
  
  
  tibble(LabelEvent=Data_table_DRIFTS$LabelEvent,
         spc_rs=Data_table_DRIFTS[-1])%>%mutate(spc_rs=spc_rs%>%rename_with(.fn=~str_remove(.,"X")))%>%
    mutate(
      #smoothing
      spc_sg=savitzkyGolay(spc_rs,m=0,p=3,w=21)
    )%>%
    mutate(
      #convex-hull baseline correction
      spc_sg_bl=baseline(X=spc_sg,wav = as.numeric(colnames(spc_sg))),
      #standard normal variate ... effectively scale+center
      spc_sg_snv=standardNormalVariate(X=spc_sg),
      # 1st derivative
      spc_sg1d=savitzkyGolay(spc_rs,m=1,p=3,w=41),
      # 2nd derivative
      spc_sg2d=savitzkyGolay(spc_rs,m=2,p=3,w=41)
    )%>%
    # resampling all to 4 cm-1
    #keep as matrix for 2024 models
    mutate(
      spc_rs4=(resample(spc_rs,wav=colnames(spc_rs),new.wav=seq(max(as.numeric(colnames(spc_rs))),
                                                                min(as.numeric(colnames(spc_rs))),
                                                                -4))),
      spc_sg_rs4=(resample(spc_sg,wav=colnames(spc_sg),new.wav=seq(max(as.numeric(colnames(spc_sg))),
                                                                   min(as.numeric(colnames(spc_sg))),
                                                                   -4))),
      spc_sg_bl_rs4=(resample(spc_sg_bl,wav=colnames(spc_sg_bl),new.wav=seq(7486,414,-4)
                              ## manual above to match original model input
                              #seq(max(as.numeric(colnames(spc_sg_bl))),
                              #min(as.numeric(colnames(spc_sg_bl))),
                              #-4)
      )),
      spc_sg_snv_rs4=(resample(spc_sg_snv,wav=colnames(spc_sg_snv),new.wav=seq(max(as.numeric(colnames(spc_sg_snv))),
                                                                               min(as.numeric(colnames(spc_sg_snv))),
                                                                               -4))),
      spc_sg1d_rs4=(resample(spc_sg1d,wav=colnames(spc_sg1d),new.wav=seq(max(as.numeric(colnames(spc_sg1d))),
                                                                         min(as.numeric(colnames(spc_sg1d))),
                                                                         -4))),
      spc_sg2d_rs4=(resample(spc_sg2d,wav=colnames(spc_sg2d),new.wav=seq(max(as.numeric(colnames(spc_sg2d))),
                                                                         min(as.numeric(colnames(spc_sg2d))),
                                                                         -4)))
      
    )->spc_tmp#%>%  
  # keep all as matrix for now
  
  #VERY IMPORTANT: USE tibble and not as_tibble. Latter bloats spc_tmp to 513 GB!
  #mutate(across(contains("spc")&!contains("rs4"),tibble))->spc_tmp
  
  
  
  
  
  
  
  variable_lookup=list(
    "Al_t"="Al_t [mg/kg]",
    "B_t"="B_t [mg/kg]",
    "Ca_t"="Ca_t [mg/kg]",
    "CORG"="CORG  [wt-%]",
    "Ct"= "Ct [wt-%]",
    "Cu_t" = "Cu_t [mg/kg]",
    "Fe_t" = "Fe_t [mg/kg]" ,
    "fS" = "fS [wt-%]",
    "fU" = "fU [wt-%]",
    "gS"= "gS [wt-%]",
    "gU" = "gU [wt-%]",
    "K_t" = "K_t [mg/kg]",
    "KAKpot" = "KAKpot [cmolc/kg]",
    "Mg_t" = "Mg_t [mg/kg]",
    "Mn_t" = "Mn_t [mg/kg]",
    "Mo_t"= "Mo_t [mg/kg]",
    "mS" = "mS [wt-%]",
    "mU" = "mU [wt-%]",
    "Ni_t" = "Ni_t [mg/kg]",
    "Nt" = "Nt [wt-%]",
    "Pt" = "Pt [wt-%]",
    "ROC" = "ROC [wt-%]",
    "S" = "S [wt-%]",
    "T" = "T [wt-%]",
    "TC" = "TC [wt-%]",
    "TIC900" = "TIC900 [wt-%]",
    "TOC" = "TOC [wt-%]",
    "TOC400" = "TOC400 [wt-%]",
    "U" = "U [wt-%]",
    "Zn_t" = "Zn_t [mg/kg]"
  )
  
  
  
  
  #### quality palette ####  
  bg_colors <- setNames(
    colorRampPalette(c("#d73027","#fee090","#1a9850"))(6),c("vp", "p", "f", "g", "vg", "e"))
  
  
  
## all_data for pred ####
all_data=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))

# all archive samples with spc available
all_archive=all_data%>%filter(!str_detect(Campaign,"field")&
                                !is.na(spc_sg_snv_rs4[,1])&
                                !LabelEvent=="LAAA" #bad scan
)
}



#all_core=all_archive%>%filter(Campaign=="DE-2023-BDF_archive")
#all_pred_core=all_core%>%select(!contains("spc")) #exclude spc to save space
all_pred_core=read_rds(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_pred_core_train.size.models"))
if(F){
  require(progress)
  pb=progress::progress_bar$new(total=800,format = "[:bar] :percent (:current/:total) | :elapsed (:elapsedfull) | ETA: :eta" )
  for (model_type_i in c("pls","cubist","svm","rf")){
    for (repeat_i in str_subset(list.dirs(paste0(data_dir,"/Sean_Environment/R_main/models/train_size/"),full.names = T),
                                model_type_i)){
      for (i in list.files(repeat_i,full.names = T)){
        pred_i=paste0(str_sub(repeat_i,str_length(repeat_i)),"_",basename(i))
        pb$tick()
        if(!(pred_i%in%names(all_pred_core))){
          mod_i=readRDS(i)
          all_pred_core[[pred_i]]=predict(mod_i,all_core$spc_sg_snv_rs4) 
          # save each time to keep progress if r crashes
          saveRDS(all_pred_core,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_pred_core_train.size.models"))
        }
        
      }
      
    }
   
    
  }
  
}

all_pred_core%>%pivot_longer(cols=contains("spc_sg_snv_rs4_TOC"))%>%
  mutate(rep=substr(name,1,1),type=str_split_fixed(name,"_",3)[,2],size=str_split_fixed(name,"TOC_",2)[,2])%>%
  ggplot(aes(x=`TOC [wt-%]`,y=value,col=as.numeric(size)))+
  geom_abline(slope = 1)+
  geom_point()+
  facet_wrap(~type)+
  scale_color_viridis_c(alpha = .1)




