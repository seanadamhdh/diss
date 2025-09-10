

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

  if(!require(caret)){
    install.packages("caret")
    require(caret)
  }
  # if(!require(pcv)){
  #   install.packages("pcv")
  #   require(pcv)
  # }

  if(!require(kohonen)){
    install.packages("kohonen")
    require(kohonen)
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
    left_join(Campaign%>%rename(Campaign=`Campaign Label`),Event_table,by="Campaign")%>%
    left_join(Data_table_reference,by="LabelEvent")%>%
    left_join(Data_table_soliTOC,by="LabelEvent")%>%
    left_join(Data_table_field_analysis,by="LabelEvent",suffix = c("","f"))%>%
    mutate(`S [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`S [wt-%]f`,`S [wt-%]`),
           `U [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`U [wt-%]f`,`U [wt-%]`),
           `T [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`T [wt-%]f`,`T [wt-%]`),
           `fU [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`fU [wt-%]f`,`fU [wt-%]`),
           `mU [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`mU [wt-%]f`,`mU [wt-%]`),
           `gU [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`gU [wt-%]f`,`gU [wt-%]`),
           `fS [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`fS [wt-%]f`,`fS [wt-%]`),
           `mS [wt-%]` = if_else(Campaign=="DE-2023-BDF_field",`mS [wt-%]f`,`mS [wt-%]`),
           `gS [wt-%]`= if_else(Campaign=="DE-2023-BDF_field",`gS [wt-%]f`,`gS [wt-%]`)
    )%>%
    select(-contains("]f"))%>%
    left_join(tibble(LabelEvent=Data_table_DRIFTS$LabelEvent,
                     spc_rs=Data_table_DRIFTS[-1]),by="LabelEvent")


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
    "Zn_t" = "Zn_t [mg/kg]",
    "FSS40" = "FSS_40 [g/cm3]",
    "FSS105" = "FSS_105 [g/cm3]"
  )




  #### quality palette ####
  bg_colors <- setNames(
    colorRampPalette(c("#d73027","#fee090","#1a9850"))(6),c("vp", "p", "f", "g", "vg", "e"))




  left_join(BDF_SSL%>%select(-spc_rs),spc_tmp)->BDF_SSL_spcPrep

}

{
# Initialisation ####
## init parameters ####
### variable name(s) ####
# main vars only

var_list=c("pH_CaCl2","dF [g/cm3]","dB [g/cm3]","pF_1_8 [Vol_%]","pF_2_5 [Vol-%]","pF_4_2 [Vol-%]")

var_list=BDF_SSL%>%filter(!str_detect(Campaign,"field"))%>%select(!contains("spc"))%>%select(where(is.numeric))%>%select(where(~sum(!is.na(.))>100))%>%names

var_list=c("FSS_40 [g/cm3]","dB_40FB [g/cm3]","dB_40 [g/cm3]",
"FSS_105 [g/cm3]","dB_105FB [g/cm3]","dB_105 [g/cm3]")

### spc-set(s) ####
set_list<-c(
  "spc_sg_rs4",
  "spc_sg_snv_rs4",
  "spc_sg1d_rs4")

### transformation(s) ####
trans_list<-c("none","log1p")

### progress counter ####
k=length(var_list)*length(trans_list)*length(set_list)
c<-0

### save location ####
# create model folder in /temp; if already exists only Warning message
model_folder<-"/SVM_models_all"
output_folder=paste0("/home/hydropedo/Desktop/",model_folder)
dir.create(output_folder)


### already done runs ####
list.files(paste0(data_dir,"Sean_Environment/R_main/models",model_folder))->done
skip_table<-c()


## set cubist tuning framework ####
### parameter grid ####
trainGrid<-expand.grid(C=seq(.05,1,.05))

### cross-validation ####
fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10,
  ## print progress
  verboseIter = TRUE,
  returnData = F,
  allowParallel = T)

pb<-progress::progress_bar$new(total=length(trans_list)*length(var_list)*length(set_list))
  # main loop ####
for (i in var_list){
  for (set in set_list) {
    for (trans in trans_list){



      # canibalised from train_sample_size
      data=BDF_SSL_spcPrep
      Xr=set
      Yr=i
      trans=trans
      trans_rev=if_else(trans=="log1p","expm1","none")
      train_ratio = 0.7
      kmeans_pc=.99
      method="svmLinear"
      trControl = fitControl
      tuneGrid=trainGrid
      save_all=F
      save_models=T
      output_folder =output_folder
      return_all=F
      seed=123
        none=function(x){return(x)} # no trans function
        # Split dataset into train/test set
        set.seed(seed)  # For reproducibility

        # make sure var match name, only Xr,Yr, complete cases only
        data=(rename(data,Xr={{Xr}},Yr={{Yr}})%>%select(Xr,Yr)%>%na.omit)
        data=tibble(Yr=data$Yr,Xr=as.matrix(data$Xr))%>%na.omit

        # apply trans function if defined
        data$Yr=match.fun(trans)(data$Yr)



        #print("DEBUG 1")


        # Define the K-means sampling function
        kmeans_sampling <- function(dataset, num_samples) {

          if (ncol(dataset$Xr)<3){
            kmeans_result <- naes(dataset$Xr, k=num_samples) # no pc decomposition for clustering
          }else{
            kmeans_result <- naes(dataset$Xr, k=num_samples, pc=kmeans_pc)
          }


          inSample <- kmeans_result$model
          return(inSample)
        }

        # split - holding test set back
        trainIndex <- kmeans_sampling(data,train_ratio*nrow(data))
        train_set <- data[trainIndex, ]
        test_set <- data[-trainIndex, ]


           #print("DEBUG 2")


        # Create directory to store models
        if ((save_models|save_all)&!dir.exists(output_folder)) {
          dir.create(output_folder)
        }

        # Train models for each sample size ####
        # ... for each value in sample_sizes loop

        message("Building models...")


          # Train the model
          # note: built in preprocessing in train for Xr...
          model <- train(y=train_set$Yr,
                         x=as.matrix(train_set$Xr),
                         method=method,
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         )


          #print("DEBUG 3")

          # appending input data ... some might be redundant, but does not use too much space and "besser haben als brauchen"
          model$documentation[["input"]]=list("model_type"=method,
                                              "Xr" = Xr,
                                              "Yr" = Yr,
                                              "trans" = trans,
                                              "trans_rev" = trans_rev,
                                              "seed" = seed,
                                              "tune_grid" = tuneGrid,
                                              "train_ratio" = train_ratio,
                                              "kmeans_pc" = kmeans_pc,
                                              "train_control" = trControl,
                                              "split"=train_set)
          #including test set in model output for streamlined evaluation
          model$documentation[["test_set"]]=test_set

          model$documentation$evaluation=tibble(# eval stats
                                       evaluate_model_adjusted(
                                         #apply reverse transformation
                                         match.fun(trans_rev)(
                                           data.frame(
                                             obs=model$documentation$test_set$Yr,
                                             pred=predict(
                                               model,
                                               model$documentation$test_set$Xr
                                             )
                                           )
                                         ),
                                         obs="obs",
                                         pred="pred"
                                       )
                                     )

          #print("DEBUG 4")

          # Save the model as RDS
          model_filename <- paste0(output_folder, "/",method,"_",Xr,"_",trans,"_",str_split_fixed(Yr," ",2)[,1])
          saveRDS(model, model_filename)



          pb$tick()
        }
    }
  }
}



evaluate_model_batch(root_dir = data_dir,
                     model_folder = "/Sean_Environment/R_main/models/SVM_models_all",model_type_pattern = "svmLinear",new_eval = T,verbose_iter = T)
