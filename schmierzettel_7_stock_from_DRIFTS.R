
# source LOAD chunk from C:/Users/adam/Documents/GitHub/Diss/load-and-eval_BDF-SSL.R
  var_list=c("dB [g/cm3]","pH_CaCl2")
  
### spc-set(s) ####
set_list<-c(
  "spc_sg_rs4",
  "spc_sg_bl_rs4",
  "spc_sg_snv_rs4",
  "spc_sg1d_rs4")

### transformation(s) ####
trans_list<-c("none","log1p")

### progress counter ####
k=length(var_list)*length(trans_list)*length(set_list)
c<-0

### save location ####
# create model folder in /temp; if already exists only Warning message
model_folder<-"/Cubist_models_better-split"
dir.create(paste0(root_dir,"/GitHub/R_main/R_main/temp",model_folder))


### already done runs ####
list.files(paste0(root_dir,"/GitHub/R_main/R_main/temp",model_folder))->done
skip_table<-c()


## set cubist tuning framework ####
### parameter grid ####
trainGrid<-expand.grid(committees=c(1,2,5,10,20,50), #typcally no imprvement beyond 50 committees
                       neighbors=c(0:9))

### cross-validation ####
fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10,
  ## print progress
  verboseIter = TRUE,
  returnData = F,
  allowParallel = T)

# main loop ####
for (i in var_list){
  # generating variable + set subset
  if(i%in%c(  "TC",
              "TOC",
              "TOC400",
              "ROC",
              "TIC900")){ #soliTOC variable
    inTrain=createDataPartition(na.omit(BDF_main$soliTOC[[i]]),p = .75,list = F)
  }else{ #BDF variable
    inTrain=createDataPartition(na.omit(BDF_main$BDF_database[[i]]),p = .75,list = F)
  }
  
  
  for (set in set_list) {
    
    # generating variable + set subset
    if(i%in%c(  "TC",
                "TOC",
                "TOC400",
                "ROC",
                "TIC900")){ #soliTOC variable
      data_subset=na.omit(tibble(BDF_main$soliTOC[,i],BDF_main[,set]))
    }else{ #BDF variable
      data_subset=na.omit(tibble(BDF_main$BDF_database[,i],BDF_main[,set]))
    }
    
    for (trans in trans_list){
      ## preparation ####
      if(trans=="log1p"){
        data_subset[[i]] <- log(data_subset[[i]]+1)
      }
      
      # somewhat equidistant (grouped) partition, by each target variable
      
      train<-data_subset[inTrain,]
      test<-data_subset[-inTrain,]
      
      ### create subsets for run ####
      
      
      
      
      c<-c+1
      cat("\n\n\n\nstarting...\t",i," ",set," ",trans,"\n ",c,"/",k,"\n\n")
      
      ### skip / reject ####
      #### reject cases ####
      # for continuing run when it was aborted - skippes combinations of variable + set + trans that were already calibrated
      skip<-F
      manual<-F
      
      # reject case 1: already calibrated
      if(paste0("cubist_",set,"-",trans,"-",i) %in% done){
        cat("\n\nModel already calibrated - skipping\n")
        skip<-T
        manual<-F
        reason<-"Model already calibrated"
        
        # reject case 2: too small n for meaningful model (n=50 is arbitrary)
      }else if(nrow(train)<50#|nrow(test)<1
      ){
        cat("\n\nN TOO SMALL - skipping\n")
        skip<-T
        manual<-F
        reason<-"low n"
      }
      
      #### legacy reject cases (excluded)  ####
      #        # reject case 3: manually excluded (after error in previous run)
      #      }else if(paste0(trans,i)%in%paste0(skip_table$transformation,skip_table$variable)){
      #        if(filter(skip_table,transformation==trans&variable==i)%>%pull(force_exclude)==T){
      #          cat("\n\nForced excluded due to ",filter(skip_table,transformation==trans&variable==i)%>%pull(reason),"\n - skipping\n")
      #          skip<-T
      #          manual<-T
      #        }
      #
      #        # reject case 4: negative or zero values - log impossible, causes -inf or NA
      #      }else if(trans=="log"&(any(train$Y<=0)#|any(test$Y<=0)|any(gt300$Y<=0)
      #      )){
      #        cat("\n\nNEGATIVE VALUES - skipping log\n")
      #        skip<-T
      #        manual<-F
      #        reason<-"negative values"
      #      }else if(trans=="log(1+p)"&(any(train$Y<=-1)#|any(test$Y<=-1)|any(gt300$Y<=-1)
      #      )){
      #        cat("\n\nNEGATIVE VALUES - skipping log(1+p)\n")
      #        skip<-T
      #        manual<-F
      #        reason<-"negative values after 1+p"
      #      }
      ### transformations ####
      #otherwise continue:
      if (!skip){
        # log1p better, log discontinued
        #if(trans=="log"){
        #  train[[i]]<-log(train$[[i]])
        #  test[[i]]<-log(test$[[i]])
        #  gt300[[i]]<-log(gt300$[[i]])
        #}
        
        
        
        
        
        ## Cubist fitting ####
        ### fitting model ####
        out<-train(x = train[[set]],
                   y = train[[i]],
                   method = "cubist",
                   trControl = fitControl,
                   tuneGrid=trainGrid
        )
        
        ### evaluation ####
        if(trans=="none"){
          # test
          ObsPred_test<-data.frame(
            obs=test[[i]],
            pred=predict(out,test[[set]]))
          
        }else if(trans=="log1p"){
          # test
          ObsPred_test<-data.frame(
            obs=exp(test[[i]])-1,
            pred=exp(predict(out,test[[set]]))-1)
          
        }
        
        eval_test<-evaluate_model_adjusted(ObsPred_test,obs="obs",pred="pred")
        
        ### adding documentation ####
        out$documentation<-list(
          inTrain=inTrain,
          train_data=train,
          test_data=test,
          spc_set=set,
          variable=i,
          transformation=trans,
          ObsPred_test=ObsPred_test,
          evaluation_test=eval_test,
          created_at=Sys.time()
        )
        
        ### save model ####
        saveRDS(out,file = paste0(root_dir,"/GitHub/R_main/R_main/temp",model_folder,"/cubist_",set,"-",trans,"-",i))
        
      }
      
      ### list skipped model runs ####
      if (skip&!manual){
        skip_table<-bind_rows(skip_table,data_frame(set=set,transformation=trans,variable=i,reason=reason,force_exclude=F))
      }
      
      cat("\n\ncompleted...\t",i," ",set," ",trans,"\n ",c,"/",k,"\n\n")
    }
  }
}


# after finished, start SVM
#source("~/Documents/GitHub/R_main/R_main/SVM_fit_all.R")



