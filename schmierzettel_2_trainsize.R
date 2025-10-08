

# fix SVMlinear repeat 1 n=400
#  "//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo//Sean_Environment/R_main/models/train_size/svmLin_train_size_1/svmLinear_spc_sg_snv_rs4_TOC_400"

train_sample_size(
  data= all_archive%>%filter(Campaign%in%c("DE-2024-BDF_archive","DE-2024-BDF_BfUL"))%>%
    select(`TOC [wt-%]`,spc_sg_snv_rs4)%>%rename(TOC=`TOC [wt-%]`)%>%na.omit,
  Xr = "spc_sg_snv_rs4",
  Yr = "TOC",
  trans = none,
  trans_rev = none,
  train_ratio = 0.7,
  kmeans_pc = 0.99,
  min_samples = 400,
  sample_step = 100,
  method = "svmLinear",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = data.frame(C=c(.1,.5,1,5,10)),
  save_all = F,
  save_models = T,
  output_folder = paste0(data_dir,"/Sean_Environment/R_main/temp/"),
  return_all = F,
  seed = 123,
)



tmpsvm400new=read_rds("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/temp/svmLinear_spc_sg_snv_rs4_TOC_400")




tmpsvm420=read_rds("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo//Sean_Environment/R_main/models/train_size/svmLin_train_size_1/svmLinear_spc_sg_snv_rs4_TOC_420")

