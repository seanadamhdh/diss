require(qs)

files=list.files(path = "//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/train_size",
           recursive = T,full.names = T,include.dirs = F,pattern="0")

all_train_size_models=c()
pb=progress::progress_bar$new(total=length(files))
for (i in files){
  all_train_size_models[[basename(i)]]=readRDS(i)
  pb$tick()
}
  

qsave(all_train_size_models,preset="archive")

           