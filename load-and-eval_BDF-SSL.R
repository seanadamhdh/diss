
# INFO ####
#'
#' Skript for initial soliTOC evaluation used in the report
#' C:/Users/adam/Documents/GitLab/phd_code/R_main/soliTOC evaluation.Rmd
#' 
#' 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'




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
"Zn_t" = "Zn_t [mg/kg]"
)
    
    
    
    
#### quality palette ####  
bg_colors <- setNames(
  colorRampPalette(c("#d73027","#fee090","#1a9850"))(6),c("vp", "p", "f", "g", "vg", "e"))

    
    
    
    
    
    
    
    
    
  }
  
  # FIELD ####
  
  ## load some reference data ####
  {
    
    
    # BDF reference data from site characterisation sheets
    Lagerungsdichte_Referenz <- read_excel(paste0(data_dir,"Sean_Environment/R_main/data/soil_physics/Lagerungsdichten_Referenz.xlsx"), na = "NA")
    Lagerungsdichte_Referenz$BDF<-str_remove(Lagerungsdichte_Referenz$BDF,"BDF")
    
    
    BDF_labeller=labeller(site_id=c(`BDF02`="BDF02 (Cropland, Elbe river floodplain)",
                                    `BDF23`="BDF23 (Cropland, Ore mountains)",
                                    `BDF30`="BDF30 (Grassland, Elbe river valley floodplain)",
                                    `BDF35`="BDF35 (Cropland, Northern reaches of the Ore Mountains)"))
    
    BDF_labeller_manual=c(`BDF02`="BDF02 (Cropland, Elbe river floodplain)",
                          `BDF23`="BDF23 (Cropland, Ore mountains)",
                          `BDF30`="BDF30 (Grassland, Elbe river valley floodplain)",
                          `BDF35`="BDF35 (Cropland, Northern reaches of the Ore Mountains)")
    
  }
  
  data=BDF_SSL%>%filter(Campaign%>%str_detect("field")&is.na(Flag))%>%
    group_by(site_id, Profile, Depth_top, Depth_bottom, Device) %>%
    mutate(rep = if_else(Profile=="Profil", as.character(row_number()), "")) %>%
    ungroup()%>%
    mutate(Profile=if_else(Profile%in%c("P1","P2","P3"),
                           paste0("Profil",substr(Profile,2,2)),
                           paste0(Profile,rep)
    ))




## Bulk density on TOC - Field samples ####
BDF_SSL%>%filter(Campaign%>%str_detect("field")&is.na(Flag))%>%
  ggplot(aes(x=`TOC [wt-%]`,fill=site_id))+
  geom_density(alpha=.5)+
  geom_boxplot(aes(group=site_id,y=-.125),alpha=.5,width=.25)+
  scale_fill_manual("Sampling site",values=colorblind_safe_colors())



## Sampling visualisation ####
ggplot(data) +
  geom_rect(aes(
    xmin = as.numeric(factor(Profile)) - 0.4,
    xmax = as.numeric(factor(Profile)) + 0.4,
    ymin = Depth_top,
    ymax = Depth_bottom,
    fill = `TOC [wt-%]`,
  ),
  col="black") +
  geom_hline(yintercept = c(0,.1,.3,.5))+
  
  scale_y_reverse() +
  scale_fill_viridis_c(option = "C") +
  facet_grid(Device ~ site_id, scales = "free_x", space = "free_x") +
  labs(
    x = "Profile",
    y = "Depth (cm)",
    fill = "TOC [wt-%]"  # This updates the legend title
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(
    breaks = unique(as.numeric(factor(data$Profile))),
    labels = unique(data$Profile)
  )


ggplot(
)+
  #manual gridlines
  #   geom_hline(yintercept = seq(.6,2.8,.1),col="grey15",linewidth=.025)+
  #  geom_vline(xintercept = seq(0,150,10),col="grey15",linewidth=.025)+
  # geom_vline(xintercept = 0,linewidth=.2,col="black")+
  # BDF Ref  
  geom_line(data = filter(Lagerungsdichte_Referenz,Tiefe<=150)%>%mutate(site_id=paste0("BDF",BDF)),
            aes(x=Tiefe/100,
                y=CORG, # 10e-2 g/cm³ | 1ha =10e8 cm² , 1 T = 10^6 g -> 1 10^-2g/cm³ = 1 T/ha*cm
                color="BDF-Referenz",
                linetype="BDF-Referenz",
                shape="BDF-Referenz"
            ),
            linewidth=1,
  )+
  
  
  # RK PS QS
  geom_line(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
            aes(
              x=(Depth_bottom+Depth_top)/2,
              y=`TOC [wt-%]`,   # same as weighed for TOC determination
              color=Device,
              linetype=Device,
              shape=Device,
              group=paste(site_id,Device,Profile)),
            linewidth=.25)+
  # geom_smooth(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>=0), 
  #             aes(
  #               x=(Depth_bottom+Depth_top)/2,
  #               y=`TOC [wt-%]`,   # same as weighed for TOC determination
  #               color=Device,
  #               linetype=Device,
  #               shape=Device,
  #               group=paste(site_id,Device)),
  #             se=F,linewidth=1,inherit.aes = T)+
  
  
  geom_line(data=TUBAFsoilFunctions::cm_aggregate(
    dataset = data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
    depth_top_col = "Depth_top",
    depth_bottom_col = "Depth_bottom",
    aggregate_list = "TOC [wt-%]",
    group_list = c("Device","site_id"),
    res_out = .05), 
    aes(
      x=(o3+u3)/2,
      y=`TOC [wt-%]`,   # same as weighed for TOC determination
      linetype=Device,
      color=Device,
      shape=Device,
      group=paste(site_id,Device)),
    linewidth=1
  )+
  geom_point(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("G")&Depth_bottom>0),
             aes(
               x=(Depth_bottom+Depth_top)/2,
               y=`TOC [wt-%]`,   # same as weighed for TOC determination
               color="neu",
               linetype="neu",
               shape="neu"
             ),
             size=2,
             stroke=2)+
  #formatting
  coord_flip(xlim=c(1.50,0),ylim=c(0,5.5))+
  scale_x_reverse("Tiefe [cm]",
                  breaks=seq(0,1.50,.50),
                  minor_breaks=seq(0,1.50,.10))+
  #scale_color_manual(breaks=c("02","23","30","35"),values=colorblind_safe_colors()[c(2:4,8)],labels=paste("BDF",c("02","23","30","35")))+
  scale_linetype_manual("",
                        breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                        values=c("solid","solid","dotdash","dashed","dotted"),
                        labels=c("Disturbed samples","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_shape_manual("",
                     breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                     values=c(4,1,1,1,1),
                     labels=c("Disturbed samples","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  
  # scale_alpha_manual("",
  #                      breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
  #                      values=c(1,1,1,1,1),
  #                      labels=c("Disturbed samples","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_color_manual("",
                     breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                     values = c(colorblind_safe_colors()[c(6,7)],"black","black","black"),
                     labels=c("Disturbed samples","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_y_continuous("TOC [M-%]")+
  theme_pubr()+
  geom_vline(xintercept = 0,color="grey50")+
  theme(legend.box="vertical",
        legend.position = "right",
        #panel.grid = element_line(colour = "grey"),
        panel.grid.major = element_line(colour = "grey",linewidth = .05),
        panel.grid.minor = element_line(colour = "grey",linewidth = .05,linetype = "dotted")
  )+
  ggh4x::facet_wrap2(facets = vars(site_id),
                     labeller = BDF_labeller,
                     scales = "fixed",nrow = 2,
                     axes="all",
                     remove_labels = "all")->p

p
#plot
ggsave(plot=p+theme(legend.position = "none"),filename="TOC profile.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)
#legend
ggsave(plot=get_legend(p),filename="TOC profile_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)


(cm_aggregate(dataset=data,
              depth_bottom_col = "Depth_bottom",
              depth_top_col = "Depth_top",
              aggregate_list = "TOC [wt-%]",
              group_list = c("Device","site_id","Profile"),
              res_out = .3))%>%
  group_by(site_id,o3,u3)%>%
  summarise(across(.cols=`TOC [wt-%]`,.fns=~mean(.,na.rm=T)))



## bulk density and FSS calculation effect ####


ggplot(data,aes(x=`Size fraction >2 mm [wt-%]`))+
  
  geom_point(aes(y=(`dB_40FB [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`*100,shape="dB40FB",fill="dB40FB"),col="black",alpha=.25)+
  #geom_smooth(aes(y=(`dB_40FB [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`,shape="dB40FB",col="dB40FB"),method="glm",formula=y~log(100/(100-x)-1),se=F)+
  
  geom_point(aes(y=(`dB_105FB [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`*100,shape="dB105FB",fill="dB105FB"),col="black",alpha=.25)+
  #geom_smooth(aes(y=(`dB_105FB [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`,shape="dB105FB",col="dB105FB"),method="glm",formula=y~log(100/(100-x)-1),se=F)+
  
  
  geom_point(aes(y=(`dB_40 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`*100,shape="dB40",fill="dB40"),col="black",alpha=.25)+
  #geom_smooth(aes(y=(`dB_40 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`,shape="dB40",col="dB40"),method="glm",formula=y~(100/(100-x)-1),se=F)+
  
  geom_point(aes(y=(`dB_105 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`*100,shape="dB105",fill="dB105"),col="black",alpha=.25)+
  #geom_smooth(aes(y=(`dB_105 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`,shape="dB105",col="dB105"),method="glm",formula=y~(100/(100-x)-1),se=F)+
  
  geom_point(aes(y=(`FSS_105 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`*100,shape="FSS105",fill="FSS105"),col="black",alpha=.25)+
  #geom_smooth(aes(y=(`FSS_105 [g/cm3]`-`FSS_40 [g/cm3]`)/`FSS_40 [g/cm3]`,shape="FSS105",col="FSS105"),method="glm",formula=y~x,se=F)+
  
  
  
  # theroretical (dB40-FSS40)/FSS40 mass relation simplifies to ((100/(100-x))-1)
  geom_function(data=data.frame(x=c(1:100)),aes(x=x,shape="dB40",col="dB40",fill="dB40"),fun=function(x) ((100/(100-x))-1)*100)+
  
  # theroretical (dB105-FSS40)/FSS40 mass relation simplifies to ((100/(100-x))-1)
  geom_function(data=data.frame(x=c(1:100)),aes(x=x,shape="dB105",col="dB105",fill="dB105"),fun=function(x) ((100/(100-x))-1-.03)*100)+
  
  
  # theroretical (dBFS40-FSS40)/FSS40, can be simplified from volume relationship to (x/(100-x)/(rho_rock/rho_fine_soil) which is approximately 2.41 for this dataset
  geom_function(data=data.frame(x=c(1:100)),aes(x=x,shape="dB40FB",col="dB40FB",fill="dB40FB"),fun=function(x) (x/(100-x)/2.41)*100)+
  
  # theroretical (dBFS105-FSS40)/FSS40, can be simplified from volume relationship to (x/(100-x)/(rho_rock/rho_fine_soil) which is approximately 2.41 for this dataset
  geom_function(data=data.frame(x=c(1:100)),aes(x=x,shape="dB105FB",col="dB105FB",fill="dB105FB"),fun=function(x) (x/(100-x)/2.41-.03)*100)+
  
  
  # theroretical (FSS105-FSS40)/FSS40, can be simplified to water loss of fine fraction (coarse fraction water loss was disregarded) -a = -.0242 (looked up data)
  geom_function(data=data.frame(x=c(1:100)),aes(x=x,shape="FSS105",col="FSS105",fill="FSS105"),fun=function(x) -.0242*100)+
  
  
  
  coord_cartesian(xlim=c(0,90),ylim=c(-10,850))+
  scale_x_continuous()+
  scale_y_continuous("Relative difference to FSS40 [%]")+
  scale_shape_manual("Relative differernce to FSS40 [%]",breaks=c("dB40FB","dB105FB","dB40","dB105","FSS105"),values = c(21:25))+
  scale_color_manual("Relative differernce to FSS40 [%]",breaks=c("dB40FB","dB105FB","dB40","dB105","FSS105"),values = colorblind_safe_colors()[c(2,7,3,6,4)])+
  scale_fill_manual("Relative differernce to FSS40 [%]",breaks=c("dB40FB","dB105FB","dB40","dB105","FSS105"),values = colorblind_safe_colors()[c(2,7,3,6,4)])+
  theme_minimal()->rel_dB_effect

#plot
ggsave(plot=rel_dB_effect+theme(legend.position = "none")+geom_rect(aes(xmin=0,xmax=5,ymin =-5,ymax=5),fill=rgb(.5,0,0,.01),color=rgb(.5,0,0,1)),filename="Relative_dB_effect.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =4,height = 4)
ggsave(plot=rel_dB_effect+coord_cartesian(xlim = c(0,5),ylim = c(-5,5))+theme(legend.position = "none"),filename="Relative_dB_effect_zoom.png",
       path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 3,height = 3)

#legend
ggsave(plot=get_legend(rel_dB_effect),filename="Relative_dB_effect_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)


#residual water content
read_rds(paste0(data_dir,"/Sean_Environment/R_main/data/soil_physics/atro_full"))->tmp
tmp%>%ungroup%>%transmute(a=(delta/lutro))%>%summarise(mean(a,na.rm=T))



## homogenised depth profile data ####
cm_aggregate(
  dataset = data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
  depth_top_col = "Depth_top",
  depth_bottom_col = "Depth_bottom",
  aggregate_list = "TOC [wt-%]",
  group_list = c("site_id"),
  res_out = .01)%>%
  mutate(
    `0-15 cm`=if_else(u3<=.150001,1,0),
    `0-30 cm`=if_else(u3<=.30001,1,0),
    `15-30 cm`=if_else(o3>=.150001&u3<=.300001,1,0),
    `0-50 cm`=if_else(u3<=.500001,1,0),
    `30-50 cm`=if_else(o3>=.300001&u3<=.500001,1,0),
    `0-50 cm`=if_else(u3<=.500001,1,0),
    `50+ cm`=if_else(u3>.5000001,1,0)
  )%>%pivot_longer(cols = c("0-15 cm","0-30 cm","15-30 cm","30-50 cm","0-50 cm","50+ cm"),
                   names_to = "set",
                   values_to = "inSet")%>%filter(inSet==1)->data_1cm

# n of interpolated segments (not the real n)
data_1cm%>%group_by(site_id,set)%>%summarise(n=n())->count_df_interpolated

# actual samples
data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0)%>%
  mutate(
    `0-15 cm`=if_else(Depth_bottom<=.150001,1,0),
    `0-30 cm`=if_else(Depth_bottom<=.30001,1,0),
    `15-30 cm`=if_else(Depth_top>=.150001&Depth_bottom<=.300001,1,0),
    `0-50 cm`=if_else(Depth_bottom<=.500001,1,0),
    `30-50 cm`=if_else(Depth_top>=.300001&Depth_bottom<=.500001,1,0),
    `50+ cm`=if_else(Depth_bottom>.500001,1,0)
  )%>%
  group_by(site_id)%>%
  summarise(across(c(`0-15 cm`,`0-30 cm`,`15-30 cm`,`30-50 cm`,`0-50 cm`,`50+ cm`),.fns = sum))%>%
  pivot_longer(cols =  c("0-15 cm","0-30 cm","15-30 cm","30-50 cm","0-50 cm","50+ cm"),
               names_to = "set",
               values_to = "n")->count_df

### TOC ####
data_1cm%>%
  ggplot(aes(x=site_id,y=`TOC [wt-%]`))+
  geom_violin(aes(fill=site_id),alpha=.25,width=.9,scale="width")+
  geom_boxplot(aes(fill=site_id),width=.1)+
  stat_summary(shape=4,geom="point",fun = "mean",stroke=1)+
  theme_minimal()+
  xlab("")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))+
  scale_fill_manual("BDF site",values = colorblind_safe_colors()[c(2:4,8)])+
  geom_text(data=count_df_interpolated,
            aes(y=-.2,
                label=paste0("n=",n)
            ),
            size=3)+
  geom_text(data=count_df,
            aes(y=-.6,
                label=paste0("(n=",n,")")
            ),
            size=2)+
  facet_wrap(~set)->TOC_depthincrements
TOC_depthincrements



#plot
ggsave(plot=TOC_depthincrements+theme(legend.position = "none"),filename="TOC_depthIncrements.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =7,height = 5)
#legend
ggsave(plot=get_legend(TOC_depthincrements),filename="TOC_depthIncrements_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

data_1cm%>%summarise_metrics(grouping_variables = c("site_id","set"),variables = "TOC [wt-%]")->TOC_summary

ggstatsplot::grouped_ggbetweenstats(data = data_1cm%>%filter(set%in%c("0-30 cm","30-50 cm", "50+ cm")),x=site_id,y=`TOC [wt-%]`,grouping.var = set,type="np")#%>%
ggsave(filename="TOC_test_diff_site.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 15,height = 10)

ggstatsplot::grouped_ggbetweenstats(data = data_1cm%>%filter(set%in%c("0-30 cm","30-50 cm", "50+ cm")),x=set,y=`TOC [wt-%]`,grouping.var = site_id,type="np")#%>%
ggsave(filename="TOC_test_diff_depth.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 15,height = 10)



ggstatsplot::grouped_ggbetweenstats(data = data_1cm%>%filter(set%in%c("0-15 cm","15-30 cm", "30-50 cm")),x=site_id,y=`TOC [wt-%]`,grouping.var = set,type="np")#%>%

############################################################################################################################################################################################################### #
# insert ####
############################################################################################################################################################################################################### #

# LOAD ####

# set root directory path
# So script can easily be used on different devices. But does not change working directroy for project




{
  # TRD measuerd traditionally (soil cylinders) by us in the scope of the sampling campaign
  Lagerungsdichte_Referenz_neu <- read_excel(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/3_physik/Adam-2024_VZ-TRD.xlsx"))
  # TRD_neu=left_join(
  #   Lagerungsdichte_Referenz_neu%>%mutate(Depth_avg=(`Tiefe von`+`Tiefe bis`)/2,site_id=paste0("BDF",`BDF-Fläche`))%>%
  #     group_by(site_id,`Soil horizon`=Horizont,Depth_top=`Tiefe von`,Depth_bottom=`Tiefe bis`,Depth_avg)%>%
  #     summarise(minTRD=min(`TRD g/cm3`),maxTRD=max(`TRD g/cm3`),TRD=mean(`TRD g/cm3`)),
  #   
  #   #manually assigning Horizont for merge
  #   data%>%select(-contains("spc"))%>%
  #     mutate(
  #           `Soil horizon`=case_match(site_id,
  #                                "BDF02"~case_match(as.character(Depth_top),
  #                                                "6"~"Ap",
  #                                                "20"~"rAp",
  #                                                "35"~"aM",
  #                                                "65"~"II aM"
  #                                ),
  #                                
  #                                "BDF23"~case_match(as.character(Depth_top),
  #                                                "2"~"Ap",
  #                                                "38"~"Sw-Bv"),
  #                                
  #                                "BDF30"~case_match(as.character(Depth_top),
  #                                                "4"~"aAxh",
  #                                                "30"~"aAxh_2",
  #                                                "54"~"Axh-aM"
  #                                ),
  #                                
  #                                "BDF35"~case_match(as.character(Depth_top),
  #                                                "5"~"Ap",
  #                                                "20"~"rAp",
  #                                                "38"~"Sw",
  #                                                "65"~"Sdw"
  #                                )
  #            )
  #     ),
  #   by=c(
  #     "site_id",
  #     "Soil horizon"
  #   )
  # )
  # 
}




pivot_wider(Lagerungsdichte_Referenz_neu%>%
              mutate(No=rep(c(1:5),13))%>%
              select(-Stechzylindernummer,-Größe),
            names_from=No,
            names_prefix = "VZ_", 
            values_from = `TRD g/cm3`)->VZ_ref


#View(VZ_ref)
# assigning reference VZ samples to other sampling methods
data%>%filter(!substr(LabelEvent,2,2)=="G")%>%mutate(
                       VZ_ref_ID=case_match(site_id,
                                            "BDF02"~case_when(
                                              
                                              Depth_top<.15&
                                                Depth_bottom<=.15~"LGAJ",
                                              
                                              Depth_top>=.15&
                                                Depth_top<.32&
                                                Depth_bottom>.15&
                                                Depth_bottom<=.32~"LGAK",
                                              
                                              Depth_top>=.32&
                                                Depth_top<.50&
                                                Depth_bottom>.32&
                                                Depth_bottom<=.50~"LGAL",
                                              
                                              Depth_top>=.50&
                                                Depth_top<1.02&
                                                Depth_bottom>.50&
                                                Depth_bottom>1.02~"LGAM"
                                              ),
                                            
                                            
                                            "BDF23"~ case_when(
                                              
                                              Depth_top<.20&
                                                Depth_bottom>=.20~"LGAH",
                                              
                                              Depth_top>=.2&
                                                Depth_top<.70&
                                              Depth_bottom>.20&
                                              Depth_bottom<=.70~"LGAI"
                                              ),
                       
                                            "BDF30" ~ case_when(
                                              Depth_top < 0.10 & 
                                                Depth_bottom <= 0.10 ~ "LGAG",
                                              
                                              Depth_top >= 0.10 & 
                                                Depth_top < 0.45 & 
                                                Depth_bottom > 0.10 & 
                                                Depth_bottom <= 0.45 ~ "LGAF",
                                              
                                              Depth_top >= 0.45 & 
                                                Depth_top < 0.90 &
                                                Depth_bottom > 0.45 &
                                                Depth_bottom <= 0.90 ~ "LGAE"
                                            ),
                                            
                                            
                                            "BDF35" ~ case_when(
                                              Depth_top < 0.20 & 
                                                Depth_bottom <= 0.20 ~ "LGAA",
                                              
                                              Depth_top >= 0.20 &
                                               Depth_top < 0.30 &
                                               Depth_bottom > 0.20 &
                                               Depth_bottom <= 0.30 ~ "LGAB",
                                        
                                              Depth_top >= 0.30 &
                                               Depth_top < 0.60 &
                                               Depth_bottom > 0.30 &
                                               Depth_bottom <= 0.60 ~ "LGAC",
                                              
                                              Depth_top >= 0.60 &
                                               Depth_top < 0.80 &
                                               Depth_bottom > 0.60 &
                                               Depth_bottom <= 0.80 ~ "LGAD"
                                            )
                                            
                                            # "BDF30"~ case_when(
                                            #   Depth_top%in%c(0:10)/100&
                                            #     Depth_bottom%in%c(0:10)/100~"LGAG",
                                            #   Depth_top%in%c(10:45)/100&
                                            #     Depth_bottom%in%c(10:45)/100~"LGAF",
                                            #   Depth_top%in%c(45:90)/100&
                                            #     Depth_bottom%in%c(45:90)/100~"LGAE"),
                                            # "BDF35"~ case_when(
                                            #   Depth_top%in%c(0:20)/100&Depth_bottom%in%c(0:20)/100~"LGAA",
                                            #   Depth_top%in%c(20:30)/100&Depth_bottom%in%c(20:30)/100~"LGAB",
                                            #   Depth_top%in%c(30:60)/100&Depth_bottom%in%c(30:60)/100~"LGAC",
                                            #   Depth_top%in%c(60:80)/100&Depth_bottom%in%c(60:80)/100~"LGAD")
                       ))%>%#select(site_id,LabelEvent,Depth_top,Depth_bottom,VZ_ref_ID)%>%print(n=999)
  filter(!is.na(VZ_ref_ID)&!str_starts(LabelEvent,"LG"))%>%
  
  mutate(L_fb=`dB_40FB [g/cm3]`,
         L_all=`dB_40 [g/cm3]`,
         A_fb=`dB_105FB [g/cm3]`,
         A_all=`dB_105 [g/cm3]`,
         Typ=Device,
         BDF=site_id)%>%
  
  group_by(VZ_ref_ID,Device)%>%
  mutate(No=c(1:length(Device)),set=substr(LabelEvent,2,2))%>%
  ungroup()->prep

#PS
prep%>%filter(set=="P")%>%
  pivot_wider(names_from=c(No),values_from=c(L_fb,L_all,A_fb,A_all))%>%
  left_join(VZ_ref,by=c("VZ_ref_ID"="LG_sample"))->PS_prep

prep%>%filter(set=="R")%>%
  pivot_wider(names_from=c(No),values_from=c(L_fb,L_all,A_fb,A_all))%>%
  left_join(VZ_ref,by=c("VZ_ref_ID"="LG_sample"))->RK_prep

prep%>%filter(set=="Q")%>%
  pivot_wider(names_from=c(No),values_from=c(L_fb,L_all,A_fb,A_all))%>%
  left_join(VZ_ref,by=c("VZ_ref_ID"="LG_sample"))->QS_prep


# new approach ####
#' Aggegieren der TRD-Daten für Atro_all (einzig wirklich vergleichbare Daten)
######################################################################### #
RK_diameter=6.35 #cm ~2.5 inch (V = pi * RK_diameter^2 / 4 * (Tiefe_bis - Tiefe_von))
RK_area=(RK_diameter/2)**2*3.1415926 #cm2 (V per 1cm Höhe)
QS_vol=66 #cm³ known syringe volume
PS_area=a=8*2 #cm2 (V per 1cm Höhe) (width*depth, V = PS_area * (Tiefe_bis - Tiefe_von))


### TRD_prep ####
BDF_SSL%>%filter(substr(LabelEvent,2,2)%in%c("Q","R","P"))%>%filter(is.na(Flag))%>%

#BDF_frisch%>%unnest(TRD,names_sep = "_")%>%unnest(sampling_data,names_sep = "_")%>%
#  filter(!is.na(TRD_BDF)&!str_starts(sample_id,"LG"))%>%
  transmute(Device,
            `TOC [wt-%]`,
            LabelEvent,
            Typ=substr(Device,1,1),
            site_id,
            Depth_top,
            Depth_bottom,
            Profile,
            `dB_40 [g/cm3]`,
            `dB_40FB [g/cm3]`,
            `dB_105 [g/cm3]`,
            `dB_105FB [g/cm3]`,
            `FSS_40 [g/cm3]`,
            `FSS_105 [g/cm3]`,
            `Soil horizon`=case_match(site_id,
                                      
                                      "BDF02" ~ case_when(
                                        Depth_top < 0.15 & Depth_bottom <= 0.15 ~ "Ap",
                                        Depth_top >= 0.15 & Depth_top < 0.32 & Depth_bottom > 0.15 & Depth_bottom <= 0.32 ~ "rAp",
                                        Depth_top >= 0.32 & Depth_top < 0.50 & Depth_bottom > 0.32 & Depth_bottom <= 0.50 ~ "aM",
                                        Depth_top >= 0.50 & Depth_top < 1.02 & Depth_bottom > 0.50 & Depth_bottom <= 1.02 ~ "II aM"
                                      ),
                                      
                                      "BDF23" ~ case_when(
                                        Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                        Depth_top >= 0.20 & Depth_top < 0.70 & Depth_bottom > 0.20 & Depth_bottom <= 0.70 ~ "Sw-Bv"
                                      ),
                                      
                                      "BDF30" ~ case_when(
                                        Depth_top < 0.10 & Depth_bottom <= 0.10 ~ "aAxh",
                                        Depth_top >= 0.10 & Depth_top < 0.45 & Depth_bottom > 0.10 & Depth_bottom <= 0.45 ~ "aAxh_2",
                                        Depth_top >= 0.45 & Depth_top < 0.90 & Depth_bottom > 0.45 & Depth_bottom <= 0.90 ~ "Axh-aM"
                                      ),
                                      
                                      "BDF35" ~ case_when(
                                        Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                        Depth_top >= 0.20 & Depth_top < 0.30 & Depth_bottom > 0.20 & Depth_bottom <= 0.30 ~ "rAp",
                                        Depth_top >= 0.30 & Depth_top < 0.60 & Depth_bottom > 0.30 & Depth_bottom <= 0.60 ~ "Sw",
                                        Depth_top >= 0.60 & Depth_top < 0.80 & Depth_bottom > 0.60 & Depth_bottom <= 0.80 ~ "Sdw"
                                      )
            ),
            dh=Depth_bottom-Depth_top,
            vol=case_match(Typ,
                           "Q"~QS_vol,
                           "R"~dh*RK_area,
                           "P"~dh*PS_area),
            vol_corr=vol-(`Size fraction >2 mm [wt-%]`/2.65)
  )%>%bind_rows(
    
    left_join(
      Lagerungsdichte_Referenz_neu%>%
        transmute(vol=`Größe`,
                  `Soil horizon`=Horizont,
                  site_id=paste0("BDF",`BDF-Fläche`),
                  Depth_top=`Tiefe von`/100,
                  Depth_bottom=`Tiefe bis`/100,
                  `dB_105 [g/cm3]`=`TRD g/cm3`,
                  Typ="VZ"),
      BDF_SSL%>%filter(str_starts(LabelEvent,"LG"))%>%select(`TOC [wt-%]`,LabelEvent,site_id,`Soil horizon`,Profile,Depth_top,Depth_bottom)%>%
        mutate(
          `Soil horizon`=case_match(site_id,
                                    
                                    "BDF02" ~ case_when(
                                      Depth_top < 0.15 & Depth_bottom <= 0.15 ~ "Ap",
                                      Depth_top >= 0.15 & Depth_top < 0.32 & Depth_bottom > 0.15 & Depth_bottom <= 0.32 ~ "rAp",
                                      Depth_top >= 0.32 & Depth_top < 0.50 & Depth_bottom > 0.32 & Depth_bottom <= 0.50 ~ "aM",
                                      Depth_top >= 0.50 & Depth_top < 1.02 & Depth_bottom > 0.50 & Depth_bottom <= 1.02 ~ "II aM"
                                    ),
                                    
                                    "BDF23" ~ case_when(
                                      Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                      Depth_top >= 0.20 & Depth_top < 0.70 & Depth_bottom > 0.20 & Depth_bottom <= 0.70 ~ "Sw-Bv"
                                    ),
                                    
                                    "BDF30" ~ case_when(
                                      Depth_top < 0.10 & Depth_bottom <= 0.10 ~ "aAxh",
                                      Depth_top >= 0.10 & Depth_top < 0.45 & Depth_bottom > 0.10 & Depth_bottom <= 0.45 ~ "aAxh_2",
                                      Depth_top >= 0.45 & Depth_top < 0.90 & Depth_bottom > 0.45 & Depth_bottom <= 0.90 ~ "Axh-aM"
                                    ),
                                    
                                    "BDF35" ~ case_when(
                                      Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                      Depth_top >= 0.20 & Depth_top < 0.30 & Depth_bottom > 0.20 & Depth_bottom <= 0.30 ~ "rAp",
                                      Depth_top >= 0.30 & Depth_top < 0.60 & Depth_bottom > 0.30 & Depth_bottom <= 0.60 ~ "Sw",
                                      Depth_top >= 0.60 & Depth_top < 0.80 & Depth_bottom > 0.60 & Depth_bottom <= 0.80 ~ "Sdw"
                                    )
          ))%>%
        select(-Depth_top,-Depth_bottom),
      by=c("site_id","Soil horizon")
    )%>%mutate(Device=if_else(Typ=="VZ","Soil ring",NA)))%>%
  mutate(across(.cols = c(`dB_40 [g/cm3]`,
                          `dB_40FB [g/cm3]`,
                          `dB_105 [g/cm3]`,
                          `dB_105FB [g/cm3]`,
                          `FSS_40 [g/cm3]`,
                          `FSS_105 [g/cm3]`),
                .fns = ~.*`TOC [wt-%]`,.names = "TOCstock_{.col}"))->TRD_prep


(
  TRD_prep%>%group_by(Typ,site_id,`Soil horizon`)%>%
    summarise(v=var(`dB_105 [g/cm3]`,na.rm = T))%>%
    ggplot(aes(x=Typ,y=v,fill=Typ))+
    geom_boxplot(alpha=.5,outliers = F)+
    geom_point(shape=4,alpha=.5,position=position_jitter(width = .25))+
    scale_fill_colorblind()+
    theme_pubr()+
    theme(legend.position = "none")+
    ylab("Bulk density variance")+
    scale_x_discrete("Sampling method",breaks=c("P","Q","R","VZ"),labels=c("Sampling spade (n=6)",
                                                         "Quicksampler (n=14)",
                                                         "Push core (n=17)",
                                                         "Soil rings (n=13)"))+
    coord_flip())#%>%


# ggplot(TRD_prep%>%filter(!is.na(`Soil horizon`)),
#        aes(x=Typ,group=Typ,y=`dB_105 [g/cm3]`))+
#   geom_boxplot()+
#   facet_wrap(~paste(site_id,`Soil horizon`))

ggsave(filename="TRD_Varianz_Probenahmetyp.png",width=6,height = 2,
       device = "png",path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/")

### SDEV sampling rep ####
(
  TRD_prep%>%group_by(Typ,site_id,`Soil horizon`)%>%
    summarise(sdev=sd(`dB_105 [g/cm3]`))%>%
    ggplot(aes(x=Typ,y=sdev,fill=Typ))+
    geom_boxplot(alpha=.5,outliers = F)+
    geom_point(shape=4,alpha=.5,position=position_jitter(width = .25))+
    scale_fill_colorblind()+
    theme_pubr()+
    theme(legend.position = "none",axis.title.y = element_blank())+
    ylab("Standard deviation of bulk density")+
    scale_x_discrete("Sampling method",breaks=c("P","Q","R","VZ"),labels=c("Sampling spade (n=6)",
                                                                           "Quicksampler (n=14)",
                                                                           "Push core (n=17)",
                                                                           "Soil rings (n=13)"))+
    coord_flip())->p_TDR_sdev

p_TDR_sdev
ggsave(p_TDR_sdev,filename="TRD_sd_Probenahmetyp.png",width=7,height = 2,
       device = "png",path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/")



### stats tables ####
TRD_prep%>%group_by(Typ,site_id,`Soil horizon`)%>%
  summarise(sdev=sd(`dB_105 [g/cm3]`))%>%summarise_metrics(grouping_variables = "Typ",variables = "sdev")

# note:
#' TRD_plot_ready build below; horizontwise avg for comparability
#' here sdev and other summary metrics refers tocomparison of  horizontwise avg across all
#' unlike sdev of methods which looks at reps of each horizon
TRD_plot_ready%>%ungroup%>%group_by(Typ)%>%
  summarise(evaluate_model_adjusted(tibble(avg,avg_TRD_VZ)%>%na.omit,avg_TRD_VZ,avg))%>%view


#################################################################################################################################################################################################################### #
# bookmark, fixed till here ####
#################################################################################################################################################################################################################### #

ggstatsplot::ggbetweenstats(TRD_prep%>%group_by(Typ,site_id,`Soil horizon`)%>%
                              summarise(sdev=sd(`dB_105 [g/cm3]`)),x=Typ,y=sdev,type = "np")

ggstatsplot::ggbetweenstats(TRD_prep%>%group_by(Typ,site_id,Horizont)%>%
                              summarise(TRD=(A_all)),x=Typ,y=TRD)


# note soil horizon is NA when data coverage is lacking / samples were taken from crossover - not assigned to either
TRD_prep%>%#pull(site_id)
  
  group_by(Typ,site_id,`Soil horizon` )%>%
  summarise(avg_TRD=mean(`dB_105 [g/cm3]`,na.rm = T),
            min_TRD=min(`dB_105 [g/cm3]`,na.rm = T),
            max_TRD=max(`dB_105 [g/cm3]`,na.rm = T),
            n=length(na.omit(`dB_105 [g/cm3]`))
  )%>%select(-n)%>% # n removed for plotting pipeline
  na.omit%>% #rm Horizons without VZ
  pivot_wider(names_from = Typ,values_from = c(avg_TRD,min_TRD,max_TRD))%>%
  pivot_longer(cols=contains(c("avg_TRD","min_TRD","max_TRD"))&!contains("VZ"))%>%
  mutate(metric=str_split_fixed(name,"_",3)[,1],
         Typ=str_split_fixed(name,"_",3)[,3])%>%select(-name)%>%
  pivot_wider(names_from = metric,values_from = value)->TRD_plot_ready

### obspred-style comparison with VZ ####

(
  TRD_plot_ready%>%
    ggplot(aes(x=avg_TRD_VZ,col=site_id,fill=site_id,shape=site_id))+
    geom_abline(slope=1,linetype="dotted")+
    geom_smooth(method="glm",aes(x=avg_TRD_VZ,y=avg,group=Typ),alpha=.2)+  
    geom_point(aes(y=avg),size=4,alpha=.5)+
    geom_errorbar(aes(y=avg,ymin=min,ymax=max),width=.02,alpha=.5)+
    geom_errorbar(aes(y=avg,xmin=min_TRD_VZ,xmax=max_TRD_VZ),width=.02,alpha=.5)+
    scale_shape_manual("BDF site",breaks=c("BDF02","BDF23","BDF30","BDF35"),labels=c("BDF02","BDF23","BDF30","BDF35"),values = c(21:25))+
    scale_fill_manual("BDF site",breaks=c("BDF02","BDF23","BDF30","BDF35"),labels=c("BDF02","BDF23","BDF30","BDF35"),values = colorblind_safe_colors()[c(1:4)])+
    scale_color_manual("BDF site",breaks=c("BDF02","BDF23","BDF30","BDF35"),labels=c("BDF02","BDF23","BDF30","BDF35"),values = colorblind_safe_colors()[c(1:4)])+
    theme_pubr()+
    # geom_label(data=tibble(x=rep(1.5,3),
    #                        y=rep(2.5,3),
    #                        Typ=c("P","Q","R"),
    #                        label=c("a",
    #                                "b",
    #                                "c"),
    #                        BDF="NA"),
    #            aes(x=x,y=y,label=label),
    #            col="black",fill="white")+
    xlab("Bulk density reference (soil rings) [g/cm³]")+
    ylab("Bulk density [g/cm³]")+
    facet_wrap(~case_match(Typ,"P"~"Sampling spade","Q"~"Quicksampler","R"~"Push core"))+
    ggtitle("Comparison of bulk density between sampling methods")+
    theme(legend.position = "inside",legend.position.inside = c(.35,.9),legend.direction = "horizontal"))->p_TRDcomp

p_TRDcomp

ggsave(p_TRDcomp,filename="TRD_scatter_VZ_vs_RK-PS-QS.png",width=7,height = 4,
       device = "png",path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/")



lm(data=TRD_plot_ready%>%filter(Typ=="R"),avg~avg_TRD_VZ)
lm(data=TRD_plot_ready%>%filter(Typ=="Q"),avg~avg_TRD_VZ)
lm(data=TRD_plot_ready%>%filter(Typ=="P"),avg~avg_TRD_VZ)

RMSE(TRD_plot_ready%>%filter(Typ=="Q")%>%pull(avg_TRD_VZ),TRD_plot_ready%>%filter(Typ=="Q")%>%pull(avg))
RMSE(TRD_plot_ready%>%filter(Typ=="R")%>%pull(avg_TRD_VZ),TRD_plot_ready%>%filter(Typ=="R")%>%pull(avg))
RMSE(TRD_plot_ready%>%filter(Typ=="P")%>%pull(avg_TRD_VZ),TRD_plot_ready%>%filter(Typ=="P")%>%pull(avg))


bind_rows(
  tibble(Typ="Q",evaluate_model_adjusted(TRD_plot_ready%>%filter(Typ=="Q"),obs = "avg_TRD_VZ",pred = "avg")),
  tibble(Typ="R",evaluate_model_adjusted(TRD_plot_ready%>%filter(Typ=="R"),obs = "avg_TRD_VZ",pred = "avg")),
  tibble(Typ="P",evaluate_model_adjusted(TRD_plot_ready%>%filter(Typ=="P"),obs = "avg_TRD_VZ",pred = "avg"))
)%>%View



TRD_prep%>%
  group_by(BDF,Horizont,Typ)%>%
  summarise(avg_TRD=mean(A_all,na.rm = T),
            min_TRD=min(A_all,na.rm = T),
            max_TRD=max(A_all,na.rm = T),
            n=length(na.omit(A_all)))%>%
  select(-c(min_TRD,max_TRD,n))%>%
  pivot_wider(names_from =Typ,values_from = avg_TRD)->tmp

cor.test(tmp$P,tmp$Q,use="pairwise.complete.obs",method="s")







### TRD aggregation ####
cm_aggregate(TRD_prep,depth_top_col = "Depth_top",
             depth_bottom_col = "Depth_bottom",
             group_list = c("site_id","Typ"),
             aggregate_list = c("dB_40 [g/cm3]",
                                "dB_40FB [g/cm3]",
                                "dB_105 [g/cm3]",
                                "dB_105FB [g/cm3]",
                                "FSS_40 [g/cm3]",
                                "FSS_105 [g/cm3]",
                                "TOC [wt-%]",
                                "TOCstock_dB_40 [g/cm3]",
                                "TOCstock_dB_40FB [g/cm3]",
                                "TOCstock_dB_105 [g/cm3]",
                                "TOCstock_dB_105FB [g/cm3]",
                                "TOCstock_FSS_40 [g/cm3]",
                                "TOCstock_FSS_105 [g/cm3]"),
             res_out = .05)->TRD_prep_agg







TRD_prep_agg%>%filter(Typ!="P")%>%
  mutate(depth_class=case_when(u3<=.150001~"15",
                               u3>.150001&u3<=.30001~"30",
                               u3>.30001&u3<=.50001~"50",
                               .default = "else"))%>%
  summarise_metrics(variables = "dB_105 [g/cm3]",
                    grouping_variables = c("site_id","depth_class"))%>%view("site top")






ggplot(TRD_prep_agg,aes(x=(o3+u3)/2,y=`dB_105 [g/cm3]`,fill=Typ,group=paste((o3+u3)/2,Typ)))+
  geom_boxplot(alpha=.1)+
  coord_flip()+
  scale_x_reverse()





ggplot(
)+
  #manual gridlines
  #   geom_hline(yintercept = seq(.6,2.8,.1),col="grey15",linewidth=.025)+
  #  geom_vline(xintercept = seq(0,150,10),col="grey15",linewidth=.025)+
  # geom_vline(xintercept = 0,linewidth=.2,col="black")+
  # BDF Ref  
  geom_line(data = filter(Lagerungsdichte_Referenz,Tiefe<=150)%>%mutate(site_id=paste0("BDF",BDF)),
            aes(x=Tiefe/100,
                y=TRD, # 10e-2 g/cm³ | 1ha =10e8 cm² , 1 T = 10^6 g -> 1 10^-2g/cm³ = 1 T/ha*cm
                color="BDF-Referenz",
                linetype="BDF-Referenz",
                shape="BDF-Referenz"
            ),
            linewidth=1,
  )+
  
  
  # RK PS QS
  geom_line(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
            aes(
              x=(Depth_bottom+Depth_top)/2,
              y=`dB_105 [g/cm3]`,# same as weighed for TOC determination
              color=Device,
              linetype=Device,
              shape=Device,
              group=paste(site_id,Device,Profile)),
            linewidth=.25)+
  # geom_smooth(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>=0), 
  #             aes(
  #               x=(Depth_bottom+Depth_top)/2,
  #               y=`TOC [wt-%]`,   # same as weighed for TOC determination
  #               color=Device,
  #               linetype=Device,
  #               shape=Device,
  #               group=paste(site_id,Device)),
  #             se=F,linewidth=1,inherit.aes = T)+
  
  
  geom_line(data=TUBAFsoilFunctions::cm_aggregate(
    dataset = data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
    depth_top_col = "Depth_top",
    depth_bottom_col = "Depth_bottom",
    aggregate_list = "dB_105 [g/cm3]",
    group_list = c("Device","site_id"),
    res_out = .05), 
    aes(
      x=(o3+u3)/2,
      y=`dB_105 [g/cm3]`,   
      linetype=Device,
      color=Device,
      shape=Device,
      group=paste(site_id,Device)),
    linewidth=1
  )+
  # geom_point(data=TRD_prep%>%filter(Typ=="VZ")%>%
  #              group_by(site_id,`Soil horizon`)%>%
  #              summarise(Depth_top=mean(Depth_top,na.rm=T),
  #                        Depth_bottom=mean(Depth_bottom,na.rm=T),
  #                        `dB_105 [g/cm3]`=mean(`dB_105 [g/cm3]`,na.rm=T)),
  #            aes(
  #              x=(Depth_bottom+Depth_top)/2,
  #              y=`dB_105 [g/cm3]`,  
  #              color="neu",
  #              linetype="neu",
  #              shape="neu"
  #            ),
  #            size=2,
  #            stroke=2)+
  geom_errorbar(data=TRD_prep%>%filter(Typ=="VZ")%>%
               group_by(site_id,`Soil horizon`)%>%
               summarise(Depth_top=mean(Depth_top,na.rm=T),
                         Depth_bottom=mean(Depth_bottom,na.rm=T),
                         n=n(),
                         mean_dB=mean(`dB_105 [g/cm3]`,na.rm=T),
                         min_dB=min(`dB_105 [g/cm3]`,na.rm=T),
                         max_dB=max(`dB_105 [g/cm3]`,na.rm=T)),
             aes(
               x=(Depth_bottom+Depth_top)/2,
               y=mean_dB,  
               ymin=min_dB,
               ymax=max_dB,
               color="neu",
               linetype="neu",
               shape="neu",
               width=(Depth_bottom-Depth_top)
             ),
             linewidth=1
             )+
  
  geom_point(data=TRD_prep%>%filter(Typ=="VZ")%>%
                  group_by(site_id,`Soil horizon`)%>%
                  summarise(Depth_top=mean(Depth_top,na.rm=T),
                            Depth_bottom=mean(Depth_bottom,na.rm=T),
                            n=n(),
                            mean_dB=mean(`dB_105 [g/cm3]`,na.rm=T),
                            min_dB=min(`dB_105 [g/cm3]`,na.rm=T),
                            max_dB=max(`dB_105 [g/cm3]`,na.rm=T)),
                aes(
                  x=(Depth_bottom+Depth_top)/2,
                  y=mean_dB,  
                  # ymin=min_dB,
                  # ymax=max_dB,
                  color="neu",
                  linetype="neu",
                  shape="neu"#,
                  #width=(Depth_bottom-Depth_top)
                ),
                linewidth=1
  )+
  #formatting
  coord_flip(xlim=c(1.50,0),
             ylim=c(0.6,2.3))+
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
                     breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                     values = c(colorblind_safe_colors()[c(6,7)],"black","black","black"),
                     labels=c("Soil rings (reference)","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_y_continuous("Bulk density [g/cm3]")+
  theme_pubr()+
  geom_vline(xintercept = 0,color="grey50")+
  theme(legend.box="vertical",
        legend.position = "right",
        #panel.grid = element_line(colour = "grey"),
        panel.grid.major = element_line(colour = "grey",linewidth = .05),
        panel.grid.minor = element_line(colour = "grey",linewidth = .05,linetype = "dotted")
  )+
  ggh4x::facet_wrap2(facets = vars(site_id),
                     labeller = BDF_labeller,
                     scales = "fixed",nrow = 2,
                     axes="all",
                     remove_labels = "all")->p
#ggplotly(p)
p
#plot
ggsave(plot=p+theme(legend.position = "none"),filename="db profile.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)
#legend
ggsave(plot=get_legend(p),filename="db profile_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)






### TOC stocks ####
#' CHANGE CODE BELOW



TRD_TOC_VZ=
  left_join(
    TRD_prep%>%filter(Typ=="VZ")%>%select(-Profile),
    BDF_SSL%>%filter(str_starts(LabelEvent,"LG"))%>%
      select(`TOC [wt-%]`,LabelEvent,site_id,`Soil horizon`,Profile,Depth_top,Depth_bottom)%>%
    
    mutate(
      `Soil horizon`=case_match(site_id,
                                
                                "BDF02" ~ case_when(
                                  Depth_top < 0.15 & Depth_bottom <= 0.15 ~ "Ap",
                                  Depth_top >= 0.15 & Depth_top < 0.32 & Depth_bottom > 0.15 & Depth_bottom <= 0.32 ~ "rAp",
                                  Depth_top >= 0.32 & Depth_top < 0.50 & Depth_bottom > 0.32 & Depth_bottom <= 0.50 ~ "aM",
                                  Depth_top >= 0.50 & Depth_top < 1.02 & Depth_bottom > 0.50 & Depth_bottom <= 1.02 ~ "II aM"
                                ),
                                
                                "BDF23" ~ case_when(
                                  Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                  Depth_top >= 0.20 & Depth_top < 0.70 & Depth_bottom > 0.20 & Depth_bottom <= 0.70 ~ "Sw-Bv"
                                ),
                                
                                "BDF30" ~ case_when(
                                  Depth_top < 0.10 & Depth_bottom <= 0.10 ~ "aAxh",
                                  Depth_top >= 0.10 & Depth_top < 0.45 & Depth_bottom > 0.10 & Depth_bottom <= 0.45 ~ "aAxh_2",
                                  Depth_top >= 0.45 & Depth_top < 0.90 & Depth_bottom > 0.45 & Depth_bottom <= 0.90 ~ "Axh-aM"
                                ),
                                
                                "BDF35" ~ case_when(
                                  Depth_top < 0.20 & Depth_bottom <= 0.20 ~ "Ap",
                                  Depth_top >= 0.20 & Depth_top < 0.30 & Depth_bottom > 0.20 & Depth_bottom <= 0.30 ~ "rAp",
                                  Depth_top >= 0.30 & Depth_top < 0.60 & Depth_bottom > 0.30 & Depth_bottom <= 0.60 ~ "Sw",
                                  Depth_top >= 0.60 & Depth_top < 0.80 & Depth_bottom > 0.60 & Depth_bottom <= 0.80 ~ "Sdw"
                                )
      ))%>%
      select(-Depth_top,-Depth_bottom),
      by=c("site_id","Soil horizon")
    )
  
  
  
  
  

ggplot()+
  #manual gridlines
  #   geom_hline(yintercept = seq(.6,2.8,.1),col="grey15",linewidth=.025)+
  #  geom_vline(xintercept = seq(0,150,10),col="grey15",linewidth=.025)+
  # geom_vline(xintercept = 0,linewidth=.2,col="black")+
  # BDF Ref  
  geom_line(data = filter(Lagerungsdichte_Referenz,Tiefe<=150)%>%mutate(site_id=paste0("BDF",BDF)),
            aes(x=Tiefe/100,
                y=CORG*TRD, # 10e-2 g/cm³ | 1ha =10e8 cm² , 1 T = 10^6 g -> 1 10^-2g/cm³ = 1 T/ha*cm
                color="BDF-Referenz",
                linetype="BDF-Referenz",
                shape="BDF-Referenz"
            ),
            linewidth=1,
  )+
  
  
  # RK PS QS
  geom_line(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
            aes(
              x=(Depth_bottom+Depth_top)/2,
              y=`TOC [wt-%]`*`dB_105 [g/cm3]`,# same as weighed for TOC determination
              color=Device,
              linetype=Device,
              shape=Device,
              group=paste(site_id,Device,Profile)),
            linewidth=.25)+
  # geom_smooth(data=data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>=0), 
  #             aes(
  #               x=(Depth_bottom+Depth_top)/2,
  #               y=`TOC [wt-%]`,   # same as weighed for TOC determination
  #               color=Device,
  #               linetype=Device,
  #               shape=Device,
  #               group=paste(site_id,Device)),
  #             se=F,linewidth=1,inherit.aes = T)+
  
  
  geom_line(data=TUBAFsoilFunctions::cm_aggregate(
    dataset = data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0),
    depth_top_col = "Depth_top",
    depth_bottom_col = "Depth_bottom",
    aggregate_list = c("dB_105 [g/cm3]","TOC [wt-%]"),
    group_list = c("Device","site_id"),
    res_out = .05), 
    aes(
      x=(o3+u3)/2,
      y=`TOC [wt-%]`*`dB_105 [g/cm3]`,   
      linetype=Device,
      color=Device,
      shape=Device,
      group=paste(site_id,Device)),
    linewidth=1
  )+
  # geom_point(data=TRD_prep%>%filter(Typ=="VZ")%>%
  #              group_by(site_id,`Soil horizon`)%>%
  #              summarise(Depth_top=mean(Depth_top,na.rm=T),
  #                        Depth_bottom=mean(Depth_bottom,na.rm=T),
  #                        `dB_105 [g/cm3]`=mean(`dB_105 [g/cm3]`,na.rm=T)),
  #            aes(
  #              x=(Depth_bottom+Depth_top)/2,
  #              y=`dB_105 [g/cm3]`,  
  #              color="neu",
  #              linetype="neu",
  #              shape="neu"
  #            ),
  #            size=2,
  #            stroke=2)+
 
   geom_errorbar(data=TRD_TOC_VZ%>%filter(Typ=="VZ")%>%
                  group_by(site_id,`Soil horizon`)%>%
                  summarise(Depth_top=mean(Depth_top,na.rm=T),
                            Depth_bottom=mean(Depth_bottom,na.rm=T),
                            n=n(),
                            mean_dB=mean(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T),
                            min_dB=min(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T),
                            max_dB=max(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T)),
                aes(
                  x=(Depth_bottom+Depth_top)/2,
                  y=mean_dB,  
                  ymin=min_dB,
                  ymax=max_dB,
                  color="neu",
                  linetype="neu",
                  shape="neu",
                  width=(Depth_bottom-Depth_top)
                ),
                linewidth=1
  )+
  
  geom_point(data=TRD_TOC_VZ%>%filter(Typ=="VZ")%>%
               group_by(site_id,`Soil horizon`)%>%
               summarise(Depth_top=mean(Depth_top,na.rm=T),
                         Depth_bottom=mean(Depth_bottom,na.rm=T),
                         n=n(),
                         mean_dB=mean(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T),
                         min_dB=min(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T),
                         max_dB=max(`dB_105 [g/cm3]`*`TOC [wt-%]`,na.rm=T)),
             aes(
               x=(Depth_bottom+Depth_top)/2,
               y=mean_dB,  
               # ymin=min_dB,
               # ymax=max_dB,
               color="neu",
               linetype="neu",
               shape="neu"#,
               #width=(Depth_bottom-Depth_top)
             ),
             linewidth=1
  )+
  #formatting
  coord_flip()+#xlim=c(1.50,0),
             #ylim=c(0.6,2.3))+
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
                     breaks=c("neu","BDF-Referenz","Profilspaten","Quicksampler","Rammkern"),
                     values = c(colorblind_safe_colors()[c(6,7)],"black","black","black"),
                     labels=c("Soil rings (reference)","Reference (Forberg and Barth, 2020)","Sampling spade","Quicksampler","Push core"))+
  scale_y_continuous("Soil TOC stock [g/cm³]")+
  theme_pubr()+
  geom_vline(xintercept = 0,color="grey50")+
  theme(legend.box="vertical",
        legend.position = "right",
        #panel.grid = element_line(colour = "grey"),
        panel.grid.major = element_line(colour = "grey",linewidth = .05),
        panel.grid.minor = element_line(colour = "grey",linewidth = .05,linetype = "dotted")
  )+
  ggh4x::facet_wrap2(facets = vars(site_id),
                     labeller = BDF_labeller,
                     scales = "fixed",nrow = 2,
                     axes="all",
                     remove_labels = "all")->p
#ggplotly(p)
p
#plot
ggsave(plot=p+theme(legend.position = "none"),filename="stock profile.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)
#legend
ggsave(plot=get_legend(p),filename="stock profile_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)


# same, but keep profiles differentiated - errorbars

stocklabeller=c("TOCstock_dB_40 [g/cm3]"="TOC stock (dB40) [T/ha]",
                   "TOCstock_dB_40FB [g/cm3]"="TOC stock (dB40FB) [T/ha]",
                   "TOCstock_dB_105 [g/cm3]"="TOC stock (dB105) [T/ha]",
                   "TOCstock_dB_105FB [g/cm3]"="TOC stock (dB105FB) [T/ha]",
                   "TOCstock_FSS_40 [g/cm3]"="TOC stock (FSS40) [T/ha]",  
                   "TOCstock_FSS_105 [g/cm3]"="TOC stock (FSS105) [T/ha]" )

TRD_prep_agg%>%
  select( contains("TOCstock_"),o3,u3,site_id,Typ)%>%
  pivot_longer(cols = contains("TOCstock_"))%>%
  filter(o3>=0&u3<.1500001)%>%
  group_by(site_id,Typ,name)%>%
  summarise(totTOCstock=sum(value,na.rm = T))%>%
  bind_rows(tibble(site_id=rep("BDF23",6),Typ=rep("Q",6),
                       totTOCstock=rep(NULL,6),name=names(select(TRD_prep_agg,contains("TOCstock")))))%>%
  ggplot(aes(x=site_id,group=Typ,y=totTOCstock,fill=Typ))+
  geom_col(position="dodge",col="black")+
  scale_fill_manual("Sampling method",
                    breaks=c("P","Q","R","VZ"),
                    labels=c("Sampling spade","Quicksampler","Push core","Soil ring"),
                    values=alpha(colorblind_safe_colors()[1:4],.5))+
  theme_pubr()+
  ylab(expression("TOC stock [T h"*a^-1*" c"*m^-1*"]"))+
  xlab("BDF site")+
  facet_wrap(~name,labeller=labeller(name=stocklabeller))


### stats ####

TRD_prep_agg%>%
  group_by(site_id,Typ)%>%
  summarise(max_depth=max(u3))



TRD_prep_agg%>%
  filter(o3>=0&u3<.1500001)%>%
  group_by(site_id,Typ)%>%
  summarise(totTOCstock15=sum(`TOCstock_dB_105 [g/cm3]`,na.rm = T))



################################################################################################################################################################################################################# #
# end of insert ####
############################################################################################################################################################################################################### #




# SOLITOC EVAL ####


{## load variant comparison dataset (not included in BDF-SSL) ####
  
  ### PRECISION MEASURES ####
  
  DINref_EM=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-frisch.xlsx"),set_id = "DINref",set_memo = "EM")
  BDF_std=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-frisch.xlsx"),set_id = "BDFstd",set_memo = NA)
  blank=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-frisch.xlsx"),set_id = c("Blnk","blnk"),set_memo = NA)
  all=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-frisch.xlsx"),set_id = c("[:alpha:]"),set_memo = NA)
  
  
  ### Blank runs for determination of LQ + LD ####
  BDF_leer=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-leer.xlsx"),set_id = "leer",fix_cols = T,set_memo = NA)
  BDF_leer=filter(BDF_leer,Pos.<=40) # loss of pressure (N2 empty), started probably when running std, values are way off from pos. 41 onwards
  Blank_LD_LQ=bind_cols(
    variable=c("TOC400","ROC","TIC900","TOC","TC"),
    AVG=t(summarise(BDF_leer,across(c("TOC400","ROC","TIC900","TOC","TC"),.fns = mean)))[,1],
    SD=t(summarise(BDF_leer,across(c("TOC400","ROC","TIC900","TOC","TC"),.fns = sd)))[,1],
    n=t(summarise(BDF_leer,across(c("TOC400","ROC","TIC900","TOC","TC"),.fns = length)))[,1]
  )%>%mutate(LD=AVG+3*SD,LQ=AVG+10*SD)
  #' 
  #' !!! Denkfehler: Default zu 100mg als Dummy-Einwage. Aber real kein Masse-% Bezug herstellbar. Stattdessen besser auf Peakfläche beziehen.
  #' Konterargument: Lineare Transformation Signal->C-Gehalt. Folglich lediglich a + signal * b / 100. 
  #' Werte sind immernoch valide, aber in wt-% besser greifbar
  
  Blank_LD_LQ_area=bind_cols(
    variable=c("TOC400","ROC","TIC900"),
    AVG=t(summarise(BDF_leer,across(c("TOC400  Fläche","ROC  Fläche","TIC900  Fläche"),.fns = mean)))[,1],
    SD=t(summarise(BDF_leer,across(c("TOC400  Fläche","ROC  Fläche","TIC900  Fläche"),.fns = sd)))[,1],
    n=t(summarise(BDF_leer,across(c("TOC400  Fläche","ROC  Fläche","TIC900  Fläche"),.fns = length)))[,1]
  )%>%mutate(LD=AVG+3*SD,LQ=AVG+10*SD)
  
  ### new DIN standard ####
  DINref_new=pull_set_soliTOC(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2024_raw_soliTOC-frisch.xlsx"),set_id = "DINref",fix_cols = T,set_memo = "new")
  DINref_new%>%
    #filter(Methode=="DIN19539")%>%
    group_by(Methode)%>%summarise(across(c("TOC400","ROC","TIC900","TOC","TC"),.fns=list(avg=mean,stdev=sd)))
  
  
  # main oGS
  BDF_main_soliTOC_oGS=pull_set_soliTOC(
    paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2023_raw_soliTOC-main.xlsx"),
    set_id = "LA",
    fix_cols = T,
    set_memo = "MM400")%>%
    filter(Methode=="DIN19539")%>%
    soliTOC_remove_duplicates(reference = F,method = "latest")
  
  
  # main GS
  BDF_main_soliTOC_GS=pull_set_soliTOC(
    paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/Adam-2023_raw_soliTOC-main.xlsx"),
    set_id = "LA",
    fix_cols = T,
    set_memo = "MM400")%>%
    filter(Methode=="DIN19539GS")%>%
    soliTOC_remove_duplicates(measurement_method = "DIN19539GS",reference = F,method = "latest")%>%
    filter(Name!="LAJQ")#one sample not avail for oGS
  
}



## Plot comparison BDF - soliTOC and variants ####  
### ROC, TIC variant #### 
rbind(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS)%>%pivot_longer(cols = c(ROC,TIC900))%>%
  ggplot(aes(x=Methode,y=value,fill=Methode))+
  #geom_violin(alpha=.5)+
  geom_boxplot()+
  stat_summary(geom="point",shape=4,size=2,stroke=1,fun=mean)+
  xlab("")+
  # instead of scale_y_log10 - this evaluates first and then transform. Needs manually set breaks, though.
  coord_trans(y="log10")+ 
  scale_y_continuous("ROC, TIC900 [wt-%]",breaks=c(0.01,.1,1))+
  scale_fill_manual("Variant",breaks=c("DIN19539","DIN19539GS"),values=c(colorblind_safe_colors()[c(2,4)]))+
  theme_pubclean()+
  #geom_jitter(shape=4,width=.25)+
  facet_wrap(~name)->p_ROC_TIC



p_ROC_TIC
#plot
ggsave(plot=p_ROC_TIC+theme(legend.position = "none"),filename="ROC_TIC_Variants.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
#legend
ggsave(plot=get_legend(p_ROC_TIC),filename="ROC_TIC_Variants_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)



### ref vs oGS vs GS ####
ref_vs_oGS_vs_GS=full_join(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS,by="Name",suffix = c(".oGS",".GS"))%>%
  left_join(BDF_SSL%>%transmute(LabelEvent,Ct=`Ct [wt-%]`,CORG=`CORG  [wt-%]`),by=c("Name"="LabelEvent"))

ref_vs_oGS_vs_GS_eval=rbind(
  # comparison with reference
  data.frame(set="Ct vs TC.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="Ct",pred="TC.GS")),
  data.frame(set="Ct vs TC.oGS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="Ct",pred="TC.oGS")),
  data.frame(set="CORG vs TOC.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="CORG",pred="TOC.GS")),
  data.frame(set="CORG vs TOC.oGS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="CORG",pred="TOC.oGS")),
  
  # comparison between methods
  data.frame(set="TC.oGS vs TC.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="TC.oGS",pred="TC.GS")),
  data.frame(set="TOC.oGS vs TOC.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="TOC.oGS",pred="TOC.GS")),
  data.frame(set="TOC400.oGS vs TOC400.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="TOC400.oGS",pred="TOC400.GS")),
  data.frame(set="ROC.oGS vs ROC.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="ROC.oGS",pred="ROC.GS")),
  data.frame(set="TIC900.oGS vs TIC900.GS",evaluate_model_adjusted(ref_vs_oGS_vs_GS,obs="TIC900.oGS",pred="TIC900.GS"))
)


### TOC vs CORG plot ####


rbind(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS)%>%
  pivot_longer(cols = c(ROC,TIC900))%>%
  left_join(BDF_SSL%>%select(LabelEvent,`Ct [wt-%]`,`CORG  [wt-%]`),by=c("Name"="LabelEvent"))%>%
  ggplot(aes(x=`CORG  [wt-%]`,y=TOC,fill=Methode))+
  geom_abline(slope = 1,linetype="dotted")+
  geom_rect(xmin = 0,ymin = 0,xmax=1,ymax=1,fill="grey80")+
  geom_point(shape=21)+
  geom_smooth(method="glm",aes(col=Methode),se=F)+
  xlab("CORG [wt-%]")+
  ylab("TOC [wt-%]")+
  scale_color_manual("Variant",values=colorblind_safe_colors()[c(2,4)])+
  scale_fill_manual("Variant",values=colorblind_safe_colors()[c(2,4)])+
  theme_pubr()->CORG_TOC_p1

rbind(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS)%>%
  pivot_longer(cols = c(ROC,TIC900))%>%
  left_join(BDF_SSL%>%select(LabelEvent,`Ct [wt-%]`,`CORG  [wt-%]`),by=c("Name"="LabelEvent"))%>%
  ggplot(aes(x=`CORG  [wt-%]`,y=TOC,fill=Methode))+
  geom_abline(slope = 1,linetype="dotted")+
  geom_point(shape=21)+
  geom_smooth(method="glm",aes(col=Methode),se=F)+
  xlab("CORG [wt-%]")+
  ylab("TOC [wt-%]")+
  scale_color_manual("Variant",values=colorblind_safe_colors()[c(2,4)])+
  scale_fill_manual("Variant",values=colorblind_safe_colors()[c(2,4)])+
  theme_pubr()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  coord_cartesian(c(0,1),c(0,1))->CORG_TOC_p2


CORG_TOC_p1
CORG_TOC_p2

#plots
ggsave(plot=CORG_TOC_p1+theme(legend.position = "none"),filename="CORG_TOC_Variants1.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
ggsave(plot=CORG_TOC_p2+theme(legend.position = "none"),filename="CORG_TOC_Variants2.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 3,height = 2)


#legend
ggsave(plot=get_legend(CORG_TOC_p1),filename="CORG_TOC_Variants_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)




rbind(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS)%>%
  pivot_longer(cols = c(ROC,TIC900))%>%
  left_join(BDF_SSL%>%select(LabelEvent,`Ct [wt-%]`,`CORG  [wt-%]`),by=c("Name"="LabelEvent"))%>%
  filter(Methode=="DIN19539")%>%
  ggplot(aes(x=`CORG  [wt-%]`,y=TOC))+
  geom_hex(binwidth=.025)+scale_fill_viridis_c()+
  scale_x_sqrt()+
  scale_y_sqrt()


#### Age of samples effect ####

BDF_SSL%>%
  filter(!str_detect(Campaign,"field")#,!str_detect(Campaign,"BfUL")
         )%>%
  #filter(Campaign=="DE-2023-BDF_archive")%>%
  transmute(DateEvent,x0=difftime(as.POSIXct("2024-12-31 UTC") # setting as "now" (at this time all samples were measuered)
                                                            # NOTE: Exact measurement dates via soliTOC raw data possible, but overkill for this comparison
                                   ,DateEvent,units = "days"),
x1=as.numeric(x0),
x=x1/(365.25)
            ,y=`TOC [wt-%]`-`CORG  [wt-%]`)->tst

#sampling runs
tst_out=c()
for (i in c(1:200)){print(i)
  tst_out=bind_rows(tst_out,
                    tibble(
                      i=lm(data=tst[sample(nrow(tst),200),],formula = y~x)[["coefficients"]][1],
                      s=lm(data=tst[sample(nrow(tst),200),],formula = y~x)[["coefficients"]][2]
                    )
  )
}

tst%>%
  ggplot(aes(x=x,y=y))+
  
  geom_bin2d(drop=F,binwidth=c(.5,.1))+
  scale_fill_gradient2("n",low = "white",midpoint = 3,
                       mid = colorblind_safe_colors()[6],
                       high=colorblind_safe_colors()[1],trans="log1p",na.value = 0,
                       breaks=c(0,1,2,5,10,20,50))+
  #geom_point(shape=4,size=.5)+
 # coord_cartesian(xlim=c(7,30),ylim=c(-1.4,1.4),expand = 0)+
  geom_hline(yintercept = 0,col="black",linetype="dotted")+
  geom_smooth(method="glm",se=T,col="red",fill="red",alpha=.1,linetype="dashed")+
  xlab("Sample age [years]")+
  ylab("Difference in TOC to CORG [wt-%]")+
  theme_pubr()+
    theme(panel.background = element_rect(fill = "white"),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.8,.9))->age_diff
age_diff  
#plots
ggsave(plot=age_diff,filename="CORG_TOC_Age.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)

# uncorrelated:
cor(x=(BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
         transmute(x=as.numeric(max(DateEvent,na.rm = T)-DateEvent)/(365.25 * 24 * 60 * 60))%>%pull(x)),y=(BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
                                                                                                             transmute(y=`TOC [wt-%]`-`CORG  [wt-%]`)%>%pull(y)),use="pairwise.complete.obs",method="k")

# using all data
if(F){
  all_archive%>%
    transmute(x=(max(DateEvent,na.rm = T)-DateEvent)/(365.25 * 24 * 60 * 60)
              ,y=`TOC [wt-%]`-`CORG  [wt-%]`)%>%
  ggplot(aes(x=x,y=y))+
 # geom_hex(bins=50)+
  geom_bin2d(drop=F,binwidth=c(.25,.15))+
  scale_fill_gradient2("n",low = "white",midpoint = 3,
                       mid = colorblind_safe_colors()[6],
                       high=colorblind_safe_colors()[1],trans="log1p",na.value = 0,
                       breaks=c(0,1,2,5,10,20,50))+
  #geom_point()+
  
  geom_hline(yintercept = 0,col="black",linetype="dotted")+
  geom_smooth(method="glm",se=F,col="red",linetype="dashed")+
  xlab("Sample age [years]")+
  ylab("Difference in TOC to CORG [wt-%]")+
  theme_pubr()+
  theme(panel.background = element_rect(fill = "white"))
}

### table variants and reference ####
rbind(BDF_main_soliTOC_oGS,BDF_main_soliTOC_GS)%>%
  left_join(BDF_SSL%>%transmute(LabelEvent,Ct=`Ct [wt-%]`,CORG=`CORG  [wt-%]`),by=c("Name"="LabelEvent"))%>%
  pivot_longer(c(TOC400,ROC,TIC900,TOC,TC,CORG,Ct))%>%
  summarise_metrics(grouping_variables = c("Methode","name"),
                    variables = "value")->stat_sum_variants
write_excel_csv(stat_sum_variants %>%
                  mutate(across(
                    where(~ typeof(.) == "double"),
                    ~ formatC(signif(., digits = 2), format = "f", digits = 2, drop0trailing = TRUE)
                  ))
                ,"C:/Users/adam/Desktop/UNI/PhD/DISS/tables/soliTOC_variants_summary.csv")


### Thermograms ####
# redo

{
  # !!! make sure peak graphics match Name !!!
  soliTOCdata<- read_excel(paste0(data_dir,"/lab_data/soliTOC/BDF_run1v6edit.xlsx"))
  
  
  
  
  fill_v<-function(x,l=length(x),with=NA,after=length(x)){
    return(c(x[1:after],rep(with,l-after)))
  }
  
  
  # Funktion zum Extrahieren der benötigten Informationen aus einer Datei
  extract_info <- function(file_path) {
    # Dateiinhalt einlesen
    lines <- readLines(file_path, warn = FALSE)
    
    # a) Sample Name extrahieren
    sample_name <- str_split(lines[1],": ",simplify = T)[,2]
    
    # b) B-Werte extrahieren
    b_values <- str_extract_all(lines[3], "\\d+")[[1]]
    
    # c) P-Werte extrahieren
    p_values <- str_extract_all(lines[4], "\\d+")[[1]]
    
    # d) Dateiname extrahieren
    file_name <- basename(file_path)
    
    # Tibble mit den extrahierten Informationen erstellen
    tibble(
      file = file_name,
      sample = sample_name,
      c1 = as.numeric(b_values[1]),
      c2 = as.numeric(b_values[3]),
      c3 = as.numeric(b_values[5]),
      c1low = as.numeric(p_values[1]),
      c1high = as.numeric(p_values[2]),
      c2low = as.numeric(p_values[3]),
      c2high = as.numeric(p_values[4]),
      c3low = as.numeric(p_values[5]),
      c3high = as.numeric(p_values[6])
    )
  }
  
  
  files<-list.files(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/2_soliTOC/1_peakgrafiken_main"),full.names = T)
  
  
  
  # SLOW!!!
  
  {
    
    pb <- progress::progress_bar$new(
      format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
      total = length(files),
      complete = "=",   # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">",    # Current bar character
      clear = FALSE,    # If TRUE, clears the bar when finish
      width = 100)      # Width of the progress bar
    
    all_graphics<-tibble()
    for (j in c(1:length(files))){
      i<-files[j]
      
      header_i=extract_info(i)
      data_i <- read.delim(i,
                           header=FALSE, comment.char="#")
      names(data_i)<-c("time",	"oven_1_temp", "oven_2_temp",	"flow_N2", "flow_arm",
                       "Pressure_in", "Pressure_out",	"signal")
      data_i$NameST<-soliTOCdata$Name[j]
      data_i$MemoST<-soliTOCdata$Memo[j]
      data_i$GewichtST<-soliTOCdata$`Gewicht  [mg]`[j]
      data_i$MethodeST<-soliTOCdata$Methode[j]
      # ! this is the raw, uncorrected (or even just partially corrected data)
      #  data_i$TOC<-soliTOCdata$`TOC  [%]`[j]
      #  data_i$TC<-soliTOCdata$`TC  [%]`[j]
      #  data_i$TOC400<-soliTOCdata$`TOC400  [%]`[j]
      #  data_i$ROC<-soliTOCdata$`ROC  [%]`[j]
      #  data_i$TIC900<-soliTOCdata$`TIC900  [%]`[j]
      data_i=bind_cols(data_i,header_i)
      data_i$filename<-i
      
      
      all_graphics<-bind_rows(all_graphics,data_i)
      pb$tick()
    }
    pb$terminate()
    
  }
}

# safety -> run only manually
if(F){
  saveRDS(all_graphics,paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/3_r_scripts/temp/soliTOC_peaks_main"))
}
#### plotting ####



#load
thermograms=readRDS(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/3_r_scripts/temp/soliTOC_peaks_main"))



#### plotting ####


# DIN std
filter(
  thermograms,
  NameST == "DINref" &
    MethodeST == "DIN19539" &
    MemoST %in% c("control", "Control", "control_new", "control_old","")
) %>% ggplot(aes(
  x = time,
  y = signal / GewichtST,
  group = filename,
  col = if_else(MemoST == "control_new", "new", "old")
)) + geom_line(alpha = .5) + xlab("Zeit [s]") + ylab("Standardised Signal [mg-1]") +
  theme_pubr()

{
  #OGS
  {
    filter(
      thermograms,
      str_detect(NameST,"LA") &
        NameST!="LAGT" & #one weired meas.
        #time>200 & time<1500 & #crop
        MethodeST %in% c("DIN19539","DIN19539GS")
    )%>%
      ggplot(aes(
        x = time,
        y = signal / GewichtST,
      ))+
      geom_line(aes(group = filename,alpha=MethodeST)) +
      geom_line(data=filter(
        thermograms,
        str_detect(NameST,"LA") &
          NameST!="LAGT" & # one weired meas.
          #time>200 & time<1500 & #crop
          MethodeST == "DIN19539")%>%group_by(time)%>%summarise(temp=mean(oven_1_temp)),aes(y=temp^2/50),col="red")+
      geom_ribbon(aes(xmin=0,xmax=max(time)),fill="orange",alpha=.1)+
      geom_text(data=tibble(x=c(500,900,1300),
                            y=rep(20000,3),
                            label=c("TOC400","ROC","TIC900")),
                aes(x=x,y=y,label=label))+
      
      scale_x_continuous("Time [s]",breaks=seq(0,1500,500)) +
      scale_y_sqrt(expression("Standardised Signal [m"*g^-1*"]"),
                   breaks=c(0,100,1000,2500,5000,10000,20000),
                   sec.axis=sec_axis(name = "Oven temperature [°C]",transform="identity",breaks=(c(0,100,400,600,900)^2/50),labels=c(0,100,400,600,900))) +
      theme_pubr()+
      theme(axis.title.y.right = element_blank(),
            axis.text.y.right = element_blank(),
            axis.ticks.y.right = element_blank(),
            legend.position = "none")+
      coord_cartesian(ylim = c(0,20000))+
      scale_alpha_manual(breaks=c("DIN19539","DIN19539GS"),values=c(.1,0))+
      ggtitle("DIN19539")
  }->p_oGS
  
  
  
  #GS
  {
    filter(
      thermograms,
      str_detect(NameST,"LA") &
        NameST!="LAGT" & # one weired meas.
        #time>200 & time<1500 & #crop
        MethodeST == "DIN19539GS")%>%
      group_by(time)%>%summarise(flow=mean(flow_arm))%>%mutate(inert=if_else(time<500|flow<mean(flow),"no","yes"))->flow
    filter(
      thermograms,
      str_detect(NameST,"LA") &
        NameST!="LAGT" & # one weired meas.
        #time>200 & time<1500 & #crop
        MethodeST %in% c("DIN19539","DIN19539GS")
    )%>%
      ggplot(aes(
        x = time,
        y = signal / GewichtST,
      ))+
      geom_line(aes(group = filename,alpha=MethodeST)) +
      geom_line(data=filter(
        thermograms,
        str_detect(NameST,"LA") &
          NameST!="LAGT" & # one weired meas.
          #time>200 & time<1500 & #crop
          MethodeST == "DIN19539GS")%>%group_by(time)%>%summarise(temp=mean(oven_1_temp)),aes(y=temp^2/50),col="red")+
      geom_ribbon(aes(xmin=0,
                      xmax=flow%>%group_by(inert)%>%
                        summarise(start=first(time),end=last(time))%>%filter(inert=="yes")%>%
                        pull(start)-1),
                  fill="orange",alpha=.1)+
      geom_ribbon(aes(xmin=flow%>%group_by(inert)%>%
                        summarise(start=first(time),end=last(time))%>%filter(inert=="yes")%>%
                        pull(start),
                      xmax=flow%>%group_by(inert)%>%
                        summarise(start=first(time),end=last(time))%>%filter(inert=="yes")%>%
                        pull(end)),
                  fill="palegreen4",alpha=.1)+
      geom_ribbon(aes(xmin=flow%>%group_by(inert)%>%
                        summarise(start=first(time),end=last(time))%>%filter(inert=="yes")%>%
                        pull(end)+1,
                      xmax=flow%>%group_by(inert)%>%
                        summarise(start=first(time),end=last(time))%>%
                        filter(inert=="no")%>%pull(end)),
                  fill="darkorange",alpha=.1)+
      geom_text(data=tibble(x=c(500,1100,1400),
                            y=rep(20000,3),
                            label=c("TOC400","TIC900","ROC")),
                aes(x=x,y=y,label=label))+
      geom_text(data=tibble(x=c(1000),
                            y=rep(17000,1),
                            label=c("Inert gas phase")),
                aes(x=x,y=y,label=label),
                col="palegreen4")+
      scale_x_continuous("Time [s]",breaks=seq(0,1500,500)) +
      scale_y_sqrt(expression("Standardised Signal [m"*g^-1*"]"),
                   breaks=c(0,100,1000,2500,5000,10000,20000),
                   sec.axis=sec_axis(name = "Oven temperature [°C]",trans="identity",breaks=(c(0,100,400,600,900)^2/50),labels=c(0,100,400,600,900))) +
      theme_pubr()+
      theme(axis.title.y.left = element_blank(),
            axis.text.y.left = element_blank(),
            axis.ticks.y.left = element_blank(),
            axis.title.y.right = element_text(angle=90),  # add margin if needed
            legend.position = "none")+
      coord_cartesian(ylim = c(0,20000))+
      scale_alpha_manual(breaks=c("DIN19539","DIN19539GS"),values=c(0,.1))+
      ggtitle("DIN19539GS")
  }->p_GS
  
  
  
  ggarrange(p_oGS,p_GS,ncol = 2)->p_thermo
  
  
  
  #plot
  ggsave(plot=p_thermo+theme(legend.position = "none"),filename="thermograms.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =9,height = 5)
  #
  
  }


### LQ LD using signal ####

soliTOCdata%>%
  filter(Methode=="DIN19539"&Name%>%str_starts("LA"))%>%
  mutate(
    LQ_TOC400_flag=if_else(`TOC400  Fläche`<Blank_LD_LQ_area$LQ[1],1,0),
    LQ_ROC_flag=if_else(`ROC  Fläche`<Blank_LD_LQ_area$LQ[2],1,0),
    LQ_TIC900_flag=if_else(`TIC900  Fläche`<Blank_LD_LQ_area$LQ[3],1,0),
    
    LD_TOC400_flag=if_else(`TOC400  Fläche`<Blank_LD_LQ_area$LD[1],1,0),
    LD_ROC_flag=if_else(`ROC  Fläche`<Blank_LD_LQ_area$LD[2],1,0),
    LD_TIC900_flag=if_else(`TIC900  Fläche`<Blank_LD_LQ_area$LD[3],1,0)
  )

### Repeatability BDFstd ####

BDF_std%>%summarise_metrics(variables = c("TOC400","ROC","TIC900","TOC","TC"))

BDF_std%>%pivot_longer(c(TOC400,ROC,TIC900,TOC,TC))%>%group_by(name)%>%
  summarise(AVG=mean(value),
            sd=sd(value),
            n=length(value),
            stderr=sd(value)/sqrt(length(value))
  )%>%
  mutate(low=AVG-2*stderr,
         high=AVG+2*stderr,
         Rel=stderr/AVG*100)%>%
  mutate(across(where(is.double),~signif(.,2)))%>%
  arrange(c(2,5,3,4,1))%>%
  write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/BDFstd_repeatability.csv")

### DIN std ####



DINref_new%>%pivot_longer(c(TOC400,ROC,TIC900,TOC,TC))%>%group_by(name)%>%
  summarise(AVG=mean(value),
            sd=sd(value),
            n=length(value),
            stderr=sd(value)/sqrt(length(value))
  )%>%
  mutate(low=AVG-2*stderr,
         high=AVG+2*stderr,
         Rel=stderr/AVG*100)%>%
  mutate(across(where(is.double),~signif(.,2)))%>%
  arrange(c(2,5,3,4,1))%>%
  write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/DINref.csv")




## ROC information ####
#BDF_SSL%>%filter(!site_id%in%c("BDF13")&!str_detect(Campaign,"field"))->all_archive_exclBDF13

BDF_SSL%>%filter(!str_detect(Campaign,"field"))%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)->all_archive


### ROC TOC 2023 archive only ####  

BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  ggplot(aes(x=`TOC [wt-%]`,y=`ROC [wt-%]`))+
  geom_abline(slope = .1)+
  geom_label(aes(x=5.5,y=.55,label="10 %"))+
  geom_point(aes(fill=`Land use`),shape=21)+
  geom_smooth(method="glm",aes(col=`Land use`),se=F,linetype="dashed")+
  theme_pubr()+
  scale_fill_manual(breaks = c("A","G"),
                    labels=c("Cropland (n=256)","Grassland (n=44)"),
                    values = colorblind_safe_colors()[c(7,4)])+
  scale_color_manual(breaks = c("A","G"),
                     labels=c("Cropland (n=256)","Grassland (n=44)"),
                     values = colorblind_safe_colors()[c(7,4)])->p_ROC_TOC


p_ROC_TOC
#plot
ggsave(plot=p_ROC_TOC+theme(legend.position = "none"),filename="ROC_TOC_scatter.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
#legend
ggsave(plot=get_legend(p_ROC_TOC),filename="ROC_TOC_scatter_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)



### ROC TOC 2023 all ####  

label_LU=all_archive%>%
  group_by(`Land use`)%>%summarise(n=length(na.omit(`TC [wt-%]`)))%>%
mutate(label=paste0(case_match(`Land use`,"A"~"Cropland","G"~"Grassland")," (n=",n,")"))
  
  all_archive%>%
  ggplot(aes(x=`TOC [wt-%]`,y=`ROC [wt-%]`))+
  geom_abline(slope = .1)+
  geom_label(aes(x=5.5,y=.55,label="10 %"))+
  geom_point(aes(fill=`Land use`),shape=21)+
  geom_smooth(method="glm",aes(col=`Land use`),se=F,linetype="dashed")+
  theme_pubr()+
  #geom_point(data=BDF_SSL%>%filter(site_id=="BDF13"),aes(col=`Soil horizon`),col="black",shape=4)+
  scale_fill_manual(breaks = label_LU$`Land use`,
                    labels=label_LU$label,
                    values = colorblind_safe_colors()[c(7,4)])+
  scale_color_manual(breaks = label_LU$`Land use`,
                     labels=label_LU$label,
                     values = colorblind_safe_colors()[c(7,4)])->p_ROC_TOC_all

p_ROC_TOC_al#l+coord_cartesian(xlim = c(0,2),ylim = c(0,.2))
#plot
ggsave(plot=p_ROC_TOC_all+theme(legend.position = "none"),filename="ROC_TOC_scatter_all.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
#legend
ggsave(plot=get_legend(p_ROC_TOC_all),filename="ROC_TOC_scatter_all_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

# stats
#cor all
cor(all_archive$`ROC [wt-%]`,all_archive$`TOC [wt-%]`,
    method="k",use="pairwise.complete.obs")
# cor A
cor(all_archive%>%filter(`Land use`=="A")%>%pull(`ROC [wt-%]`),
    all_archive%>%filter(`Land use`=="A")%>%pull(`TOC [wt-%]`),
    method="k",use="pairwise.complete.obs")
# cor G
cor(all_archive%>%filter(`Land use`=="G")%>%pull(`ROC [wt-%]`),
    all_archive%>%filter(`Land use`=="G")%>%pull(`TOC [wt-%]`),
    method="k",use="pairwise.complete.obs")

# summary all
all_archive%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
  summarise_metrics(variables = c("TOC400 [wt-%]",
                                  "ROC [wt-%]",
                                  "TIC900 [wt-%]",
                                  "TOC [wt-%]",
                                  "TC [wt-%]",
                                  "ROCratio"))
# summary grouped

all_archive%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
  summarise_metrics(grouping_variables = "Land use",
                    variables = c("TOC400 [wt-%]",
                                  "ROC [wt-%]",
                                  "TIC900 [wt-%]",
                                  "TOC [wt-%]",
                                  "TC [wt-%]",
                                  "ROCratio"))


if(F){  all_archive%>%
    ggplot(aes(x=`ROC [wt-%]`,y=`ROC [wt-%]`/`TOC [wt-%]`))+
    #geom_abline(slope = .1)+
    #geom_label(aes(x=5.5,y=.55,label="10 %"))+
    geom_point(aes(fill=`Land use`),shape=21)+
    geom_smooth(method="glm",aes(col=`Land use`),se=F,linetype="dashed")+
    theme_pubr()+
    #geom_point(data=BDF_SSL%>%filter(site_id=="BDF13"),aes(col=`Soil horizon`),col="black",shape=4)+
    scale_fill_manual(breaks = label_LU$`Land use`,
                      labels=label_LU$label,
                      values = colorblind_safe_colors()[c(7,4)])+
    scale_color_manual(breaks = label_LU$`Land use`,
                       labels=label_LU$label,
                       values = colorblind_safe_colors()[c(7,4)])}

### Fractions Landuse  ####
#### archive only ####
BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%group_by(`Land use`)%>%summarise(n=length(na.omit(`TC [wt-%]`))) #a251, g43 (excl. NA)
landuse_list=list()
for (fr in c("TOC400","ROC","TIC900","TOC","TC","ROCratio")){
  # BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  #   mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`)%>%
  #   group_by(`Land use`)%>%
  #   filter(
  #     .data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]] >= 
  #       quantile(.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]], 0.05,na.rm=T) &
  #     
  #     .data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]] <= 
  #       quantile(.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]], 0.95,na.rm=T)
  #   )->trimmed_data
  #   # meh does not work as intended (wanted to smooth out the violin for outliers instead of the bulbs)
  landuse_list[[fr]]<-plot(
    
    BDF_SSL%>%filter(!site_id%in%c("BDF12","BDF13")&!str_detect(Campaign,"field"))%>%
      #BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
      mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
      ggplot(aes(x=`Land use`,
                 y=.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]]
                 ,fill=`Land use`))+
      geom_violin(alpha=.2,scale="width")+
      geom_boxplot(width=.2)+
      stat_summary(geom="point",shape=4,size=2,stroke=1.5,fun=mean)+
      theme_pubr()+
      ylab(if_else(fr=="ROCratio",paste(fr,"[%]"),paste(fr,"[wt-%]")))+
      scale_x_discrete("",
                       breaks = c("A","G"),
                       labels=c("Cropland (n=251)","Grassland (n=43)"))+
      scale_fill_manual(breaks = c("A","G"),
                        labels=c("Cropland (n=251)","Grassland (n=43)"),
                        values = colorblind_safe_colors()[c(7,4)])+
      ggtitle(label = if_else(fr=="ROCratio",paste(fr,"[%]"),paste(fr,"[wt-%]"))))
  
  landuse_list[[fr]]
  #plot
  ggsave(plot=landuse_list[[fr]]+theme(legend.position = "none"),filename=paste0(fr,"_landuse.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  # legend
  ggsave(plot=get_legend(landuse_list[[fr]]),filename=paste0(fr,"_landuse_legend.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  
}

ggsave(
  plot=ggarrange(landuse_list[["TOC400"]],
                 landuse_list[["TIC900"]],
                 landuse_list[["ROC"]],
                 landuse_list[["ROCratio"]],legend =  "none"),
  filename=paste0("multiplot","_landuse.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8.5)

# stats
BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
  summarise_metrics(grouping_variables = "Land use",
                    variables = c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"))%>%
  view("landuse fractions")


BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)->tmp

stat_landuse_fr=ggstatsplot::grouped_ggbetweenstats(data = tmp%>%pivot_longer(c(`TOC400 [wt-%]`,`ROC [wt-%]`,`TIC900 [wt-%]`,`TOC [wt-%]`,`TC [wt-%]`,`ROCratio`)),
                                                    grouping.var = name,
                                                    x=`Land use`,
                                                    y=value,
                                                    type="np",
                                                    paiwise.display="all",
                                                    ggplot.component=scale_color_manual(breaks = c("A","G"),
                                                                                        labels=c("Cropland (n=256)","Grassland (n=44)"),
                                                                                        values = colorblind_safe_colors()[c(7,4)])
)
stat_landuse_fr
ggsave(plot=stat_landuse_fr,filename="stat_landuse_fr.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 20,height = 10)



# additional eval
ggstatsplot::grouped_ggbetweenstats(data = tmp%>%mutate(TICROC=`TIC900 [wt-%]`/`ROC [wt-%]`,
                                                        depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
                                                        depth_class2=if_else(Depth_bottom<.35,"t","b"),
                                                        depth_class3=if_else(Depth_bottom<=.35,"t",
                                                                             if_else(Depth_top>=.3,"b","u")),
                                                        TOC_class=if_else(`TOC [wt-%]`<=2,"l","h")),
                                    grouping.var = depth_class,  #select grouping here
                                    x=`Land use`,
                                    y=`TIC900 [wt-%]`,#ROC,#ROCratio,
                                    type="np",
                                    paiwise.display="all",
                                    ggplot.component=list(scale_color_manual(breaks = c("A","G"),
                                                                             labels=c("Cropland (n=256)","Grassland (n=44)"), 
                                                                             values = colorblind_safe_colors()[c(7,4)])
                                                          ,coord_cartesian(ylim=c(.0,.8))
                                    )
)

#avg
tmp%>%mutate(depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
             depth_class2=if_else(Depth_bottom<.35,"t","b"),
             depth_class3=if_else(Depth_bottom<=.35,"t",
                                  if_else(Depth_top>=.3,"b","u")),
             TOC_class=if_else(`TOC [wt-%]`<=2,"l","h"))%>%
  group_by(depth_class,`Land use`)%>%
  summarise(across(c(`TOC400 [wt-%]`,`ROC [wt-%]`,`TIC900 [wt-%]`,`TOC [wt-%]`,`TC [wt-%]`,`ROCratio`),~mean(.,na.rm=T)))%>%
  view("LU_depth")

#### using all archive samples ####
{
 # !!! n is still manual here
  all_archive%>%group_by(`Land use`)%>%summarise(n=length((na.omit(`TC [wt-%]`)))) #a612, g289 ecsl. NA
  landuse_list_all=list()
  for (fr in c("TOC400","ROC","TIC900","TOC","TC","ROCratio")){
    # BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
    #   mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`)%>%
    #   group_by(`Land use`)%>%
    #   filter(
    #     .data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]] >= 
    #       quantile(.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]], 0.05,na.rm=T) &
    #     
    #     .data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]] <= 
    #       quantile(.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]], 0.95,na.rm=T)
    #   )->trimmed_data
    #   # meh does not work as intended (wanted to smooth out the violin for outliers instead of the bulbs)
    landuse_list_all[[fr]]<-plot(all_archive%>%
                                   mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
                                   ggplot(aes(x=`Land use`,
                                              y=.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]],fill=`Land use`))+
                                   geom_violin(alpha=.2,scale="width")+
                                   geom_boxplot(width=.2)+
                                   stat_summary(geom="point",shape=4,size=2,stroke=1.5,fun=mean)+
                                   theme_pubr()+
                                   ylab(if_else(fr=="ROCratio",paste(fr,"[%]"),paste(fr,"[wt-%]")))+
                                   scale_x_discrete("",
                                                    breaks = c("A","G"),
                                                    labels=c("Cropland (n=612)","Grassland (n=289)"))+
                                   scale_fill_manual(breaks = c("A","G"),
                                                     labels=c("Cropland (n=612)","Grassland (n=289)"),
                                                     values = colorblind_safe_colors()[c(7,4)]))
    
    landuse_list_all[[fr]]
    #plot
     ggsave(plot=landuse_list_all[[fr]]+theme(legend.position = "none"),filename=paste0(fr,"_landuse_all.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
    # legend
     ggsave(plot=get_legend(landuse_list_all[[fr]]),filename=paste0(fr,"_landuse_all_legend.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
    # 
  }
  
  ggsave(
    plot=ggarrange(landuse_list_all[["TOC400"]],
                   landuse_list_all[["TIC900"]],
                   landuse_list_all[["ROC"]],
                   landuse_list_all[["ROCratio"]],legend =  "none"),
    filename=paste0("multiplot","all_landuse.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8.5)
  }
  

##### stats all landuse ####
all_landuse_fr_stat=ggstatsplot::grouped_ggbetweenstats(data = all_archive%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
  pivot_longer(c(`TOC400 [wt-%]`,`ROC [wt-%]`,`TIC900 [wt-%]`,`TOC [wt-%]`,`TC [wt-%]`,`ROCratio`)),
                                    grouping.var = name,
                                    x=`Land use`,
                                    y=value,
                                    type="np",
                                    paiwise.display="all",
                                    ggplot.component=scale_color_manual(breaks = c("A","G"),
                                                                        labels=c("Cropland (n=612)","Grassland (n=289)"),
                                                                        values = colorblind_safe_colors()[c(7,4)])
)


all_landuse_fr_stat

ggsave(
  plot=all_landuse_fr_stat,
  filename="all_landuse_stat.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 20,height = 10)


all_archive%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100)%>%
  summarise_metrics(grouping_variables = "Land use",
                                variables = c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"))%>%
  view("allLU")





all_archive%>%mutate(depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
                     depth_class2=if_else(Depth_bottom<.35,"t","b"),
                     depth_class3=if_else(Depth_bottom<=.35,"t",
                                          if_else(Depth_top>=.3,"b","u")),
                     TOC_class=if_else(`TOC [wt-%]`<=2,"l","h"))%>%
  group_by(depth_class,`Land use`)%>%
  summarise(n=length(na.omit(`TC [wt-%]`)))


all_archive%>%mutate(depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
          depth_class2=if_else(Depth_bottom<.35,"t","b"),
          depth_class3=if_else(Depth_bottom<=.35,"t",
                               if_else(Depth_top>=.3,"b","u")),
          TOC_class=if_else(`TOC [wt-%]`<=2,"l","h"))%>%
  group_by(depth_class,`Land use`)%>%
  summarise(across(c(`TOC400 [wt-%]`,`ROC [wt-%]`,`TIC900 [wt-%]`,`TOC [wt-%]`,`TC [wt-%]`,`ROCratio`),~mean(.,na.rm=T)))%>%
  view("LU_depth")
ggstatsplot::grouped_ggbetweenstats(
all_archive%>%mutate(depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
                     depth_class2=if_else(Depth_bottom<.35,"t","b"),
                     depth_class3=if_else(Depth_bottom<=.35,"t",
                                          if_else(Depth_top>=.3,"b","u")),
                     TOC_class=if_else(`TOC [wt-%]`<=2,"l","h")),x=`Land use`,
y=ROCratio,
grouping.var = depth_class,
type="np"
)



ggstatsplot::grouped_ggbetweenstats(
  all_archive%>%#filter(site_id!="BDF13")%>%
    mutate(depth_class=if_else((Depth_top+Depth_bottom)/2<.3,"t","b"),
                       depth_class2=if_else(Depth_bottom<.35,"t","b"),
                       depth_class3=if_else(Depth_bottom<=.35,"t",
                                            if_else(Depth_top>=.3,"b","u")),
                       TOC_class=if_else(`TOC [wt-%]`<=2,"l","h")),x=`Land use`,
  y=ROCratio,
  grouping.var = depth_class,
  type="np",
)
### Fractions Depth&Horizon ####


# include all major horizon classifications, i.e, Al-Bv -> A and B
# tmp%>%mutate(
#   A=str_detect(`Soil horizon`,"A"),
#   B=str_detect(`Soil horizon`,"B"),
#   C=str_detect(`Soil horizon`,"C"),
#   G=str_detect(`Soil horizon`,"G"),
#   M=str_detect(`Soil horizon`,"M"),
#   S=str_detect(`Soil horizon`,"S"),
# )%>%pivot_longer(c(A,B,C,G,M,S),names_to = "hz",values_to = "is_hz")%>%
#   filter(is_hz)%>%
#   ggstatsplot::ggbetweenstats(x=hz,y=`ROC [wt-%]`,type="np")
# 

# use pronounced (last) soil horizon ony

# NOTE: using all data to begin with
all_archive%>%mutate(hz=`Soil horizon`%>%
               str_remove_all("[:lower:]")%>%
               str_remove_all("[:symbol:]")%>%
               str_remove_all("[:punct:]")%>%
               str_pad(5,"left")%>%
               substr(5,5)
)->tmp_hz


tmp_hz%>%ggstatsplot::ggbetweenstats(x=hz,y=`ROCratio`,type="np")%>%ggsave(filename="all_landuse_horizon_stat.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 20,height = 10)



label=tmp_hz%>%filter(!is.na(hz)&!is.na(`TOC400 [wt-%]`))%>%group_by(hz)%>%summarise(n=length(`TC [wt-%]`))%>%mutate(label=paste0(hz," (n=",n,")"))
horizon_list=c()
for (fr in c("TOC400","ROC","TIC900","TOC","TC","ROCratio")){
  horizon_list[[fr]]<-plot(tmp_hz%>%filter(!is.na(hz))%>%
                             ggplot(aes(x=hz,
                                        y=.data[[if_else(fr=="ROCratio",fr,paste(fr,"[wt-%]"))]]
                                        ,fill=hz))+
                             geom_violin(alpha=.2,scale="width")+
                             geom_boxplot(width=.2)+
                             stat_summary(geom="point",shape=4,size=2,stroke=1.5,fun=mean)+
                             theme_pubr()+
                             ylab(if_else(fr=="ROCratio",paste(fr,"[%]"),paste(fr,"[wt-%]")))+
                             scale_x_discrete("",
                                              breaks = label$hz,
                                              labels=label$label)+
                             scale_fill_manual("Horizon",breaks = label$hz,
                                               labels=label$label,
                                               values = colorblind_safe_colors()[c(2:4,6:8)]))+
    ggtitle(label = if_else(fr=="ROCratio",paste(fr,"[%]"),paste(fr,"[wt-%]")))
  
  horizon_list[[fr]]
  #plot
  ggsave(plot=horizon_list[[fr]]+theme(legend.position = "none"),filename=paste0(fr,"_horizon.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # legend
  ggsave(plot=get_legend(horizon_list[[fr]]),filename=paste0(fr,"_landuse_horizon.png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  
}


tmp_hz%>%summarise_metrics(grouping_variables = "hz",variables = c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"))%>%
  view("allHZ")


## Vertical profiles #####

### archive samples ####

#### yearwise for each site ####
# select sites with best data coverage
site_list=list()
#year=2006
for (site in unique(all_archive$site_id)){
  
  print(site)
cm_aggregate(all_archive%>%filter(site_id==site)%>%mutate(year=year(DateEvent)),
             depth_top_col = "Depth_top",
             depth_bottom_col = "Depth_bottom",
             aggregate_list =  c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"),
             group_list = c("year"),
             add_funs = list(n=~length(na.omit(.))),
             res_out = .05)->tmp_site

print(nrow(tmp_site))

incompl_profile=tmp_site%>%filter(!is.na(`TC [wt-%]_mean`))%>%
  group_by(year)%>%
  summarise(maxdepth=max(u3),
            ndepth=length(unique(u3)),
            x=maxdepth/ndepth)%>%
  filter(x!=.05)%>%pull(year)

sel_year=
  tmp_site%>%group_by(year)%>%
  summarise(min_n=min(`TC [wt-%]_n`),max_depth=max(u3),min_depth=min(o3))%>%
  filter(max_depth>.5&min_depth==0& #profil min 50 cm from top
           !year%in%incompl_profile)%>%
  pull(year)%>%na.omit()


print(sel_year)
# overview
if(length(sel_year)>0){
  site_list[[site]]<-plot(
    tmp_site%>%
      filter(year%in%c(sel_year))%>%
  mutate(`TOC400 [wt-%]_mean`=`TOC400 [wt-%]_mean`/10)%>%
  pivot_longer(cols = c(`TOC400 [wt-%]_mean`,`ROC [wt-%]_mean`,`TIC900 [wt-%]_mean`))%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_ribbon(aes(col=name,ymax=value,ymin=0,fill=name),
              outline.type="upper",
              alpha=.1)+
  coord_flip()+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROC, TIC900 [wt-%]",sec.axis = sec_axis("TOC400 [wt-%]",transform = ~.*10))+
  scale_color_manual("Fraction",
                     breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                     labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                     values=colorblind_safe_colors()[c(4,3,2)])+
  scale_fill_manual("Fraction",
                    breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                    labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                    values=colorblind_safe_colors()[c(4,3,2)])+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  facet_wrap(~year,scales = "free_x")+
    ggtitle(site))
}
}
saveRDS(site_list,"C:/Users/adam/Desktop/UNI/PhD/DISS/plots/site_year_fractionProfiles")
# select sites with best data coverage

#### yearwise interpol. #####
year_list=list()
#year=2006
for (year in unique(year(na.omit(all_archive$DateEvent)))){
  
  print(year)
  cm_aggregate(all_archive%>%filter(year(DateEvent)==year),
               depth_top_col = "Depth_top",
               depth_bottom_col = "Depth_bottom",
               aggregate_list =  c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"),
               group_list = c("site_id","Land use"),
               add_funs = list(n=~length(na.omit(.))),
               res_out = .05)->tmp_year
  
  print(nrow(tmp_year))
  
  incompl_profile=tmp_year%>%filter(!is.na(`TC [wt-%]_mean`))%>%
    group_by(site_id)%>%
    summarise(maxdepth=max(u3),
              ndepth=length(unique(u3)),
              x=maxdepth/ndepth)%>%
    filter(x!=.05)%>%pull(site_id)
  
  sel_sites=
    tmp_year%>%group_by(site_id)%>%
    summarise(min_n=min(`TC [wt-%]_n`),max_depth=max(u3),min_depth=min(o3))%>%
    filter(max_depth>.5&min_depth==0& #profil min 50 cm from top
             !site_id%in%c("BDF02","BDF23","BDF30","BDF35", #combined with field data
                           incompl_profile) # depth increments missing
    )%>%
    pull(site_id)
  
  
  print(sel_sites)
  # overview
  if(length(sel_sites)>0){
    year_list[[as.character(year)]]<-plot(
      tmp_year%>%
        
        filter(site_id%in%c(sel_sites))%>%
        mutate(`TOC400 [wt-%]_mean`=`TOC400 [wt-%]_mean`/10)%>%
        pivot_longer(cols = c(`TOC400 [wt-%]_mean`,`ROC [wt-%]_mean`,`TIC900 [wt-%]_mean`))%>%
        ggplot(aes(x=(o3+u3)/2,col=site_id))+
        geom_ribbon(aes(col=name,ymax=value,ymin=0,fill=name),
                    outline.type="upper",
                    alpha=.1)+
        coord_flip()+
        scale_x_reverse("Depth [m]")+
        scale_y_continuous("ROC, TIC900 [wt-%]",sec.axis = sec_axis("TOC400 [wt-%]",transform = ~.*10))+
        scale_color_manual("Fraction",
                           breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                           labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                           values=colorblind_safe_colors()[c(4,3,2)])+
        scale_fill_manual("Fraction",
                          breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                          labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                          values=colorblind_safe_colors()[c(4,3,2)])+
        theme_pubclean()+
        theme(legend.position = "right")+
        geom_vline(xintercept = 0)+
        facet_wrap(~paste0(site_id," (",`Land use`,")"),scales = "free_x")+
        ggtitle(year))
  }
}

saveRDS(year_list,"C:/Users/adam/Desktop/UNI/PhD/DISS/plots/year_site_fractionProfiles")




# col_landuse=c("G"=colorblind_safe_colors()[4],"A"=colorblind_safe_colors()[7])
# filter(tmp,site_id%in%paste0("BDF",c("04","07","13","18","24","33","44")))%>%
#   mutate(label=paste0(site_id," (",`Land use`,")"))%>%pull(label)%>%unique->label_list
# 
# fill_list=col_landuse[substr(label_list,8,8)]
# names(fill_list)=label_list

cm_aggregate(all_archive,
             depth_top_col = "Depth_top",
             depth_bottom_col = "Depth_bottom",
             aggregate_list =  c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"),
             group_list = c("site_id","Land use"),
             add_funs = list(mn=~median(.,na.rm=T),
                             q25=~quantile(.,.25,na.rm=T),
                             q75=~quantile(.,.75,na.rm=T),
                             n=~length(na.omit(.))),
             res_out = .05)->tmp





incompl_profile=tmp%>%filter(!is.na(`TC [wt-%]_mean`))%>%
  group_by(site_id)%>%
  summarise(maxdepth=max(u3),
            ndepth=length(unique(u3)),
            x=maxdepth/ndepth)%>%
  filter(x!=.05)%>%pull(site_id)

sel_sites=
  tmp%>%group_by(site_id)%>%
  summarise(min_n=min(`TC [wt-%]_n`),max_depth=max(u3),min_depth=min(o3))%>%
  filter(max_depth>.5&min_depth==0& #profil min 50 cm from top
           
           !site_id%in%c("BDF02","BDF23","BDF30","BDF35", #combined with field data
                         incompl_profile) # depth increments missing
  )#%>%
  pull(site_id)



tmp%>%
  filter(site_id%in%c(sel_sites%>%filter(min_n>3&max_depth>.6)%>%
                        pull(site_id),
                      "BDF33"))%>%
  #filter(site_id%in%paste0("BDF",c("04","07","13","18","24","33","44")))%>%
  mutate(`TOC400 [wt-%]_mean`=`TOC400 [wt-%]_mean`/10)%>%
  pivot_longer(cols = c(`TOC400 [wt-%]_mean`,`ROC [wt-%]_mean`,`TIC900 [wt-%]_mean`))%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_ribbon(aes(col=name,ymax=value,ymin=0,fill=name),
              outline.type="upper",
              ,alpha=.1
              )+
  coord_flip()+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROC, TIC900 [wt-%]",sec.axis = sec_axis("TOC400 [wt-%]",transform = ~.*10))+
  scale_color_manual("Fraction",
                     breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                     labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                     values=colorblind_safe_colors()[c(4,3,2)])+
  scale_fill_manual("Fraction",
                    breaks=c("TOC400 [wt-%]_mean","ROC [wt-%]_mean","TIC900 [wt-%]_mean"),
                    labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]"),
                    values=colorblind_safe_colors()[c(4,3,2)])+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  facet_wrap(~paste0(site_id," (",`Land use`,")"),scales = "free_x")->archive_profiles
  
archive_profiles

#plot
ggsave(plot=archive_profiles+theme(legend.position = "none"),filename="archive_profiles5cm.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 8)
# legend
ggsave(plot=get_legend(archive_profiles),filename="archive_profiles5cm_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)


#### Average Landuse profiles ####


cm_aggregate(all_archive,
             depth_top_col = "Depth_top",
             depth_bottom_col = "Depth_bottom",
             aggregate_list =  c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","TOC [wt-%]","TC [wt-%]","ROCratio"),
             group_list = c("Land use"),
             add_funs = list(mn=~median(.,na.rm=T),
                             q25=~quantile(.,.25,na.rm=T),
                             q75=~quantile(.,.75,na.rm=T),
                             n=~length(na.omit(.))),
             res_out = .05)->tmp_LU_general

tmp_LU_general%>%
  pivot_longer(cols = c(`TOC400 [wt-%]_mean`,`ROC [wt-%]_mean`,`TIC900 [wt-%]_mean`,
                        `TOC400 [wt-%]_q25`,`ROC [wt-%]_q25`,`TIC900 [wt-%]_q25`,
                        `TOC400 [wt-%]_q75`,`ROC [wt-%]_q75`,`TIC900 [wt-%]_q75`,
                        `TOC400 [wt-%]_mn`,`ROC [wt-%]_mn`,`TIC900 [wt-%]_mn`,
                        `TIC900 [wt-%]_n`))%>%
  mutate(metric=str_split_fixed(name,"_",2)[,2],name=str_split_fixed(name,"_",2)[,1])%>%
  mutate(value=if_else(name=="TOC400 [wt-%]",value/10,value),
         `Land use`=case_match(`Land use`,"A"~"Cropland","G"~"Grassland"))%>%
  pivot_wider(values_from = value,names_from=metric)%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_line(aes(col=name,y=mean))+
  geom_ribbon(aes(ymin=q25,max=q75,fill=name),alpha=.1)+
  geom_ribbon(aes(ymin=0,ymax=n/-20000,group=name,,col="n",fill="n"))+
  coord_flip(ylim = c(-.05,.5),xlim=c(.6,.0))+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROC, TIC900 [wt-%]",sec.axis = sec_axis("TOC400 [wt-%]",transform = ~.*10))+
  scale_color_manual("Fraction",
                     breaks=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","n"),
                     labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","Data availbility"),
                     values=c(colorblind_safe_colors()[c(4,3,2)],"gray"))+
  scale_fill_manual("Fraction",
                    breaks=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","n"),
                    labels=c("TOC400 [wt-%]","ROC [wt-%]","TIC900 [wt-%]","Data availbility"),
                    values=c(colorblind_safe_colors()[c(4,3,2)],"lightgray"))+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  facet_wrap(~`Land use`,scales = "free_x")->LU_profile_all



#plot
ggsave(plot=LU_profile_all#+theme(legend.position = "none")
       ,filename="LU_profile_all5cm.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
# legend
ggsave(plot=get_legend(LU_profile_all),filename="LU_profile_all5cm_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)



### ROCratios sites ####
### sites ####
tmp%>%

  filter(site_id%in%c(sel_sites%>%filter(min_n>3&max_depth>.6)%>%
                        pull(site_id),
                      "BDF33"))%>%
  #filter(site_id%in%paste0("BDF",c("04","07","13","18","24","33","44")))%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_line(aes(col=site_id,y=ROCratio_mean,group =paste(`Land use`,site_id)))+
  geom_ribbon(aes(fill=site_id,
                  ymin=ROCratio_q25,
                  ymax=ROCratio_q75,
                  group = paste(`Land use`,site_id)),
              alpha=.1)+
  geom_ribbon(aes(ymax=ROCratio_n/-50,ymin=0,col=site_id),fill="lightgray",alpha=.2)+
  coord_flip(xlim=c(.6,0),ylim=c(-5,25))+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROCratio [%]")+
  scale_color_manual("BDF site",values = colorblind_safe_colors())+
  scale_fill_manual("BDF site",values = colorblind_safe_colors())+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)

#### landuse ####
tmp_LU_general%>%
  pivot_longer(cols = c(,`ROCratio_mean`,
                        ,`ROCratio_q25`,
                        ,`ROCratio_q75`,
                        ,`ROCratio_mn`,
                        `ROCratio_n`
                        ))%>%
  mutate(metric=str_split_fixed(name,"_",2)[,2],name=str_split_fixed(name,"_",2)[,1])%>%
  #mutate(value=if_else(name=="TOC400 [wt-%]",value/10,value))%>%
  pivot_wider(values_from = value,names_from=metric)%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_line(aes(col=`Land use`,y=mean))+
  geom_ribbon(aes(ymin=q25,max=q75,fill=`Land use`),alpha=.1)+
  geom_ribbon(aes(ymin=7,ymax=n/-500+7,col=`Land use`,fill="n"),alpha=.1)+
  coord_flip(ylim = c(3,15),xlim=c(.6,.0))+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROCratio [%]",breaks = c(8,10,12,14),minor_breaks = c(9,11,13,15))+
  scale_color_manual("Fraction",
                     breaks=c("A","G","n"),
                     labels=c("Cropland","Grassland","Data availbility"),
                     values=c(colorblind_safe_colors()[c(7,4)],"gray"))+
  scale_fill_manual("Fraction",
                    breaks=c("A","G","n"),
                    labels=c("Cropland","Grassland","Data availbility"),
                    values=c(colorblind_safe_colors()[c(7,4)],"gray"))+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 7)->LU_profile_ROCratio_all
LU_profile_ROCratio_all
#plot
ggsave(plot=LU_profile_ROCratio_all#+theme(legend.position = "none")
       ,filename="LU_profile_ROCratio_all5cm.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
#legend
ggsave(plot=get_legend(LU_profile_ROCratio_all),filename="LU_profile__ROCratioall5cm_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

#### for combined plots ####
#### landuse ####
tmp_LU_general%>%
  pivot_longer(cols = c(,`ROCratio_mean`,
                        ,`ROCratio_q25`,
                        ,`ROCratio_q75`,
                        ,`ROCratio_mn`,
                        `ROCratio_n`
  ))%>%
  mutate(metric=str_split_fixed(name,"_",2)[,2],name=str_split_fixed(name,"_",2)[,1])%>%
  #mutate(value=if_else(name=="TOC400 [wt-%]",value/10,value))%>%
  pivot_wider(values_from = value,names_from=metric)%>%
  mutate(dummy=`Land use`,`Land use`="ROCratio")%>%
  ggplot(aes(x=(o3+u3)/2))+
  geom_line(aes(col=dummy,y=mean))+
  geom_ribbon(aes(ymin=q25,max=q75,fill=dummy),alpha=.1)+
  #geom_ribbon(aes(ymin=7,ymax=n/-500+7,col=dummy,fill="n"),alpha=.1)+
  coord_flip(ylim = c(7,15),xlim=c(.6,.0))+
  scale_x_reverse("Depth [m]")+
  scale_y_continuous("ROCratio [%]",breaks = c(8,10,12,14),minor_breaks = c(9,11,13,15),
                     # dummy 2nd axis
                     sec.axis = sec_axis(name="  ",transform = ~.))+
  scale_color_manual("Fraction",
                     breaks=c("A","G","n"),
                     labels=c("Cropland","Grassland","Data availbility"),
                     values=c(colorblind_safe_colors()[c(7,4)],"gray"))+
  scale_fill_manual("Fraction",
                    breaks=c("A","G","n"),
                    labels=c("Cropland","Grassland","Data availbility"),
                    values=c(colorblind_safe_colors()[c(7,4)],"gray"))+
  theme_pubclean()+
  theme(legend.position = "right")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 7)+
  facet_wrap(~`Land use`)+
  #make top axis invisible
  theme(axis.text.x.top = element_text(color=rgb(1,1,1,1)),
        axis.ticks.x.top = element_line(color=rgb(1,1,1,1)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())->LU_profile_ROCratio_all2

LU_profile_ROCratio_all2


#plot
ggsave(plot=ggarrange(LU_profile_all,LU_profile_ROCratio_all2,legend = "none",widths = c(2,1))
       ,filename="LU_profile_final_5cm.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
#legend
ggsave(plot=get_legend(LU_profile_ROCratio_all2),filename="LU_profile_final_5cm_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)



# DRIFTS ####



### general prep, excl. field ####

  
  
## BDF-SSL EDA ####

### preprocessing spectra plots ####
spc_processing_plts=c()

# manual, individual spc diplay
set.seed(123)
(spc_tmp[sample(1:nrow(spc_tmp),5),])%>%
  select(all_of(c("LabelEvent","spc_rs4")))%>%
  mutate(across(all_of("spc_rs4"),as_tibble))%>%
  unnest(cols=all_of("spc_rs4"))%>%
  pivot_longer(cols=colnames(spc_tmp[["spc_rs4"]]))%>%
  mutate(name=as.numeric(name))%>%
  filter(name>=425&name<=7475)%>% #crop ends
  
  ggplot(aes(x=name))+
  geom_line(aes(y=value,group=LabelEvent))+
  #geom_line(aes(y=mean))+
  scale_x_log10(expression("Wavenumber [c"*m^-1*"]"))+
  ggtitle("A1  spc_rs4")+
  theme_pubr()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())->spc_processing_plts[["individual_spc_rs4"]]

set.seed(123)
(spc_tmp[sample(1:nrow(spc_tmp),5),])%>%
  select(all_of(c("LabelEvent","spc_sg_rs4")))%>%
  mutate(across(all_of("spc_sg_rs4"),as_tibble))%>%
  unnest(cols=all_of("spc_sg_rs4"))%>%
  pivot_longer(cols=colnames(spc_tmp[["spc_sg_rs4"]]))%>%
  mutate(name=as.numeric(name))%>%
  filter(name>=425&name<=7475)%>% #crop ends
  
  ggplot(aes(x=name))+
  geom_line(aes(y=value,group=LabelEvent))+
  #geom_line(aes(y=mean))+
  scale_x_log10(expression("Wavenumber [c"*m^-1*"]"))+
  ggtitle("B1  spc_sg_rs4")+
  theme_pubr()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())->spc_processing_plts[["individual_spc_sg_rs4"]]

# loop through all subsets
#i="spc_rs"

for (i in names(spc_tmp)[-1]){
  print(i)
  
  i_title=(list("spc_rs4" = "A2  spc_rs4",
               "spc_sg_rs4" = "B2  spc_sg_rs4",
               "spc_sg_bl_rs4" = "C  spc_sg_bl_rs4",
               "spc_sg_snv_rs4" = "D  spc_sg_snv_rs4",
               "spc_sg1d_rs4" = "E  spc_sg1d_rs4",
               "spc_sg2d_rs4" = "F  spc_sg2d_rs4"))[[i]]
  spc_tmp%>%
    select(all_of(c("LabelEvent",i)))%>%
    mutate(across(all_of(i),as_tibble))%>%
    unnest(cols=all_of(i))%>%
    pivot_longer(cols=colnames(spc_tmp[[i]]))%>%
    mutate(name=as.numeric(name))%>%
    summarise_metrics(grouping_variables = "name",
                      variables = "value")->plt_data
  
  
  plt_data%>%filter(name>=425&name<=7475)%>% #crop ends
    ggplot(aes(x=name))+
    geom_ribbon(aes(ymin=min,ymax=max),alpha=.1)+
    geom_ribbon(aes(ymin=q25,ymax=q75),alpha=.2)+
    geom_line(aes(y=median))+
    #geom_line(aes(y=mean))+
    ggtitle(i_title)+
    scale_x_log10(expression("Wavenumber [c"*m^-1*"]"))+
    theme_pubr()+
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())->spc_processing_plts[[i]]
}
plt=ggarrange(plotlist = spc_processing_plts[which(names(spc_processing_plts)%>%str_detect("rs4"))],ncol=2,nrow=4)%>%
  annotate_figure(bottom=text_grob(expression("Wavenumber [c"*m^-1*"]")))

plt

ggsave(plot=plt,filename="spc_processing_vis.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 12)

### PCA ####
#### calculate / load ####
# !!! CREATED BELOW !!!!
all_data=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))

# all archive samples with spc available
all_archive=all_data%>%filter(!str_detect(Campaign,"field")&
                                !is.na(spc_sg_snv_rs4[,1])&
                                !LabelEvent=="LAAA" #bad scan
                              )


# fast ...-ish
#pc=prcomp(all_archive%>%pull(spc_sg_snv),rank. = 200)
# slow but fancier

pc=pca(all_archive%>%pull(spc_sg_snv_rs4),ncomp = 200,method = "nipals")
saveRDS(pc,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/temp/pc")#paste0(data_dir,"/Sean_Environment/R_main/model_out/pca_all-spc_sg-snv"))

# quick vis
plotScores(pc$calres,cgroup=all_archive%>%
             #pull(pH_CaCl2)
             #pull(`S [wt-%]`)%>%factor
             #pull(`ROC [wt-%]`)
             pull(`Nt [wt-%]`)
             #pull(site_id)%>%factor
             #pull(Depth_bottom)
             #pull(DateEvent)
             #pull(`Land use`)%>%factor
             #pull(`TOC [wt-%]`)
           )

#### loadings ####
pc$loadings%>%
  as_tibble()%>%
  mutate(wn=rownames(pc$loadings)%>%as.numeric())%>%
  # comp 1-6
  select(all_of(c("wn",paste("Comp",c(1:6)))))%>%
  pivot_longer(cols = paste("Comp",c(1:6)))->loadings_data
  
all_archive$spc_sg_snv_rs4%>%as_tibble%>%pivot_longer(cols=as.character(seq(7474,426,-4)))%>%
  mutate(name=as.numeric(name))%>%
  summarise_metrics(grouping_variables = "name",
                    variables = "value")%>%
  rename(wn=name)->spc_data


plot(ggplot(spc_data,aes(x=wn))+
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_ribbon(aes(ymin=min,ymax=max),alpha=.1)+
  geom_ribbon(aes(ymin=q25,ymax=q75),alpha=.1)+
  geom_line(aes(y=median),linewidth=.25)+
  geom_line(data=loadings_data,aes(x=wn,y=value*20),col="red3")+
  scale_x_log10(expression("Wavenumber [c"*m^-1*"]"),breaks=c(500,2000,5000))+
  scale_y_continuous(name = "Loadings",labels=~./20,
                     sec.axis = sec_axis(name = "Absorbance",transform = "identity"))+
theme_pubr()+
  # theme(axis.title = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank())+
facet_wrap(~name,scales="fixed"))->loadings_plt
  
loadings_plt

  ggsave(plot=loadings_plt,filename="pc_loadings.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 6)


#### RELOAD pca ####
  
all_data=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))

# all archive samples with spc available
all_archive=all_data%>%filter(!str_detect(Campaign,"field")&
                                !is.na(spc_sg_snv_rs4[,1])&
                                !LabelEvent=="LAAA" #bad scan
)

  
pc=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/pca_all-spc-sg-snv-rs4"))

pc_merge=all_archive
pc_merge=cbind(pc_merge,pc$calres$scores)


#### pca scores with ellipses ####

pca_plt_txt_scale=20

sel_sites =  c(
  "BDF02",
  "BDF12",
  "BDF13",
  "BDF23",
  "BDF30",
  "BDF33",
  "BDF35",
  "BDF46"
)
expl_var=pc$res$cal$expvar

ggplot(filter(pc_merge,
              !site_id %in% sel_sites),
       aes(x = `Comp 1`, y = `Comp 2`)) +
  geom_vline(xintercept = 0,linetype="dotted")+
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_point(size = .75, col = "grey") +
  geom_point(data = filter(
    pc_merge,
    site_id %in% sel_sites
  ), aes(col = site_id,shape=if_else(Depth_top>.25,"b","t")),stroke=1) + stat_ellipse(
    data = filter(
      pc_merge,
      site_id %in% sel_sites
    ),
    aes(fill = site_id, col = site_id),
    level = .99,
    geom = "polygon",
    alpha = .1
  ) + 
  xlab(paste0("PC1 (",(expl_var[1])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  ylab(paste0("PC2 (",(expl_var[2])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  scale_shape_manual("",breaks=c("b","t"),values=c(3,1),labels=c("Subsoil","Topsoil"))+
  ggthemes::scale_color_colorblind("Location") + 
  ggthemes::scale_fill_colorblind("Location") +
  theme_minimal()+
  theme(text = element_text(size = pca_plt_txt_scale),
        axis.text = element_text(size = pca_plt_txt_scale))->plt_pc12



ggplot(filter(pc_merge,
              !site_id %in% sel_sites),
       aes(x = `Comp 3`, y = `Comp 4`)) +
  geom_vline(xintercept = 0,linetype="dotted")+
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_point(size = .75, col = "grey") +
  geom_point(data = filter(
    pc_merge,
    site_id %in% sel_sites
  ), aes(col = site_id,shape=if_else(Depth_top>.2,"b","t")),stroke=1) + stat_ellipse(
    data = filter(
      pc_merge,
      site_id %in% sel_sites
    ),
    aes(fill = site_id, col = site_id),
    level = .99,
    geom = "polygon",
    alpha = .1
  ) + 
  xlab(paste0("PC3 (",(expl_var[3])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  ylab(paste0("PC4 (",(expl_var[4])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  scale_shape_manual("",breaks=c("b","t"),values=c(3,1),labels=c("Subsoil","Topsoil"))+
  ggthemes::scale_color_colorblind("Location") + 
  ggthemes::scale_fill_colorblind("Location") +
  theme_minimal()+
  theme(text = element_text(size = pca_plt_txt_scale),
        axis.text = element_text(size = pca_plt_txt_scale))->plt_pc34

plt_pc12
plt_pc34
ggsave(plot=plt_pc12+theme(legend.position = "none"),filename = "pca_12.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=7,width=7,device="png")

ggsave(plot=plt_pc34+theme(legend.position = "none"),filename = "pca_34.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=7,width=7,device="png")



#### correlations ####
# overview

svg("C:/Users/adam/Desktop/UNI/PhD/DISS/plots/corrplot_all.svg")
pc_merge%>%select(!matches(c(paste("Comp",c(11:200))))&!contains("spc")&where(is.numeric))%>%
  cor(method="s",use="pairwise.complete.obs")%>%
  corrplot(tl.col = "black",method="color",diag = F,addCoef.col = "black",tl.cex = .125,number.cex = .0625)->all_cor
dev.off()


svg("C:/Users/adam/Desktop/UNI/PhD/DISS/plots/corrplot_all_used.svg")
pc_merge%>%select(all_of(c(paste("Comp",c(1:10)),
                           variable_lookup%>%unlist)))%>%
  cor(method="s",use="pairwise.complete.obs")%>%
corrplot(tl.col = "black",method="color",diag = F,addCoef.col = "black",tl.cex = .5,number.cex = .2)
dev.off()
expl_var=pc$res$cal$expvar
#expl_var
#names(pc_merge)

# custom selection
corrplot_variable_lookup=variable_lookup[c("TOC400","ROC","TIC900","TOC","TC","CORG","Ct","Nt","Pt","K_t","Mg_t",
                                           "KAKpot","T","U","S","Fe_t","Al_t")]
corrplot_variable_lookup$pH_CaCl2="pH (CaCl2)"


png(filename = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/corrplot.png",width = 4000,height=4000)

{pc_merge%>%rename(`pH (CaCl2)`=pH_CaCl2)%>%select(all_of(c(paste("Comp",c(1:6)),
                           corrplot_variable_lookup%>%unlist)))%>%
  #custom variables for diss, manual order
  select(all_of(paste("Comp",c(1:6))),TOC400,ROC,TIC900,TOC,TC,CORG,Ct,Nt,Pt,K_t,Mg_t,KAKpot,pH_CaCl2,`T`,U,S,Fe_t,Al_t)%>%
  rename_with(~(unlist(corrplot_variable_lookup))[.x],.cols = c(TOC400,ROC,TIC900,TOC,TC,CORG,Ct,Nt,Pt,K_t,Mg_t,KAKpot,pH_CaCl2,`T`,U,S,Fe_t,Al_t))%>%
  rename_with(~paste0("PC",str_remove(.x,"Comp ")," (",
                      (expl_var[.x])%>%format(digits=2,nsmall = 1,scientific = F)%>%str_remove(" "),"%)"),
              .cols = paste("Comp",c(1:6)))%>%
  cor(method="s",use="pairwise.complete.obs")%>%
  corrplot(tl.col = "black",method="color",diag = F,addCoef.col = "black",tl.cex = 6,number.cex = 3)}


dev.off()


# correlating spectra derived pc with variables in BDF database + soliTOC
pc_merge%>%select(all_of(c(
  paste("Comp",c(1:10)),
  Data_table_reference%>%select(where(is.numeric))%>%names,
  Data_table_soliTOC%>%select(where(is.numeric))%>%names,
  Event_table%>%select(where(is.numeric))%>%names
)))%>%
  cor(method="s",use="pairwise.complete.obs")%>%as_tibble->all_cor
view(tibble(var=colnames(all_cor),all_cor))
  
# 
# left_join(all_prep_flag,tibble(sample_id=pc_merge$sample_id,pc=select(pc_merge,all_of(names(pc_merge)[which(str_detect(names(pc_merge),"Comp"))]))),by=c("Name"="sample_id"))->pc_merge_prep
# 
# 
# pc_merge_unnest=unnest(pc_merge_prep,pc)
# 
# pc_corTable_data=select(pc_merge_unnest,
#                         `Comp 1`,`Comp 2`,`Comp 3`,
#                         Ct,CORG,TC,TOC,TOC400,ROC,TIC900,
#                         Nt,Pt,K_t,Fe_t,Al_t,`T`,U,S)
# 
# png(paste0(code_dir,"GitLab/phd_code/R_main/temp/pca123_corrplot.png"),
#     width=1000,height=1000)
# corrplot::corrplot.mixed(cor(pc_corTable_data,use="pairwise.complete.obs",method = "spearman"),lower.col = "black")
# dev.off()



### SOM Self-organizing map ####

#### training and data prep ####
som_grid <- somgrid(xdim=10, ydim=10, topo="hexagonal")
set.seed(123)
som_model <- supersom(data = all_archive%>%
                        pull(spc_sg_snv_rs4),
                      #Y=Data_table_reference%>%as.matrix(),
                      grid = som_grid,rlen = 500,alpha=c(.05,.01))

saveRDS(som_model,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/temp/som")

som_model=readRDS("C:/Users/adam/Desktop/UNI/PhD/DISS/data/temp/som")

# extract codebooks
som_grid <- som_model$grid
codebook=som_model$codes[[1]]
pc_codebook=predict(pc,codebook)

# Compute the hex grid layout
get_hex_coords = function(x, y) {
  x_offset <- ifelse(y %% 2 == 0, 0, sqrt(3)/2)
  data.frame(
    x = x * sqrt(3) + x_offset,
    y = y * 1.5
  )
}
positions <- as.data.frame(som_grid$pts)
positions <- positions %>%
  mutate(index = row_number()) %>%
  rowwise() %>%
  mutate(
    hex_x = get_hex_coords(x, y)$x,
    hex_y = get_hex_coords(x, y)$y
  ) %>%
  ungroup()


#### overview ####
# quick & ditry grid vis


ggplot(positions, aes(x = x, y = y)) +
  geom_hex(stat = "identity", fill = rgb(1,1,1,0), color = "black",linewidth=.5) +  # for actual hex shapes
  coord_fixed(xlim=c(1,10.5),ylim=c(.7,9)) +
  geom_text(aes(label=paste(x%>%floor,(y*1.25)%>%floor,sep="-")))+
  theme_pubr()

# visualise codebook vectors (like pca loadings)
codebook_prep=tibble(positions,codebook%>%as_tibble)%>%
  pivot_longer(colnames(codebook))%>%mutate(name=as.numeric(name))

ggplot(spc_data,aes(x=wn))+
  geom_hline(yintercept = 0,linetype="dotted")+
  geom_ribbon(aes(ymin=min,ymax=max),alpha=.1)+
  geom_ribbon(aes(ymin=q25,ymax=q75),alpha=.1)+
  geom_line(aes(y=median),linewidth=.25)+

  geom_line(
  data=
    codebook_prep%>%mutate(x= floor(x),
                           y = (y*1.25)%>%floor,
                           label=paste(x,y,sep="-"))%>%
       filter(label%in%c(
        "1-1"
        ,"1-10"
        ,"10-1"
        ,"10-10"
        ,"10-7"
        ,"5-6"
       # ,"9-9"
      )),
aes(x=name,y=value,col=label))+
  scale_color_manual("Node",values = colorblind_safe_colors())+
  theme_pubr()+
  scale_x_log10(expression("Wavenumber [c"*m^-1*"]"),breaks=c(500,2000,5000))+
  scale_y_continuous("Absorbance")
  



#### pc codebook vector vis #####
ggplot(data=pc_codebook$scores%>%as_tibble%>%
         bind_cols(expand.grid(x=1:10,y=1:10))%>%
         mutate(label=paste0("[",x,",",y,"]")),
       aes(x=`Comp 1`,y=`Comp 2`),col="grey")+
  #geom_point(data = pc_merge,size=.5,alpha=.5,shape=16)+
  geom_density_2d_filled(data=pc_merge)+
  scale_fill_discrete(type = c("white",RColorBrewer::brewer.pal(9,name = "Blues")))+
  geom_path(color = "grey80",aes(group=x),linewidth=.25) +
  geom_path(color = "grey80",aes(group=y),linewidth=.25) +
  geom_point(aes(col=factor(c(1:100))),shape=4,col="red",stroke=1,size=1)+
  
  geom_text(aes(label=label),col="black",size=3,vjust=1,hjust=1,check_overlap = T)+
  coord_cartesian(xlim = c(-10,13),ylim = c(-8,6))+
  xlab(paste0("PC1 (",(expl_var[1])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  ylab(paste0("PC2 (",(expl_var[2])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  #ggtitle("Self organising map grid shown in the PC space")+
  theme_pubr()+
  theme(legend.position = "none")->som_grid_plt

ggsave(plot=som_grid_plt,filename="som_pc_grid.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 5,height = 5)



ggplot(data=pc_codebook$scores%>%as_tibble%>%
         bind_cols(expand.grid(x=1:10,y=1:10))%>%
         mutate(label=paste0("[",x,",",y,"]")),
       aes(x=`Comp 3`,y=`Comp 4`),col="grey")+
  #geom_point(data = pc_merge,size=.5,alpha=.5,shape=16)+
  geom_density_2d_filled(data=pc_merge)+
  scale_fill_discrete(type = c("white",RColorBrewer::brewer.pal(9,name = "Blues")))+
  geom_path(color = "grey80",aes(group=x),linewidth=.25) +
  geom_path(color = "grey80",aes(group=y),linewidth=.25) +
  geom_point(aes(col=factor(c(1:100))),shape=4,col="red",stroke=1,size=1)+
  
  geom_text(aes(label=label),col="black",size=3,vjust=1,hjust=1,check_overlap = T)+
  coord_cartesian(xlim = c(-4.5,3.5),ylim = c(-3.5,3))+
  xlab(paste0("PC3 (",(expl_var[3])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  ylab(paste0("PC4 (",(expl_var[4])%>%format(digits=2,nsmall = 1,scientific = F),"%)"))+
  #ggtitle("Self organising map grid shown in the PC space")+
  theme_pubr()+
  theme(legend.position = "none")->som_grid_plt2



som_grid_plt2
  
  ggsave(plot=som_grid_plt2,filename="som_pc_grid2.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 5,height = 5)



#### property vis ####
# Coordinates of SOM units
unit_coords <- som_model$grid$pts  # a data frame of x, y positions for each SOM node

# Each observation's unit mapping (which neuron it belongs to)
obs_units <- som_model$unit.classif

# Map each observation to its SOM unit's (x, y) coordinates
obs_coords <- unit_coords[obs_units, ]

# Combine into a single data frame for plotting
plot_data <- data.frame(
  x = obs_coords[, 1],
  y = obs_coords[, 2]
)%>%bind_cols(pc_merge)

sel_sites =  c(
  "BDF02",
  "BDF12",
  "BDF13",
  "BDF23",
  "BDF30",
  "BDF33",
  "BDF35",
  "BDF46"
)

# Plot the hex grid template
ggplot(positions, aes(x = x, y = y)) +
  geom_hex(stat = "identity", fill = rgb(1,1,1,0), color = "black",linewidth=.5) +  # for actual hex shapes
  coord_fixed() +
  theme_pubr()+
# plot mapping
  geom_jitter(data=plot_data%>%mutate(site_id_col=if_else(site_id%in%sel_sites,site_id,"other"),
                                      topbot=(if_else(Depth_top>.25,"b","t"))),
              aes(x = x, y = y, color =site_id_col,shape = topbot,size=site_id_col),
              width = 0.2, height = 0.2,alpha=.5,stroke=1.25) +
  #theme_minimal() +
  theme(panel.grid = element_blank())+
  coord_fixed() +
  scale_x_continuous("SOM X",breaks=seq(1.25,10.25,1),labels=c(1:10),minor_breaks = c())+
  scale_y_continuous("SOM Y",breaks=som_grid$pts[,2]%>%unique,labels=c(1:10),minor_breaks = c())+
  scale_size_manual("BDF sites",breaks=c(sel_sites,"other"),values = c(c(rep(1,8),.5)))+
  scale_shape_manual("",breaks=c("t","b"),values = c(1,4),labels=c("Topsoil","Subsoil"))+
  scale_color_manual("BDF sites",breaks=c(sel_sites,"other"),
                     values = c(colorblind_safe_colors(),rgb(.7,.7,.7,.25)),na.value = rgb(0,0,0,0))+
  ggtitle("Self-organising map - BDF sites")->som_codes_sites
 
som_codes_sites

ggsave(plot=som_codes_sites+theme(legend.position = "none"),filename="som_code_sites.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 5,height = 5)

# legend
ggsave(plot=get_legend(som_codes_sites,position = "right"),filename="som_code_sites_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)


##### loop ####
# Plot the hex grid template
som_var_list=c()
for (var_ in names(variable_lookup)){
  data_all=left_join(positions,
            plot_data%>%group_by(x,y)%>%
              summarise(mean=mean(.data[[variable_lookup[[var_]]]],na.rm=T),
                        n=length(na.omit(.data[[variable_lookup[[var_]]]])))
  )
  data_n=data_all%>%filter(n>0)
  min_val=min(data_all$mean,na.rm = T)
  max_val=max(data_all$mean,na.rm = T)
ggplot(#positions,
  data_all,
  aes(x = x, y = y,fill=mean)
  )+
  geom_hex(stat = "identity", #fill = rgb(1,1,1,0),
           color = "black",linewidth=.5) +  # for actual hex shapes
  coord_fixed() +
  theme_pubr()+
  geom_label(data=data_n,aes(x=x,y=y,label=n),fill="white",label.padding = unit(.15,"lines"),size=3,alpha=.75)+
  # plot mapping
  # geom_jitter(
  #   data=
  #     plot_data%>%mutate(site_id_col=if_else(site_id%in%sel_sites,site_id,"other"),
  #                                     topbot=(if_else(Depth_top>.25,"b","t"))),
  #             aes(x = x, y = y, color =site_id_col,shape = topbot,size=site_id_col),
  #             width = 0.2, height = 0.2,alpha=.5,stroke=1.25) +
  # #theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size=15))+
  coord_fixed(xlim=c(1,11),ylim=c(.7,9)) +
  scale_x_continuous("SOM X",breaks=seq(1.25,10.25,1),labels=c(1:10),minor_breaks = c())+
  scale_y_continuous("SOM Y",breaks=som_grid$pts[,2]%>%unique,labels=c(1:10),minor_breaks = c())+
  scale_fill_viridis_c(name = paste(variable_lookup[[var_]],"    "),labels=c(format(min_val,digits=2,nsmall=0),format(max_val,digits=2,nsmall=0)),breaks=c(min_val,max_val))->som_var
som_var_list[[var_]]=som_var
#som_TOC
ggsave(plot=som_var#+theme(legend.position = "none")
       ,filename=paste0("som_",var_,".png"),path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/hexplots som refvar/",width = 5,height = 5)
}

ggarrange(som_var_list$TOC,som_var_list$Nt,som_var_list$Pt,
          som_var_list$Al_t,som_var_list$Fe_t,som_var_list$KAKpot,
          som_var_list$S,som_var_list$U,som_var_list$`T`)%>%
  ggsave(filename="som_var_multiplot.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/hexplots som refvar/",width = 15,height = 15)

# all var som property plots
if(F){
  
  # Plot the hex grid template
  som_var_list_all=c()
  vars_=c(names(Data_table_reference%>%select(where(is.numeric))),
          names(Data_table_soliTOC%>%select(where(is.numeric))),
          names(select(Event_table,where(is.numeric))),"DateEvent")
  
  for (var_ in vars_){
    
    data_all=left_join(positions,
                       summarise(group_by(plot_data,x,y),mean=mean(.data[[var_]],na.rm=T),
                                 n=length(na.omit(.data[[var_]]))))
    
    
    data_n=filter(data_all,n>0)
    
    min_val=min(data_all$mean,na.rm = T)
    max_val=max(data_all$mean,na.rm = T)
    
    ggplot(
      data_all,
      aes(x = x, y = y,fill=mean)
    )+
      geom_hex(stat = "identity", #fill = rgb(1,1,1,0),
               color = "black",linewidth=.5) +  # for actual hex shapes
      coord_fixed() +
      theme_pubr()+
      geom_label(data=data_n,aes(x=x,y=y,label=n),fill="white",label.padding = unit(.15,"lines"),size=3,alpha=.75)+
      
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_text(size=15))+
      coord_fixed(xlim=c(1,11),ylim=c(.7,9)) +
      scale_x_continuous("SOM X",breaks=seq(1.25,10.25,1),labels=c(1:10),minor_breaks = c())+
      scale_y_continuous("SOM Y",breaks=unique(som_grid$pts[,2]),labels=c(1:10),minor_breaks = c())+
      scale_fill_viridis_c(name = paste(var_,"    "),
                           labels=c(format(min_val,digits=2,nsmall=0),format(max_val,digits=2,nsmall=0)),
                           breaks=c(min_val,max_val))->som_var
    som_var_list_all[[var_]]=som_var
    #som_TOC
    ggsave(plot=som_var#+theme(legend.position = "none")
           ,filename=paste0("som_",str_replace_all(str_replace(var_,"%","pct"),"/","_"),".png"),
           path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/hexplots som refvar/all/",width = 5,height = 5)
  }
}

##### etc ####


ggplot(plot_data, aes(x = x, y = y, color =`U [wt-%]`,shape = `Land use`)) +
  geom_jitter(width = 0.2, height = 0.2, size = 3) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "SOM Mapping in ggplot2", x = "SOM X", y = "SOM Y")+scale_color_viridis_c(na.value = rgb(0,0,0,0),alpha = .5)


ggplot(plot_data, aes(x = x, y = y, color =`TOC [wt-%]`,shape = `Land use`)) +
  geom_jitter(width = 0.2, height = 0.2, size = 3) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "SOM Mapping in ggplot2", x = "SOM X", y = "SOM Y")+scale_color_viridis_c(na.value = rgb(0,0,0,0),alpha = .5)


## Predictions 2024models all ####

### do not use DE-2023-BDF_archive data though, was used for calibration and validation.
### due to random splitting, there is no re-attribution of LabelEvent possible, just obs/pred comparison of test sets (handed by model object)
### Re-running with added metadata would take weeks to months - no time

if(F){ # takes quite some time to run
 ### PLS ####
  
  pls_pred=tibble(ID=spc_tmp$LabelEvent)
  pb=progress::progress_bar$new(total=length( list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/"),pattern="pls_",full.names = T)))
  for (mod_path_i in list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/"),pattern="pls_",full.names = F)){
    
    pred_i=predict_variable(target_data = spc_tmp,
                            model_folder = paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/"),
                            manual = mod_path_i)
    pls_pred=tibble(pls_pred,RES=bind_cols(pred_i$predictions,
                                                 pred_i$spc_diss,
                                                 pred_i$model$bestTune))%>%
      rename_with(.cols=RES,.fn=~mod_path_i)
    pb$tick()
  }
  saveRDS(pls_pred,paste0(data_dir,"/Sean_Environment/R_main/model_out/pls_pred"))
  
 ### Cubist ####
  
  cubist_pred=tibble(ID=spc_tmp$LabelEvent)
  pb=progress::progress_bar$new(total=length( list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/"),pattern="cubist_",full.names = T)))
  for (mod_path_i in list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/"),pattern="cubist_",full.names = F)){
    
  pred_i=predict_variable(target_data = spc_tmp,
                          model_folder = paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/"),
                          manual = mod_path_i)
  cubist_pred=tibble(cubist_pred,RES=bind_cols(pred_i$predictions,
                                            pred_i$spc_diss,
                                            pred_i$model$bestTune))%>%
    rename_with(.cols=RES,.fn=~mod_path_i)
  pb$tick()
  }
  saveRDS(cubist_pred,paste0(data_dir,"/Sean_Environment/R_main/model_out/cubist_pred"))
  
  ### MBL ####
  # reloading predictions (MBL is the odd one, due to its working principle)
  #!!! NOTE that log1p has not yet been expm1
  #### frisch ####
  # load BDF_frisch original data (3 samples omitted from BDF-SSL later on)
  {
    tmp1=read_rds(paste0(data_dir,"/BDF/BDF-SSL/4_datasets/Adam-2024_BDF_frisch_sub-1"))
    tmp2=read_rds(paste0(data_dir,"/BDF/BDF-SSL/4_datasets/Adam-2024_BDF_frisch_sub-2"))
    tmp3=read_rds(paste0(data_dir,"/BDF/BDF-SSL/4_datasets/Adam-2024_BDF_frisch_sub-3"))
    BDF_frisch=rbind(
      tmp1,
      tmp2,
      tmp3
    )
    rm(tmp1)
    rm(tmp2)
    rm(tmp3)
  }
  
  
  
  
  mbl_frisch=tibble(ID=BDF_frisch$sample_id)
  pb=progress::progress_bar$new(total=length( list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/MBL_models_better-split_BDF_frisch/"),pattern="mbl_",full.names = T)))
  for (mod_path_i in list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/MBL_models_better-split_BDF_frisch/"),pattern="mbl_",full.names = T)){
    mod_i=readRDS(mod_path_i)
    
    pred_i=tibble(RES=mod_i$results[[
      paste0("k_",
             mod_i$validation_results$local_cross_validation$k[
               which.min(mod_i$validation_results$local_cross_validation$st_rmse)
             ]
      )
    ]] 
    )%>%
      rename_with(.cols=RES,.fn=~basename(mod_path_i))
    
    nrow(pred_i)
    
    mbl_frisch=bind_cols(mbl_frisch,pred_i)
    pb$tick()
  }
  saveRDS(mbl_frisch,paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_frisch"))
    
  # predict_variable( 
  #   BDF_frisch, 
  #   model_folder = paste0(root_dir,model_folder),
  #   prefix = "pls_", # model type, usually
  #   variable_ = i,
  #   metric = "test.rmse",
  #   maximise = F, # set T, e.g. when R2 is used as metric
  #   restrict = T, # choose optimal model only form sets that are available both in Xu (new data) and Xr (calibrated)
  #   diss_limit = 2) # limit for outlier flagging
  # pred_out=data.frame(left_join(pred$predictions,pred$spc_diss),
  #                     model_name=pred$model_name)
  # 
  #### zeitreihe ####
  mbl_zeitreihe=tibble(ID=BDF_SSL%>%filter(!is.na(spc_rs[,1]))%>%filter(LabelEvent%>%str_starts("LC"))%>%pull(LabelEvent))
  
  
  
  
  pb=progress::progress_bar$new(total=length( list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/MBL_Zeitreihe/"),pattern="mbl_",full.names = T)))
  for (mod_path_i in list.files(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/MBL_Zeitreihe/"),pattern="mbl_",full.names = T)){
    mod_i=readRDS(mod_path_i)
    
    pred_i=tibble(RES=mod_i$results[[
      paste0("k_",
             mod_i$validation_results$local_cross_validation$k[
               which.min(mod_i$validation_results$local_cross_validation$st_rmse)
             ]
      )
    ]] 
    )%>%
      rename_with(.cols=RES,.fn=~basename(mod_path_i))
    
    nrow(pred_i)
    
    mbl_zeitreihe=bind_cols(mbl_zeitreihe,pred_i)
    pb$tick()
  }
  
  
  saveRDS(mbl_zeitreihe,paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_zeitreihe"))
  
}


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
  #### partial ####
  pls_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/pls_pred"))
  
  cubist_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/cubist_pred"))
  
  mbl_frisch=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_frisch"))
  
  mbl_zeitreihe=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_zeitreihe"))

}

all_pred=left_join(pls_pred,cubist_pred)%>%left_join(bind_rows(mbl_frisch,mbl_zeitreihe))
all_data=left_join(BDF_SSL,spc_tmp%>%select(-spc_rs),by="LabelEvent")%>%left_join(all_pred,by=c("LabelEvent"="ID"))



#saveRDS(spc_tmp,paste0(data_dir,"/Sean_Environment/R_main/model_out/spc_tmp"))
#saveRDS(all_data,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))
# local serialisation when not at uni -> move manually to dir stated above
saveRDS(spc_tmp,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/temp/spc_tmp")
saveRDS(all_data,"C:/Users/adam/Desktop/UNI/PhD/DISS/data/temp/all_data")


all_data=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))

# !!! log1p mbl are not expm1 yet !!!


## eval frisch / zeitreihe pred ####
eval_all=c()
eval_frisch=c()
eval_zeitreihe=c()
for (i in all_data%>%select(contains(c("pls","cubist","mbl"))&!contains("ratio"))%>%names()){
  type = str_split_fixed(i,"_",2)[[1]]
  set = str_split_fixed(str_split_fixed(i,"_",2)[[2]],"-",2)[[1]]
  trans = str_split_fixed(i,"-",3)[[2]]
  pred_var = str_split_fixed(i,"-",3)[[3]]
  obs_var = variable_lookup[[pred_var]]

  
  dataset=na.omit(tibble(all_data%>%select(Campaign,LabelEvent),obs=all_data[[obs_var]],pred=all_data[[i]]$pred))
  if(type=="mbl"&trans=="log1p"){
    dataset$pred=expm1(dataset$pred)
  }
  # note: only additional for soliTOC
  eval_all=bind_rows(eval_all,
                     tibble(subset="all",
                            type,
                            set,
                            trans,
                            variable=pred_var,
                            evaluate_model_adjusted(dataset,obs="obs",pred="pred")))
  # note: only for soliTOC
  if((dataset%>%filter(Campaign%>%str_detect("field"))%>%nrow)>0){
  eval_frisch=bind_rows(eval_frisch,
                        tibble(subset="frisch",
                               type,
                               set,
                               trans,
                               variable=pred_var,
                               evaluate_model_adjusted(dataset%>%filter(Campaign%>%str_detect("field")),
                                                       obs="obs",pred="pred")))
  }
  # for all
  eval_zeitreihe=bind_rows(eval_zeitreihe,
                           tibble(subset="zeitreihe",
                                  type,
                                  set,
                                  trans,
                                  variable=pred_var,
                                  evaluate_model_adjusted(dataset%>%filter(Campaign%>%str_detect("DE-2024-BDF_BfUL")|
                                                                             Campaign%>%str_detect("DE-2024-BDF_archive")),
                                                          obs="obs",pred="pred"))
                           )
  
}

saveRDS(eval_all,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_eval"))
saveRDS(eval_zeitreihe,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_zeitreihe"))
saveRDS(eval_frisch,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_frisch"))

## 2024models all evaluation test + zeitreihe ####

### all testing ####
pls_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/evaluation"))
cubist_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/evaluation"))
mbl_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Mbl_models_better-split/evaluation"))

bind_rows(
tibble(type="pls",pls_eval$new_eval_table),
tibble(type="cubist",cubist_eval$new_eval_table),
tibble(type="mbl",mbl_eval$new_eval_table)
)->all_test_eval


#### obspredplots ####

pls_eval$Obs_Pred_data%>%names



# best rmse_test
all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
                         group_by(variable)%>%
                         filter(rmse==min(rmse))->best_candidates


view(best_candidates)
write_excel_csv(best_candidates,"C:/Users/adam/Desktop/UNI/PhD/DISS/tables/best_candidates_test.csv")
  
  # # log y
  # all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
  #   group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
  #   right_join(
  #     all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
  #     ,by="variable")%>%
  #   ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
  #              y=nrmseavg*100,
  #              group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
  # #  geom_hline(yintercept = c(5,10,25,50,100,250,500,1000,2000),linetype="dotted")+
  #   geom_col(position="dodge",linewidth=1,aes(fill=variable))+
  #   #scale_y_log10(breaks=c(5,10,25,50,100,250,500,1000,2000))+
  #   #coord_cartesian(ylim=c(5.5,150))+
  #   stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+theme_pubr()+
  #   theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
  #         axis.title.x = element_blank())+ylab(expression("NRMS"*E[avg]*" [%]"))+
  #   annotate(geom = "text",label="Test evaluation",x = 1,y=120,hjust=0,size=5)#->test_eval_bar
  # 
  # 
  
#### nrmseavg ####
  ##### test split axis ####
  all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
    group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
    right_join(
      eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
      ,by="variable")%>%
    ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
               y=nrmseavg*100,
               group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
    geom_hline(yintercept = c(10,25,50,75),linetype="dotted")+
    geom_col(position="dodge",linewidth=1,aes(fill=variable))+
    scale_y_continuous(breaks=c(10,25,50,75),expand=c(0,0)) +
    coord_cartesian(ylim=c(7.5,80))+
    stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+theme_pubr()+
    theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(
            angle = 90,
            hjust = .6,
            vjust=0
          ))+
    ylab(expression("NRMS"*E[avg]*" [%]"))->test_eval_bar1
  
  
  
  all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
    group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
    right_join(
      eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
      ,by="variable")%>%
    ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
               y=nrmseavg*100,
               group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
    geom_hline(yintercept = c(100,250),linetype="dotted")+
    #geom_hline(yintercept = 0)+
    geom_col(position="dodge",linewidth=1,aes(fill=variable))+
    scale_y_continuous(breaks=c(100,250),expand=c(0,0)) +
    coord_cartesian(ylim=c(80,250))+
    #stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+
    theme_pubr()+
    theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          axis.title.x = element_blank())+
    theme(
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )+
    annotate(geom = "text",label="Extended library evaluation",x = 1,y=500,hjust=0,size=5)->test_eval_bar2
  
  
  test_eval_bar={test_eval_bar2 / test_eval_bar1 + plot_layout(heights = c(.1, .9),guides = "collect") &
      theme(plot.margin = margin(0, 0, 0, 0))}
  test_eval_bar
  ggsave(plot=test_eval_bar,filename="test_eval_bar.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         width=8,
         height=4)
  # #rpiq
  # all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
  #   group_by(variable)%>%summarise(rpiq_max=max(rpiq))%>%
  #   right_join(
  #     all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
  #     ,by="variable")%>%
  #   ggplot(aes(x=fct_reorder(variable,rpiq_max,.desc = T),
  #              y=rpiq,
  #              group=fct_reorder(paste(variable,type,set,trans),rpiq,.desc=T)))+
  #   #geom_hline(yintercept = c(5,10,25,50,100,250,500,1000,2000),linetype="dotted")+
  #   geom_col(position="dodge",linewidth=1,aes(fill=variable))+
  #   #scale_y_log10(breaks=c(5,10,25,50,100,250,500,1000,2000))+
  #   #coord_cartesian(ylim=c(5.5,150))+
  #   stat_summary(geom="point",fun = max,aes(group=variable),shape="_",size=10)+theme_pubr()+
  #   theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
  #         axis.title.x = element_blank())+ylab("RPIQ")+
  #   annotate(geom = "label",label="Test evaluation",x = 1,y=1,hjust=0,size=5)->test_eval_bar_rpiq
  # test_eval_bar_rpiq
  
  ggsave(plot=test_eval_bar,filename="test_eval_bar.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         width=8,
         height=4)
  
  
  
  
  ###### compare test and zr eval ####
  eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
    group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
    right_join(
      eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
      ,by="variable")%>%
    left_join(all_test_eval#%>%select(type,set,trans,variable,nrmseavg)%>%
                #rename(nrmseavg_test=nrmseavg)
                ,by=c("type","set","trans","variable"),suffix = c("","_test"))%>%
    
    ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
               y=nrmseavg*100,
               group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
    geom_hline(yintercept = c(10,25,50,75),linetype="dotted")+
    geom_col(position="dodge",linewidth=1,aes(fill=nrmseavg_test))+
    scale_y_log10()+
    coord_cartesian(ylim=c(9,1250))+
    scale_fill_viridis_c()+
    stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+theme_pubr()+
    theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(
            angle = 90,
            hjust = .6,
            vjust=0
          ))+
    ylab(expression("NRMS"*E[avg]*" [%]"))
  
  
  ##### zr split axis ####
  eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
    group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
    right_join(
      eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
      ,by="variable")%>%
    ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
               y=nrmseavg*100,
               group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
    geom_hline(yintercept = c(10,25,50,75),linetype="dotted")+
    geom_col(position="dodge",linewidth=1,aes(fill=variable))+
    scale_y_continuous(breaks=c(10,25,50,75),expand=c(0,0)) +
    coord_cartesian(ylim=c(7.5,80))+
    stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+theme_pubr()+
    theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(
            angle = 90,
            hjust = .6,
            vjust=0
          ))+
    ylab(expression("NRMS"*E[avg]*" [%]"))->zr_eval_bar1
  
  
  
  eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))%>%#mutate(NRMSE_iqr=test.rmse/test.iqr*100)%>%
    group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
    right_join(
      eval_zeitreihe%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
      ,by="variable")%>%
    ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
               y=nrmseavg*100,
               group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
    geom_hline(yintercept = c(100,1000),linetype="dotted")+
    #geom_hline(yintercept = 0)+
    geom_col(position="dodge",linewidth=1,aes(fill=variable))+
    scale_y_continuous(breaks=c(100,1000),expand=c(0,0)) +
    coord_cartesian(ylim=c(80,1250))+
    #stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+
    theme_pubr()+
    theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
          axis.title.x = element_blank())+
    theme(
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )+
    annotate(geom = "text",label="Extended library evaluation",x = 1,y=500,hjust=0,size=5)->zr_eval_bar2
  
  
  zr_eval_bar={zr_eval_bar2 / zr_eval_bar1 + plot_layout(heights = c(.1, .9),guides = "collect") &
      theme(plot.margin = margin(0, 0, 0, 0))}
  zr_eval_bar
  ggsave(plot=zr_eval_bar,filename="zr_eval_bar.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
  width=8,
  height=4)


##### zr with tst best model as highlighted ####
  {
    best_candidates%>%select(type,set,trans,variable)%>%mutate(best=1)%>%
      right_join(eval_zeitreihe)%>%
      mutate(best=if_else(is.na(best),0,1))%>%mutate(nrmseavg=1/rpiq)->tmp0
    tmp0%>%
      filter(variable%>%str_detect("ratio",negate = T))%>%
      group_by(variable)%>%summarise(NRMSEavg_min=min(nrmseavg))%>%
      right_join(
        tmp0%>%filter(variable%>%str_detect("ratio",negate = T))#%>%mutate(NRMSE_iqr=test.rmse/test.iqr*100)
        ,by="variable")->tmp
    # zr split axis
    tmp%>%
      ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
                 y=nrmseavg*100,
                 group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
      geom_hline(yintercept = c(10,25,50,75,100),linetype="dotted")+
      geom_col(position="dodge",linewidth=1,aes(fill=variable))+
      geom_col(data=tmp,position="dodge",aes(y=if_else(best==1,nrmseavg*100,0)),fill="red3")+
      geom_point(data=tmp%>%filter(best==1),shape="_",size=10,col="red3")+
      scale_y_continuous(breaks=c(10,25,50,75,100),expand=c(0,0)) +
      coord_cartesian(ylim=c(7.5,120))+
      stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+theme_pubr()+
      theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
            axis.title.x = element_blank(),
            axis.title.y = element_text(
              angle = 90,
              hjust = .6,
              vjust=0
            ))+
      ylab(expression("NRMS"*E[avg]*" [%]"))->zr_eval_bar1
    zr_eval_bar1
    
    
    
    
    tmp%>%
      ggplot(aes(x=fct_reorder(variable,NRMSEavg_min),
                 y=nrmseavg*100,
                 group=fct_reorder(paste(variable,type,set,trans),nrmseavg*100)))+
      geom_hline(yintercept = c(200,1000),linetype="dotted")+
      #geom_hline(yintercept = 0)+
      geom_col(position="dodge",linewidth=1,aes(fill=variable))+
      geom_col(data=tmp,position="dodge",aes(y=if_else(best==1,nrmseavg*100,0)),fill="red3")+
      # geom_point(data=tmp%>%filter(best==1),shape="_",size=10,col="red3")+
      scale_y_continuous(breaks=c(200,1000),expand=c(0,0)) +
      coord_cartesian(ylim=c(120,1250))+
      #stat_summary(geom="point",fun = min,aes(group=variable),shape="_",size=10)+
      theme_pubr()+
      theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1),
            axis.title.x = element_blank())+
      theme(
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      )+
      annotate(geom = "text",label="Extended library evaluation",x = 1,y=500,hjust=0,size=5)->zr_eval_bar2
    
    
    zr_eval_bar_tst={zr_eval_bar2 / zr_eval_bar1 + plot_layout(heights = c(.1, .9),guides = "collect") &
        theme(plot.margin = margin(0, 0, 0, 0))}
    zr_eval_bar_tst
    
    ggsave(plot=zr_eval_bar_tst,filename="zr_eval_bar_with_tst.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
           width=8,
           height=4)
    
  }

  
#### rpd combined ####
  bind_rows(tibble(ds="tst",all_test_eval),
                   tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(max_rpd=max(rpd))%>%
    ggplot()+
    
    # williams 2014 (though some authors are far less rigourous with rpd>2 as very good)
    geom_rect(data=tibble(
      ymin=c(-Inf,2,2.5,3,3.5,4),
      ymax=c(2,2.5,3,3.5,4,Inf),
      group=c("vp","p","f","g","vg","e"),
      ),
      aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_boxplot(aes(x=fct_reorder(variable,rpd,.desc = T),
                     y=rpd,
                     group=paste(variable,ds),
                     fill=ds
                   #  group=paste(variable,d_type),
                 #    fill=d_type
                 )
                 )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    scale_y_continuous("RPD",expand=c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
                                  c("vp","p","f","g","vg","e"),"tst","zr"),
                      labels=c(#"PLS","Cubist","MBL",
                               "Very Poor (< 2.0)" ,"Poor (2.0 - 2.5)","Fair (2.5 - 3.0)","Good (3.0 - 3-5)", "Very Good (3.5 - 4.0)","Excellent (> 4.0)"
                               ,"Test set","Extended library"),
                      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
                                 unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr"))))+
    geom_vline(xintercept = c(1:30)+.5,col="white")+
    #ggtitle("Testset")+
    coord_cartesian(ylim = c(0,11.5))+
    theme(legend.position = "right",
      panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=45,hjust=1))->rpd_comparison
  
  
  ggsave(plot=rpd_comparison+theme(legend.position = "none"),filename="RPD_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # legend
  ggsave(plot=get_legend(rpd_comparison),filename="RPD_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

  
#### rmse combined ####
  # RMSE/sd == 1/RPD ... redundant
  
  bind_rows(tibble(ds="tst",all_test_eval),
            tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
    #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(rmse=rmse/sdev,min_rmse=min(rmse))%>%#---------------------------------change to min if minimize goal
    ggplot()+
    
    
    # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
    # geom_rect(data=tibble(
    #   ymin=c(-1,.65,.8,.9,.95,.99),
    #   ymax=c(.65,.8,.9,.95,.99,1),
    #   group=c("vp","p","f","g","vg","e"),
    # ),
    # aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    # 
    geom_boxplot(aes(x=fct_reorder(variable,min_rmse,.desc = F),
                     y=rmse,
                     group=paste(variable,ds),
                     fill=ds
                     #  group=paste(variable,d_type),
                     #    fill=d_type
    )
    )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    geom_point(
      data=
      bind_rows(tibble(ds="tst",all_test_eval),
                tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
        # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
        #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
        
        mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
               group=paste0(variable,d_type))%>%
        group_by(variable)%>%
        mutate(rmse=rmse/sdev,min_rmse=min(rmse))%>%filter(rmse>5),
      aes(x=fct_reorder(variable,min_rmse,.desc = F),
                   y=rmse,
                   group=paste(variable,ds)),position = position_nudge(x=.2,y=-2.2),
      shape="*",size=8)+
    scale_y_continuous("NRMSEsd",expand=c(0,0),limits=c(0,6),oob = scales::squish)+
    scale_fill_manual("Quality",
                      breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),"tst","zr","o","no"),
      
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor" ,"Poor","Fair","Good", "Very Good","Excellent"
        ,"Test set","Extended library","o","no"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr")),
        setNames(list(rgb(1,0,0,1),rgb(1,1,1,0)),c("o","no"))))+
    geom_vline(xintercept = c(1:30)+.5,col="white")+
    ggtitle("Testset")+
    coord_cartesian(ylim = c(0,3.9))+
    theme(legend.position = "right",
          panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))->rmse_comparison
  rmse_comparison
  
  # ggsave(plot=R2_comparison+theme(legend.position = "none"),filename="linccc_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # # legend
  # ggsave(plot=get_legend(R2_comparison),filename="R2_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  # 
  
  
#### R2 combined ####
  
  
  bind_rows(tibble(ds="tst",all_test_eval),
            tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
    #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(max_R2=max(R2))%>%#---------------------------------change to min if minimize goal
    ggplot()+
    
    
    # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
    geom_rect(data=tibble(
      ymin=c(-1,.65,.8,.9,.95,.99),
      ymax=c(.65,.8,.9,.95,.99,1),
      group=c("vp","p","f","g","vg","e"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_boxplot(aes(x=fct_reorder(variable,max_R2,.desc = T),
                     y=R2,
                     group=paste(variable,ds),
                     fill=ds
                     #  group=paste(variable,d_type),
                     #    fill=d_type
    )
    )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    scale_y_continuous("R2",expand=c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),"tst","zr"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor" ,"Poor","Fair","Good", "Very Good","Excellent"
        ,"Test set","Extended library"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr"))))+
    geom_vline(xintercept = c(1:30)+.5,col="white")+
    #ggtitle("Testset")+
    coord_cartesian(ylim = c(-1,1))+
    theme(legend.position = "right",
          panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))->R2_comparison
  
  # 
   ggsave(plot=R2_comparison+theme(legend.position = "none"),filename="linccc_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # # legend
   ggsave(plot=get_legend(R2_comparison),filename="R2_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  # 
  
#### linCCC combined ####
  
  
  bind_rows(tibble(ds="tst",all_test_eval),
            tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
    #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(max_linsCCC=max(linsCCC))%>%#---------------------------------change to min if minimize goal
    ggplot()+
    
    
    # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
    geom_rect(data=tibble(
      ymin=c(-1,.65,.8,.9,.95,.99),
      ymax=c(.65,.8,.9,.95,.99,1),
      group=c("vp","p","f","g","vg","e"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_boxplot(aes(x=fct_reorder(variable,max_linsCCC,.desc = T),
                     y=linsCCC,
                     group=paste(variable,ds),
                     fill=ds
                     #  group=paste(variable,d_type),
                     #    fill=d_type
    )
    )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    scale_y_continuous("Lin's CCC",expand=c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),"tst","zr"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor (< 0.65)" ,"Poor  (0.65 - 0.80)","Fair (0.80 - 0.90)","Good (0.90 0.95)", "Very Good (0.95 - 0.99)","Excellent (> 0.99)"
        ,"Test set","Extended library"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr"))))+
     geom_vline(xintercept = c(1:30)+.5,col="white")+
    #ggtitle("Testset")+
    coord_cartesian(ylim = c(0,1))+
    theme(legend.position = "right",
          panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))->linccc_comparison

  
  ggsave(plot=linccc_comparison+theme(legend.position = "none"),filename="linccc_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # legend
  ggsave(plot=get_legend(linccc_comparison),filename="linccc_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  
  
  
#### bias combined ####
  # ! NOTE avg-normalised, as variable unit dependant
  
  bind_rows(tibble(ds="tst",all_test_eval),
            tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
    #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(bias=bias/sdev,min_bias=min(bias))%>%#---------------------------------change to min if minimize goal
    ggplot()+
    
    
    geom_rect(data=tibble(
      ymin=c(-Inf,-.5,-.3,-.2,-.1,-.05,.05,.1,.2,.3,.5),
      ymax=c(-.5,-.3,-.2,-.1,-.05,.05,.1,.2,.3,.5,Inf),
      group=c("vp","p","f","g","vg","e","vg","g","f","p","vp"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_boxplot(aes(x=fct_reorder(variable,min_bias,.desc = T),
                     y=bias,
                     group=paste(variable,ds),
                     fill=ds
                     #  group=paste(variable,d_type),
                     #    fill=d_type
    )
    )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    scale_y_continuous("Standardised Bias",expand=c(0,0))+
    scale_fill_manual("Quality (|bias|)",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),"tst","zr"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor (> 0.5)" ,"Poor (0.3 - 0.5)","Fair (0.2 - 0.3)","Good (0.1 - 0.2)", "Very Good (0.05 - 0.1)","Excellent (< 0.05)"
        ,"Test set","Extended library"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr"))))+
    geom_vline(xintercept = c(1:30)+.5,col="white")+
    #ggtitle("Testset")+
    coord_cartesian(ylim = c(-1.7,1.5))+
    theme(legend.position = "right",
          panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))->bias_comparison
  
  
  ggsave(plot=bias_comparison+theme(legend.position = "none"),filename="bias_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # legend
  ggsave(plot=get_legend(bias_comparison),filename="bias_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  
  
  
  
#### slope combined ####
  # ! NOTE avg-normalised, as variable unit dependant
  
  bind_rows(tibble(ds="tst",all_test_eval),
            tibble(ds="zr",eval_zeitreihe))%>%filter(!str_detect(variable,"ratio"))%>%
    # mutate(ds=factor(ds,levels = c("tst","zr"),labels=c(1,2)),
    #        type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
    
    mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
           group=paste0(variable,d_type))%>%
    group_by(variable)%>%
    mutate(min_b=abs(b-1))%>%#---------------------------------change to min if minimize goal
    ggplot()+
    
    
    geom_rect(data=tibble(
      ymin=c(0,.75, .85, .9, .95, .98, 1.02, 1.05, 1.1, 1.2,1.33),
      ymax=c(.75,.85, .9, .95, .98, 1.02, 1.05, 1.1, 1.2, 1.33,Inf),
      group=c("vp","p","f","g","vg","e","vg","g","f","p","vp"),
    ),
    aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
    
    geom_boxplot(aes(x=fct_reorder(variable,min_b,.desc = F),
                     y=b,
                     group=paste(variable,ds),
                     fill=ds
                     #  group=paste(variable,d_type),
                     #    fill=d_type
    )
    )+
    #geom_point()
    theme_pubr()+
    theme(axis.text.x = element_text(angle=90,hjust=1))+
    scale_y_continuous("Slope offset",expand=c(0,0))+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),"tst","zr"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor" ,"Poor","Fair","Good", "Very Good","Excellent"
        ,"Test set","Extended library"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[2:3],c("tst","zr"))))+
    geom_vline(xintercept = c(1:30)+.5,col="white")+
    #ggtitle("Testset")+
    coord_cartesian(ylim = c(0,2.2))+
    theme(legend.position = "right",
          panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45,hjust=1))#->b_comparison
  
  
  ggsave(plot=b_comparison+theme(legend.position = "none"),filename="b_comp.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
  # legend
  ggsave(plot=get_legend(b_comparison),filename="b_comp_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)
  
  
## processing effect ####
all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
  
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggplot(aes(x=d_set,group=group,y=relrmse,fill=d_type))+geom_boxplot(outliers = F,notch = T,alpha=.1,position = position_dodge(width = .8))+
  geom_jitter(shape=4,alpha=.5,size=.5,aes(col=d_type,group=group),
              position = position_jitterdodge(dodge.width = .8,jitter.width = 0.2))+
  scale_y_continuous("Rel. RMSE [%]",limits=c(30,270))+
  scale_fill_manual("",breaks=c("a","b","c"),labels=c("PLS","Cubist","MBL"),values = colorblind_safe_colors()[2:4])+
  scale_color_manual("",breaks=c("a","b","c"),labels=c("PLS","Cubist","MBL"),values = colorblind_safe_colors()[2:4])+
  scale_x_discrete("",breaks=c("A","B","C","D","E","F"),labels=c("spc_rs4","spc_sg_rs4","spc_sg_bl_rs4","spc_sg_snv_rs4","spc_sg1d_rs4","spc_sg2d_rs4"))+
  geom_hline(yintercept = 100)+
  theme_pubclean()+
  ggtitle("Testset")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  geom_text(aes(x = "E",y = 250,group="bE",label="> 300 %\n*"),hjust = .5,col=colorblind_safe_colors()[3],
            show.legend = F)->plt_preprocessing_relrmse_test

plt_preprocessing_relrmse_test



# all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
#   mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
#          d_set=case_match(set,
#                           "spc_rs4" ~ "A",
#                           "spc_sg_rs4" ~ "B",
#                           "spc_sg_bl_rs4" ~"C",
#                           "spc_sg_snv_rs4" ~ "D",
#                           "spc_sg1d_rs4" ~ "E",
#                           "spc_sg2d_rs4" ~ "F"),
#          group=paste0(d_set,d_type))%>%
#   group_by(variable)%>%
#   mutate(mean_rmse=mean(rmse),
#          relrmse=rmse/mean_rmse*100)%>%
#   ggplot(aes(x=variable,group=paste(variable,type),y=linsCCC,fill=type))+
#   geom_boxplot(outliers = F,alpha=.1,position = position_dodge(width = .8))+
#   geom_jitter(shape=4,alpha=.5,size=.5,aes(col=type,group=paste(variable,type)),
#               position = position_jitterdodge(dodge.width = .8,jitter.width = 0.2))+
#   ylab("Lin's CCC")+
#   xlab("")+
#   geom_hline(yintercept = 1)+
#   theme_pubclean()+
#   theme(axis.text.x = element_text(angle=45,hjust=1),legend.position = "none")
# 


ggsave(plot=plt_preprocessing_relrmse_test+theme(legend.position = "none"),filename="preprocessingRMSEtest.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
# legend
ggsave(plot=get_legend(plt_preprocessing_relrmse_test),filename="preprocessingRMSEtest_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)


### all zeitreihe ####

# Step 1: Extract all d_set–d_type combinations used in the plot
plot_groups <- eval_zeitreihe %>%filter(!str_detect(variable,"ratio"))%>%
  mutate(
    d_type = case_match(type, "pls" ~ "a", "cubist" ~ "b", "mbl" ~ "c"),
    d_set = case_match(
      set,
      "spc_rs4"        ~ "A",
      "spc_sg_rs4"     ~ "B",
      "spc_sg_bl_rs4"  ~ "C",
      "spc_sg_snv_rs4" ~ "D",
      "spc_sg1d_rs4"   ~ "E",
      "spc_sg2d_rs4"   ~ "F"
    )
  ) %>%
  distinct(d_set, d_type) %>%
  mutate(group = paste0(d_set, d_type))

# Step 2: Merge with your original `outlier_data`
outlier_data <- plot_groups %>%
  left_join(
    tibble(x = c("A","D","E","F"),
                   y = rep(320,4),
                   d_set=c("A","D","E","F"),
                   d_type=c("a","a","b","a"),
                   group=paste0(d_set,d_type),
                   label=c("> 300\n**",rep("> 300\n*",3))),
    by = c("d_set" = "x", "d_type", "group")) %>%
  mutate(
    y = ifelse(is.na(y), 320, y),         # same y position for all
    label = ifelse(is.na(label), "", label)
  )


eval_zeitreihe%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggplot(aes(x=d_set,group=group,y=relrmse,fill=d_type))+
  geom_boxplot(outliers = F,notch = T,alpha=.1,position = position_dodge(width = .8))+
  geom_jitter(shape=4,alpha=.5,size=.5,aes(col=d_type,group=group),
              position = position_jitterdodge(dodge.width = .8,jitter.width = 0.2))+
  scale_y_continuous("Rel. RMSE [%]",limits=c(30,330))+
  scale_fill_manual("",breaks=c("a","b","c"),labels=c("PLS","Cubist","MBL"),values = colorblind_safe_colors()[2:4])+
  scale_color_manual("",breaks=c("a","b","c"),labels=c("PLS","Cubist","MBL"),values = colorblind_safe_colors()[2:4])+
  scale_x_discrete("",breaks=c("A","B","C","D","E","F"),labels=c("spc_rs4","spc_sg_rs4","spc_sg_bl_rs4","spc_sg_snv_rs4","spc_sg1d_rs4","spc_sg2d_rs4"))+
  geom_hline(yintercept = 100)+
  theme_pubclean()+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  geom_text(inherit.aes = F,
            data=outlier_data,
            aes(x = d_set,
                y = y,
                group=group,
                label=label,
                col=d_type),position = position_dodge(width = .8),
            show.legend = F)->plt_preprocessing_relrmse_zr

plt_preprocessing_relrmse_zr

ggsave(plot=plt_preprocessing_relrmse_zr+theme(legend.position = "none"),filename="preprocessingRMSEzr.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
# legend
ggsave(plot=get_legend(plt_preprocessing_relrmse_zr),filename="preprocessingRMSEzr_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

### EDA versions with ggstatsplot ####

#### test set ####
##### set ####
all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggbetweenstats(x=d_set,y=relrmse,,type="np",
                         # ggplot.component = list(
                         #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                         # )
  )%>%
  ggsave(filename="EDApreprocessingRMSEtest_set.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)


##### type ####
all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggbetweenstats(x=d_type,y=relrmse,,type="np",
                 # ggplot.component = list(
                 #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                 # )
  )%>%
  ggsave(filename="EDApreprocessingRMSEtest_type.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)

##### set + type ####
all_test_eval%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  grouped_ggbetweenstats(x=d_type,y=relrmse,
                         grouping.var = d_set,type="np",
                         # ggplot.component = list(
                         #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                         # )
                         )%>%
  ggsave(filename="EDApreprocessingRMSEtest.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)

#### zeitreihe ####
##### set #####
eval_zeitreihe%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggbetweenstats(x=d_set,y=relrmse,
                         type="np",
                         # ggplot.component = list(
                         #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                         # )
                         )%>%
  ggsave(filename="EDApreprocessingRMSEzr_set.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)



##### set #####
eval_zeitreihe%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  ggbetweenstats(x=d_type,y=relrmse,
                 type="np",
                 # ggplot.component = list(
                 #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                 # )
  )%>%
  ggsave(filename="EDApreprocessingRMSEzr_type.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)

##### set + type ####
eval_zeitreihe%>%filter(!str_detect(variable,"ratio"))%>%
  mutate(d_type=case_match(type,"pls"~"a","cubist"~"b","mbl"~"c"),
         d_set=case_match(set,
                          "spc_rs4" ~ "A",
                          "spc_sg_rs4" ~ "B",
                          "spc_sg_bl_rs4" ~"C",
                          "spc_sg_snv_rs4" ~ "D",
                          "spc_sg1d_rs4" ~ "E",
                          "spc_sg2d_rs4" ~ "F"),
         group=paste0(d_set,d_type))%>%
  group_by(variable)%>%
  mutate(mean_rmse=mean(rmse),
         relrmse=rmse/mean_rmse*100)%>%
  grouped_ggbetweenstats(x=d_type,y=relrmse,
                         grouping.var = d_set,type="np",
                         # ggplot.component = list(
                         #   ggplot2::ylim(0, 300)  # replace with your desired y-limits
                         # )
  )%>%
  ggsave(filename="EDApreprocessingRMSEzr.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 16,height = 12)



## GT300 model validation ####

### Load GT300 spc ####
if(F){gt300_spc=OPUSraw_to_Preprocessed(folder = paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/1_raw_scans/2_scans_GT300/"),
                        save_location = paste0(data_dir,"/Sean_Environment/BDF_dataset/spc/gt300"),
                        save = T,
                        save_raw = T,
                        return = T,
                        reload=F
                        )
}else{
#reload
  gt300_spc=readRDS(paste0(data_dir,"/Sean_Environment/BDF_dataset/spc/gt300/spc_data"))
  }
# !!! NOTE: spc_range is slightly different for non-rs4 (i.e. bl 7489...411 vs 7490...410)

gt300_data=left_join(gt300_spc,all_data,by=c("sample_id"="LabelEvent"),suffix = c(".gt300",".mm400"))%>%
  rename(LabelEvent=sample_id)

### plt spc ####

i="spc_sg_snv_rs4.gt300"
gt300_data%>%select(all_of(c("LabelEvent",i)))%>%
  mutate(across(all_of(i),as_tibble))%>%
  unnest(cols=all_of(i))%>%
  pivot_longer(cols=colnames(gt300_data[[i]]))%>%
  mutate(name=as.numeric(name))%>%
  summarise_metrics(grouping_variables = "name",
                    variables = "value")%>%
  mutate(mill="GT300")->gt300_spc_summary

i="spc_sg_snv_rs4.mm400"
gt300_data%>%select(all_of(c("LabelEvent",i)))%>%
  mutate(across(all_of(i),as_tibble))%>%
  unnest(cols=all_of(i))%>%
  pivot_longer(cols=colnames(gt300_data[[i]]))%>%
  mutate(name=as.numeric(name))%>%
  summarise_metrics(grouping_variables = "name",
                    variables = "value")%>%
  mutate(mill="MM400")->mm400_spc_summary


mm400_spc_summary%>%filter(name>=425&name<=7475)%>% #crop ends
  ggplot(aes(x=name,col=mill,fill=mill))+
  
  geom_ribbon(aes(ymin=min,ymax=max),alpha=.1,linetype="dotted")+
  geom_ribbon(aes(ymin=q25,ymax=q75),alpha=.2,linetype="dashed")+
  geom_line(aes(y=median))+
  
  geom_ribbon(data=gt300_spc_summary%>%filter(name>=425&name<=7475),
              aes(ymin=min,ymax=max),alpha=.1,linetype="dotted")+
  geom_ribbon(data=gt300_spc_summary%>%filter(name>=425&name<=7475),
              aes(ymin=q25,ymax=q75),alpha=.2,linetype="dashed")+
  geom_line(data=gt300_spc_summary%>%filter(name>=425&name<=7475),
            aes(y=median))+
  
  
  #geom_line(aes(y=mean))+
  #ggtitle(i_title)+
  ylab("Absorbance")+
  scale_x_log10(expression("Wavenumber [c"*m^-1*"]"))+
  scale_color_manual("Used mill",breaks = c("GT300","MM400"),values=colorblind_safe_colors()[c(3,7)])+
  scale_fill_manual("Used mill",breaks = c("GT300","MM400"),values=colorblind_safe_colors()[c(3,7)])+
  theme_pubr()+
  theme(#axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())->spc_gt300_mm400



ggsave(plot=spc_gt300_mm400+theme(legend.position = "none"),filename="spc_gt300_mm400.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 4)
# legend
ggsave(plot=get_legend(spc_gt300_mm400),filename="spc_gt300_mm400_leg.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)



### pca ####
#### using full pca
gt300_pca=predict(pc,gt300_data$spc_sg_snv_rs4.gt300)

pc_combined=bind_rows(
  tibble(LabelEvent=gt300_data$LabelEvent,
         mill="GT300",
         gt300_data%>%select(contains("["),pH_CaCl2),
         gt300_pca$scores%>%as_tibble),
  left_join(
    tibble(LabelEvent=gt300_data$LabelEvent,
           mill="MM400",
         gt300_data%>%select(contains("["),pH_CaCl2)),
         pc_merge%>%select(LabelEvent,contains("Comp"))
    )
  )



pc_combined%>%
  ggplot(aes(x = `Comp 1`,y = `Comp 2`,col=mill))+
  geom_point()+
  geom_line(aes(group=LabelEvent))

pc_combined%>%
  ggplot(aes(x = `Comp 3`,y = `Comp 4`,col=mill))+
  geom_point()+
  geom_line(aes(group=LabelEvent))




#### using subset only (resemble) ####

  
pc_mill <- pc_projection(
  Xr = gt300_data$spc_sg_snv_rs4.mm400,
  Xu = gt300_data$spc_sg_snv_rs4.gt300,
  pc_selection = list("cumvar", .99),
  method = "pca",
  center = TRUE,
  scale = FALSE
)





### models ####
GT300_eval=readRDS("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/Sean_Environment/R_main/models/2024 models/GT300_testing/all_evaluation")


### redo obspred plots ####

GT300_val_plotter=function(Obs_pred_eval,
                           type_="pls",
                           set_="spc_sg_snv_rs4",
                           trans_="log1p",
                           variable_="TOC"){
  
  mm400_data=Obs_pred_eval$Obs_Pred[[paste0(type_,"_",set_,"-",trans_,"-",variable_)]]$test
  gt300_data=Obs_pred_eval$Obs_Pred[[paste0(type_,"_",set_,"-",trans_,"-",variable_)]]$gt300
  eval=GT300_eval$evaluation%>%filter(type==type_,
                                            set==set_,
                                            trans==trans_,
                                            variable==variable_)
  #print(eval) debug
  
  mm400_label=paste0("RMSE: ",signif(eval$test.rmse,3),"\n",
                     "R2: ",signif(eval$test.R2,3),"\n",
                     "RPD: ",signif(eval$test.rpd,3)
                     )
                     
  gt300_label=paste0("RMSE: ",signif(eval$gt300.rmse,3),"\t\t\n",
                     "R2: ",signif(eval$gt300.R2,3),"\t\t\n",
                     "RPD: ",signif(eval$gt300.rpd,3),"\t\t"
  )
  
  limits=c(min(mm400_data,gt300_data),max(mm400_data,gt300_data))
  
  ggplot(mapping=aes(x=obs,y=pred))+
    geom_abline(slope = 1,linetype="dotted")+
    geom_point(data = mm400_data,shape=21)+
    geom_point(data = gt300_data,shape=4,col="red3")+
    scale_x_continuous("Observed",limits=limits)+
    scale_y_continuous("Predicted",limits=limits)+
    coord_fixed()+
    annotate(geom="label",
             x = limits[1], 
             y = limits[2], 
             label = mm400_label, 
             hjust = 0, vjust = 1)+
    
    annotate(geom="label",
             x = limits[2], 
             y = limits[1], 
             label = gt300_label,
             col="red3",
             hjust = "inward", vjust = 0)+
    ggtitle(paste0(type_,"_",set_,"-",trans_,"-",variable_))+
    theme_pubr()+
    theme(title = element_text(size=10))
  
}

for (i in names(GT300_eval$plots)){

  GT300_eval$plots[[i]]=GT300_val_plotter(GT300_eval,
                                          type=str_split_fixed(i,"_",2)[1],
                                          set=str_split_fixed(str_split_fixed(i,"_",2)[2],"-",2)[1],
                                          trans=str_split_fixed(i,"-",3)[2],
                                          variable=str_split_fixed(i,"-",3)[3]
                                          )
}

### obspred plots ####
best_test.rmse=GT300_eval$evaluation%>%filter(variable%in%names(variable_lookup))%>%
  group_by(trans)%>%filter(test.rmse==min(test.rmse))%>%transmute(mod=paste0(type,"_",set,"-",trans,"-",variable))%>%pull(mod)
ggarrange(plotlist = GT300_eval$plots[best_test.rmse])%>%
  ggsave(filename="mm400_gt300_obspred.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 20,height = 20)



ggarrange(plotlist=GT300_eval$plots[c("pls_spc_sg_snv_rs4-log1p-TOC",
                                      "pls_spc_sg_snv_rs4-log1p-T",
                                      "pls_spc_sg_snv_rs4-log1p-Fe_t",
                                      
                                      "cubist_spc_sg_snv_rs4-log1p-TOC",
                                      "cubist_spc_sg_snv_rs4-log1p-T",
                                      "cubist_spc_sg_snv_rs4-log1p-Fe_t",
                                      
                                      "mbl_spc_sg_snv_rs4-log1p-TOC",
                                      "mbl_spc_sg_snv_rs4-log1p-T",
                                      "mbl_spc_sg_snv_rs4-log1p-Fe_t",
                                      
                                      "svm_spc_sg_snv_rs4-log1p-TOC",
                                      "svm_spc_sg_snv_rs4-log1p-Fe_t",
                                      "svm_spc_sg_snv_rs4-log1p-T"
                                      )],
          ncol=3,nrow=4)%>%
  ggsave(filename="mm400_gt300_obspred_select.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 12,height = 16)



#### generalised ####
##### lin ccc ####
var_="linsCCC"

GT300_eval$evaluation%>%mutate(test.nrmseavg=test.rmse/test.mean*100,gt300.nrmseavg=gt300.rmse/gt300.mean*100)%>%
  pivot_longer(cols = c(paste("test",var_,sep="."),paste("gt300",var_,sep=".")),values_to = "value",names_to = "ds")%>%
  mutate(ds=str_remove(ds,pattern = paste0(".",var_))%>%
           factor(levels = c("test","gt300"),labels=c(1,2)),
         type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
  # fair comparison with main initial models
  filter(variable%in%names(variable_lookup))%>%
  ggplot()+
  # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
  geom_rect(data=tibble(
    ymin=c(-1,.65,.8,.9,.95,.99),
    ymax=c(.65,.8,.9,.95,.99,1),
    group=c("vp","p","f","g","vg","e"),
  ),
  aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
  
  geom_boxplot(aes(x=fct_reorder(type,value,.desc = T),
                   y=value,
                   group=paste(type,ds),
                   fill=ds
  )
  )+
  #geom_point()
  theme_pubr()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  scale_x_manual(values=c(1:4),labels = c("PLS","Cubist","MBL","SVM"))+
  scale_y_continuous("Lin's CCC",expand = c(0,0))+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual("Quality",breaks=c(#"a","b","c",
    c("vp","p","f","g","vg","e"),1,2),#"test","gt300"),
    labels=c(#"PLS","Cubist","MBL",
      "Very Poor (< 0.65)" ,"Poor  (0.65 - 0.80)","Fair (0.80 - 0.90)","Good (0.90 0.95)", "Very Good (0.95 - 0.99)","Excellent (> 0.99)"
      ,"MM400","GT300"),
    values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
      unlist(bg_colors),setNames(colorblind_safe_colors()[c(7,3)],c(1,2))))+#"test","gt300"))))+
  theme(legend.position = "right",
        panel.grid.major.y = element_line(linetype="dotted",colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1))->mm400_gt300_lin_30var


ggsave(plot=mm400_gt300_lin_30var
       # +theme(legend.position = "none")
       ,filename="mm400_gt300_lin_30var.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
# legend
ggsave(plot=get_legend(mm400_gt300_lin_30var),filename="mm400_gt300_lin_30var_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

#all variables lin
{
GT300_eval$evaluation%>%mutate(test.nrmseavg=test.rmse/test.mean*100,gt300.nrmseavg=gt300.rmse/gt300.mean*100)%>%
  pivot_longer(cols = c(paste("test",var_,sep="."),paste("gt300",var_,sep=".")),values_to = "value",names_to = "ds")%>%
  mutate(ds=str_remove(ds,pattern = paste0(".",var_))%>%
           factor(levels = c("test","gt300"),labels=c(1,2)),
         type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
  # fair comparison with main initial models
  #filter(variable%in%names(variable_lookup))%>%
  ggplot()+
  # Wadoux and Minasny, 2024, McBride 2005 (combining thresholds)
  geom_rect(data=tibble(
    ymin=c(-1,.65,.8,.9,.95,.99),
    ymax=c(.65,.8,.9,.95,.99,1),
    group=c("vp","p","f","g","vg","e"),
  ),
  aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
  
  geom_boxplot(aes(x=fct_reorder(type,value,.desc = T),
                   y=value,
                   group=paste(type,ds),
                   fill=ds
  )
  )+
  #geom_point()
  theme_pubr()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  scale_x_manual(values=c(1:4),labels = c("PLS","Cubist","MBL","SVM"))+
  scale_y_continuous("Lin's CCC")+
    scale_fill_manual("Quality",breaks=c(#"a","b","c",
      c("vp","p","f","g","vg","e"),1,2),#"test","gt300"),
      labels=c(#"PLS","Cubist","MBL",
        "Very Poor (< 0.65)" ,"Poor  (0.65 - 0.80)","Fair (0.80 - 0.90)","Good (0.90 0.95)", "Very Good (0.95 - 0.99)","Excellent (> 0.99)"
        ,"MM400","GT300"),
      values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
        unlist(bg_colors),setNames(colorblind_safe_colors()[c(7,3)],c(1,2))))+#"test","gt300"))))+
    theme(legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(linetype="dotted",colour = "black"),
          axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1))->mm400_gt300_lin_allvar
}

##### rpd ####
var_="rpd"

GT300_eval$evaluation%>%mutate(test.nrmseavg=test.rmse/test.mean*100,gt300.nrmseavg=gt300.rmse/gt300.mean*100)%>%
  pivot_longer(cols = c(paste("test",var_,sep="."),paste("gt300",var_,sep=".")),values_to = "value",names_to = "ds")%>%
  mutate(ds=str_remove(ds,pattern = paste0(".",var_))%>%
           factor(levels = c("test","gt300"),labels=c(1,2)),
         type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
  # fair comparison with main initial models
  filter(variable%in%names(variable_lookup))%>%
  ggplot()+
  # williams 2014 (though some authors are far less rigourous with rpd>2 as very good)
  geom_rect(data=tibble(
    ymin=c(-Inf,2,2.5,3,3.5,4),
    ymax=c(2,2.5,3,3.5,4,Inf),
    group=c("vp","p","f","g","vg","e"),
  ),
  aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
  
  geom_boxplot(aes(x=fct_reorder(type,value,.desc = T),
                   y=value,
                   group=paste(type,ds),
                   fill=ds
  )
  )+
  #geom_point()
  theme_pubr()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  scale_x_manual(values=c(1:4),labels = c("PLS","Cubist","MBL","SVM"))+
  scale_y_continuous("RPD",expand = c(0,0))+
  coord_cartesian(ylim = c(0,7.8))+
  scale_fill_manual("Quality",breaks=c(#"a","b","c",
    c("vp","p","f","g","vg","e"),1,2),#"test","gt300"),
    labels=c(#"PLS","Cubist","MBL",
      "Very Poor (< 2.0)" ,"Poor (2.0 - 2.5)","Fair (2.5 - 3.0)","Good (3.0 - 3-5)", "Very Good (3.5 - 4.0)","Excellent (> 4.0)"
      ,"MM400","GT300"),
    values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
      unlist(bg_colors),setNames(colorblind_safe_colors()[c(7,3)],c(1,2))))+#"test","gt300"))))+
  theme(legend.position = "right",
        panel.grid.major.y =  element_line(linetype="dotted",colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1))->mm400_gt300_rpd_30var


ggsave(plot=mm400_gt300_rpd_30var
       # +theme(legend.position = "none")
       ,filename="mm400_gt300_rpd_30var.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
# legend
ggsave(plot=get_legend(mm400_gt300_rpd_30var),filename="mm400_gt300_rpd_30var_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)



# nrmse ####



##### rpd ####
var_="nrmseavg"

GT300_eval$evaluation%>%mutate(test.nrmseavg=test.rmse/test.mean*100,gt300.nrmseavg=gt300.rmse/gt300.mean*100)%>%
  pivot_longer(cols = c(paste("test",var_,sep="."),paste("gt300",var_,sep=".")),values_to = "value",names_to = "ds")%>%
  mutate(ds=str_remove(ds,pattern = paste0(".",var_))%>%
           factor(levels = c("test","gt300"),labels=c(1,2)),
         type=factor(type,levels = c("pls","cubist","mbl","svm"),labels=c(1:4)))%>%
  # fair comparison with main initial models
  filter(variable%in%names(variable_lookup))%>%
  ggplot()+
  # williams 2014 (though some authors are far less rigourous with rpd>2 as very good)
  geom_rect(data=tibble(
    ymin=c(Inf,100,50,25,10,5),
    ymax=c(100,50,25,10,5,0),
    group=c("vp","p","f","g","vg","e"),
  ),
  aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax,fill=group),alpha=.25)+
  
  geom_boxplot(aes(x=fct_reorder(type,value,.desc = T),
                   y=value,
                   group=paste(type,ds),
                   fill=ds
  )
  )+
  #geom_point()
  theme_pubr()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  scale_x_manual(values=c(1:4),labels = c("PLS","Cubist","MBL","SVM"))+
  scale_y_log10("NRMSEavg",expand = c(0,0))+
  #coord_cartesian(ylim = c(0,7.8))+
  scale_fill_manual("Quality",breaks=c(#"a","b","c",
    c("vp","p","f","g","vg","e"),1,2),#"test","gt300"),
    labels=c(#"PLS","Cubist","MBL",
      "Very Poor (< 2.0)" ,"Poor (2.0 - 2.5)","Fair (2.5 - 3.0)","Good (3.0 - 3-5)", "Very Good (3.5 - 4.0)","Excellent (> 4.0)"
      ,"MM400","GT300"),
    values = c(#setNames(colorblind_safe_colors()[2:4],c("a","b","c")),
      unlist(bg_colors),setNames(colorblind_safe_colors()[c(7,3)],c(1,2))))+#"test","gt300"))))+
  theme(legend.position = "right",
        panel.grid.major.y =  element_line(linetype="dotted",colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1))->mm400_gt300_nrmse_30var


ggsave(plot=mm400_gt300_nrmse_30var
       # +theme(legend.position = "none")
       ,filename="mm400_gt300_nrmse_30var.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
# legend
ggsave(plot=get_legend(mm400_gt300_nrmse_30var),filename="mm400_gt300_nrmse_30var_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)


### stat table comparison ####


#all
GT300_eval$evaluation%>%filter(variable%in%names(variable_lookup))%>%
  #group_by(type)%>%
  summarise(
    p_rpd=wilcox.test(test.rpd,gt300.rpd,paired=T)[["p.value"]],
    p_rmse=wilcox.test(test.rmse,gt300.rmse,paired=T)[["p.value"]],
    p_lin=wilcox.test(test.linsCCC,gt300.linsCCC,paired=T)[["p.value"]],
    n=length(test.linsCCC)
  )%>%view


#for model types
GT300_eval$evaluation%>%filter(variable%in%names(variable_lookup))%>%
  group_by(type)%>%
  summarise(
    p_rpd=wilcox.test(test.rpd,gt300.rpd,paired=T)[["p.value"]]%>%signif(2),
    p_rmse=wilcox.test(test.rmse,gt300.rmse,paired=T)[["p.value"]]%>%signif(2),
    p_lin=wilcox.test(test.linsCCC,gt300.linsCCC,paired=T)[["p.value"]]%>%signif(2),
    n=length(test.linsCCC)
  )%>%view



# for each variable
GT300_eval$evaluation%>%
  group_by(variable)%>%
  summarise(
    p_rpd=wilcox.test(test.rpd,gt300.rpd,paired=T)[["p.value"]]%>%signif(2),
    p_rmse=wilcox.test(test.rmse,gt300.rmse,paired=T)[["p.value"]]%>%signif(2),
    p_lin=wilcox.test(test.linsCCC,gt300.linsCCC,paired=T)[["p.value"]]%>%signif(2),
    n=length(test.linsCCC),
    diff=(gt300.rmse-test.rmse)/test.rmse
  )%>%filter(variable%in%names(variable_lookup))%>%view



## Models - train size ####

### dummy for schematic split vis
#### df ####

##### Tibble 1: repeat_splits ####

n_train <- 423

repeat_splits <- tibble(x = c(1:605))

for (i in 1:5) {
  train_ids <- naes(all_archive%>%filter(!str_detect(Campaign,"3")&!is.na(`TOC [wt-%]`))%>%pull(spc_sg_snv_rs4),k = 423)[["model"]]
  repeat_splits[[paste0("repeat", i)]] <- ifelse(x1 %in% train_ids, "train", "test")
}

# Convert to character columns
repeat_splits <- repeat_splits %>% mutate(across(-x, as.character))



##### Tibble 2: subset_sizes ####

subset_sizes <- tibble(x = 1:423)

for (size in seq(30, 420, by = 10)) {
  train_ids <- naes(all_archive%>%filter(!str_detect(Campaign,"3")&!is.na(`TOC [wt-%]`))%>%filter(repeat_splits$repeat1=="train")%>%pull(spc_sg_snv_rs4),k = size)[["model"]]
  subset_sizes[[paste0("size", size)]] <- ifelse(x2 %in% train_ids, "train", "not_used")
  print(i)
}

# Convert to character columns
subset_sizes <- subset_sizes %>% mutate(across(-x, as.character))



#### plot ####
##### repeats ####
repeat_long <- repeat_splits %>%
  pivot_longer(cols = starts_with("repeat"), names_to = "Repeat", values_to = "Set")

# Factor to order the repeats
repeat_long$Repeat <- factor(repeat_long$Repeat, levels = paste0("repeat", 1:5))

# Assign numeric y-position for repeats (for horizontal stacking)
repeat_long <- repeat_long %>%
  mutate(y = as.numeric(Repeat))

# Plot using geom_tile for horizontal bar style
ggplot(repeat_long, aes(x = x, y = y, fill = Set)) +
  geom_tile(height = .8,alpha=.7) +
  scale_y_continuous(
    breaks = 1:5,
    labels = paste0(1:5),
    trans = "reverse"  # Optional: reverse to have repeat1 at top
  ) +
  scale_fill_manual("Split",values = c("train" = colorblind_safe_colors()[7], "test" = colorblind_safe_colors()[3]),labels=c("train"="Training","test"="Testing")) +
  labs(
    x = "Index",
    y = "Repeat",
    fill = "Set Type"
  ) + 
  coord_cartesian(expand = F)+
  theme(panel.grid = element_blank(),
        legend.position = "top")->repeat_split


ggsave(plot=repeat_split,filename = "train_size_repeat_split.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=2,width=6,device="png")

##### subsets ####


# Reshape to long format
subset_long <- subset_sizes %>%
  pivot_longer(cols = starts_with("size"), names_to = "Size", values_to = "Set")

# Factor to order size labels properly
subset_long$Size <- factor(subset_long$Size, levels = paste0("size", seq(30, 420, by = 10)))

# Assign numeric y-position for visual separation
subset_long <- subset_long %>%
  mutate(y = as.numeric(Size))

# Plot using geom_tile
ggplot(subset_long, aes(x = x, y = y, fill = Set)) +
  geom_tile(height = 0.8,alpha=.7) +
  scale_y_continuous(
    breaks = seq_along(levels(subset_long$Size)),
    labels = levels(subset_long$Size)%>%str_remove("size"),
    trans = "reverse"  # Optional: largest training size on bottom
  ) +
  scale_fill_manual("Split",values = c("train" = colorblind_safe_colors()[7], "not_used" = "lightgray"),labels=c("train"="Training","not_used"="Not used")) +
  labs(
    x = "Index",
    y = "Training Size",
    fill = "Set Type"
  ) +
  theme_minimal() +
  coord_cartesian(expand = F)+
  theme(panel.grid = element_blank(),
        legend.position = "top")->subset_split


ggsave(plot=subset_split,filename = "train_size_subset_split.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=5,width=6,device="png")


# fetch eval for TOC, diff model types,
#' using DE-2024-BDF_archive + DE-2024-BDF_BfUL for training
# eval with test set

out_test=c()
for (i in c(1:5)){
  for (mod_type in c("cubist_train_size_",
                     "svmLin_train_size_",
                     "pls_train_size_",
                     "rf_train_size_")
  ){
    run_i=paste0(mod_type,i)
    message(paste0("#######################################################################\n",
                   "#######################################################################\n",
                   "#######################################################################\n",
                   run_i))  
    out_test=bind_rows(out_test,(readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/train_size/validation/Valtest_",run_i))))
  }
}

# eval with core library
out=c()
for (i in c(1:5)){
  for (mod_type in c("cubist_train_size_",
                     "svmLin_train_size_",
                     "pls_train_size_",
                     "rf_train_size_")
  ){
    run_i=paste0(mod_type,i)
    message(paste0("#######################################################################\n",
                   "#######################################################################\n",
                   "#######################################################################\n",
                   run_i))  
    out=bind_rows(out,(readRDS(paste0(data_dir,"Sean_Environment/R_main/models/train_size/validation/Valmain_",run_i))))
  }
}


### Results ####

####test eval plot  ####
out_test%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(model,size)%>%summarise(median.rmse=median(rmse,na.rm=T),
                                   min.rmse=min(rmse,na.rm=T),
                                   max.rmse=max(rmse,na.rm=T),
                                   
                                   median.rpd=median(rpd,na.rm=T),
                                   min.rpd=min(rpd,na.rm=T),
                                   max.rpd=max(rpd,na.rm=T),
                                   
                                   median.linsCCC=median(linsCCC,na.rm=T),
                                   min.linsCCC=min(linsCCC,na.rm=T),
                                   max.linsCCC=max(linsCCC,na.rm=T),
                                   
                                   median.time=median(time/tuning_grid_size,na.rm=T),
                                   min.time=min(time/tuning_grid_size,na.rm=T),
                                   max.time=max(time/tuning_grid_size,na.rm=T),
                                   
                                   median.total.time=median(time,na.rm=T),
                                   min.total.time=min(time,na.rm=T),
                                   max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  geom_hline(yintercept = c(
    seq(.1,.7,.1),
    log10(c(.1,.3,1,3,10,30,100,300))/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2))
  ),
  col="grey",linetype="dotted")+
  
  geom_hline(yintercept = .0)+
  
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rmse,
                group=model))+
  
  
  geom_errorbar(aes(ymin=log10(min.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
                    ymax=log10(max.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
                    y=log10(median.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=log10(median.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
                group=model))+
  scale_y_continuous("RMSE [wt-% TOC]",
                     breaks=seq(0.1,.7,.1),
                     sec.axis = sec_axis("Time per tuning-grid element [sec]",
                                         transform=~10**((.+.2+(((log10(300))/10-.2)-((log10(100))/10-.2)))*10),
                                         breaks=c(.1,.3,1,3,10,30,100)))+
  coord_cartesian(ylim = c(-.35,.75))+
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  xlab("Traingsset-size")+
  scale_linetype_discrete("",breaks=c("rmse","time"),labels=c("RMSEP","Time"))+
  theme_pubr()+
  theme(axis.title.y.left = element_text(hjust=.725),
        axis.title.y.right = element_text(hjust=1),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95))->plt_train_size_test

plt_train_size_test


ggsave(plot=plt_train_size_test,filename = "train_size_test.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=8,width=6,device="png")





#### DE-2023-BDF_archive eval plot  ####
# eval with DE-2023-BDF_archive

out%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(model,size)%>%summarise(median.rmse=median(rmse,na.rm=T),
                                   min.rmse=min(rmse,na.rm=T),
                                   max.rmse=max(rmse,na.rm=T),
                                   median.time=median(time/tuning_grid_size,na.rm=T),
                                   min.time=min(time/tuning_grid_size,na.rm=T),
                                   max.time=max(time/tuning_grid_size,na.rm=T),
                                   median.total.time=median(time,na.rm=T),
                                   min.total.time=min(time,na.rm=T),
                                   max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  geom_hline(yintercept = c(
    seq(.3,.8,.1),
    log10(c(.1,.3,1,3,10,30,100,300))/10-(((log10(300))/10+.0)-((log10(100))/10+.0))
  ),
  col="grey",linetype="dotted")+
  
  geom_hline(yintercept = .2)+
  
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rmse,
                group=model))+
  geom_errorbar(aes(ymin=log10(min.time)/10-(((log10(300))/10+.0)-((log10(100))/10+.0)),
                    ymax=log10(max.time)/10-(((log10(300))/10+.0)-((log10(100))/10+.0)),
                    y=log10(median.time)/10-(((log10(300))/10+.0)-((log10(100))/10+.0)),
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=log10(median.time)/10-(((log10(300))/10+.0)-((log10(100))/10+.0)),
                group=model))+
  scale_y_continuous("RMSE [wt-% TOC]",
                     breaks=seq(0.3,.8,.1),
                     sec.axis = sec_axis("Time per tuning-grid element [sec]",
                                         transform=~10**((.+(((log10(300))/10+.0)-((log10(100))/10+.0)))*10),
                                         breaks=c(.1,.3,1,3,10,30,100)))+
  coord_cartesian(ylim = c(-.15,.85))+
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  xlab("Traingsset-size")+
  scale_linetype_discrete("",breaks=c("rmse","time"),labels=c("RMSEP","Time"))+
  theme_pubr()+
  theme(axis.title.y.left = element_text(hjust=.725),
        axis.title.y.right = element_text(hjust=1),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95),
        legend.text = element_text(size=12))+
  guides(color=guide_legend(override.aes = list(size=5)))->plt_train_size_main


plt_train_size_main


ggsave(plot=plt_train_size_main,filename = "train_size.png",
       path ="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=8,width=6,device="png")


#out_comp=
out%>%select(model_type,size,time,n,rmse)%>%
  left_join(out_test%>%select(model_type,size,time,n,rmse),
            by=c("model_type","size","time"),suffix = c(".main",".test"))%>%  
  mutate(model=str_split_fixed(model_type,"_",2)[,1],rmse.diff=(rmse.main-rmse.test)/rmse.test)%>%#pull(model)
  group_by(model,size)%>%summarise(median.rmse=median(rmse.diff,na.rm=T),
                                   min.rmse=min(rmse.diff,na.rm=T),
                                   max.rmse=max(rmse.diff,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rmse,
                group=model))




#### DE-2023-BDF_archive eval plot TOTAL TIME  ####
# eval with DE-2023-BDF_archive

out=c()
for (i in c(1:5)){
  for (mod_type in c("cubist_train_size_",
                     "svmLin_train_size_",
                     "pls_train_size_",
                     "rf_train_size_")
  ){
    run_i=paste0(mod_type,i)
    message(paste0("#######################################################################\n",
                   "#######################################################################\n",
                   "#######################################################################\n",
                   run_i))  
    out=bind_rows(out,(readRDS(paste0(data_dir,"Sean_Environment/R_main/models/train_size/validation/Valmain_",run_i))))
  }
}




out%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(model,size)%>%summarise(median.rmse=median(rmse,na.rm=T),
                                   min.rmse=min(rmse,na.rm=T),
                                   max.rmse=max(rmse,na.rm=T),
                                   median.time=median(time/tuning_grid_size,na.rm=T),
                                   min.time=min(time/tuning_grid_size,na.rm=T),
                                   max.time=max(time/tuning_grid_size,na.rm=T),
                                   median.total.time=median(time,na.rm=T),
                                   min.total.time=min(time,na.rm=T),
                                   max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  geom_hline(yintercept = c(
    seq(.3,.8,.1),
    log10(c(3,10,30,100,300,1000,3000))/10-(.2)
  ),
  col="grey",linetype="dotted")+
  
  geom_hline(yintercept = .2)+
  
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rmse,
                group=model))+
  
  
  geom_errorbar(aes(ymin=log10(min.total.time)/10-.2,
                    ymax=log10(max.total.time)/10-.2,
                    y=log10(median.total.time)/10-.2,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=log10(median.total.time)/10-(.2),
                group=model))+
  
  
  scale_y_continuous("RMSE [wt-% TOC]",
                     breaks=seq(0.3,.8,.1),
                     sec.axis = sec_axis("Total training time [sec]",
                                         transform=~10**((.+.2)*10),
                                         breaks=c(3,10,30,100,300,1000,3000)))+
  coord_cartesian(ylim = c(-.15,.85))+
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  xlab("Traingsset-size")+
  scale_linetype_discrete("",breaks=c("rmse","time"),labels=c("RMSEP","Time"))+
  theme_pubr()+
  theme(axis.title.y.left = element_text(hjust=.725),
        axis.title.y.right = element_text(hjust=.9),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95),
        legend.text = element_text(size=12))+
  guides(color=guide_legend(override.aes = list(size=5)))->plt_train_size_main_tot_time


plt_train_size_main_tot_time


ggsave(plot=plt_train_size_main_tot_time,filename = "train_size_tot_time.png",
       path ="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=8,width=6,device="png")


#out_comp=
out%>%select(model_type,size,time,n,rmse)%>%
  left_join(out_test%>%select(model_type,size,time,n,rmse),
            by=c("model_type","size","time"),suffix = c(".main",".test"))%>%  
  mutate(model=str_split_fixed(model_type,"_",2)[,1],rmse.diff=(rmse.main-rmse.test)/rmse.test)%>%#pull(model)
  group_by(model,size)%>%summarise(median.rmse=median(rmse.diff,na.rm=T),
                                   min.rmse=min(rmse.diff,na.rm=T),
                                   max.rmse=max(rmse.diff,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rmse,
                group=model))


#### all (multimetric plots) ####
##### test ####



bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%pivot_longer(cols = c(rmse,
                                                                              rpd,
                                                                              R2,
                                                                              #linsCCC,
                                                                              bias))%>%
  group_by(dataset,model,size,name)%>%summarise(
    across(.cols = value,.fns = list(mean=mean,min=min,max=max,median=median)))%>%
  filter(!dataset=="DE-2023-BDF_archive")%>%
  
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=value_min,
                    ymax=value_max,
                    y=value_median,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=value_median,
                group=model))+
  
  geom_hline(data=tibble(name="bias"),aes(yintercept = 0))+
  
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(panel.grid.major.y = element_line(linetype = "dotted",color="grey"),
        #legend.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.5,.97),panel.border = element_rect(colour = "black",fill=rgb(0,0,0,0))
  )+
  xlab("Size of Training-Set")+
  ylab("")+
  # scale_y_continuous("R2 [wt-% TOC]",breaks=seq(0,1,.1))+
  # facet_wrap(~name,scales="free_y")#+
  facet_grid(rows=vars(factor(name,levels = c("rmse","bias","rpd","R2","linsCCC"),labels=c("RMSE","Bias","RPD","R2","Lin's CCC"))),
             cols=vars(factor(dataset,levels=c("Test sets","DE-2023-BDF_archive"))),
             scales="free_y")->train_size_all_val_test
train_size_all_val_test
ggsave(train_size_all_val,filename = "train_size_all_val_test.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=12,width=8,device="png")



##### core ####

bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%pivot_longer(cols = c(rmse,
                                                                              rpd,
                                                                              R2,
                                                                              #linsCCC,
                                                                              bias))%>%
  group_by(dataset,model,size,name)%>%summarise(
    across(.cols = value,.fns = list(mean=mean,min=min,max=max,median=median)))%>%
  filter(dataset=="DE-2023-BDF_archive")%>%
  
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=value_min,
                    ymax=value_max,
                    y=value_median,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=value_median,
                group=model))+
  
  geom_hline(data=tibble(name="bias"),aes(yintercept = 0))+
  
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(panel.grid.major.y = element_line(linetype = "dotted",color="grey"),
        #legend.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.5,.97),panel.border = element_rect(colour = "black",fill=rgb(0,0,0,0))
  )+
  xlab("Size of Training-Set")+
  ylab("")+
  # scale_y_continuous("R2 [wt-% TOC]",breaks=seq(0,1,.1))+
  # facet_wrap(~name,scales="free_y")#+
  facet_grid(rows=vars(factor(name,levels = c("rmse","bias","rpd","R2","linsCCC"),labels=c("RMSE","Bias","RPD","R2","Lin's CCC"))),
             cols=vars(factor(dataset,levels=c("Test sets","DE-2023-BDF_archive"))),
             scales="free_y")->train_size_all_val_core
train_size_all_val_core
ggsave(train_size_all_val,filename = "train_size_all_val_core.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=12,width=8,device="png")


##### all ####

bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%pivot_longer(cols = c(rmse,
                                                                              rpd,
                                                                              R2,
                                                                              #linsCCC,
                                                                              bias))%>%
  group_by(dataset,model,size,name)%>%summarise(
    across(.cols = value,.fns = list(mean=mean,min=min,max=max,median=median)))%>%
  #filter(dataset=="DE-2023-BDF_archive")%>%
  
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=value_min,
                    ymax=value_max,
                    y=value_median,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=value_median,
                group=model))+
  
  geom_hline(data=tibble(name="bias"),aes(yintercept = 0))+
  
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(panel.grid.major.y = element_line(linetype = "dotted",color="grey"),
        #legend.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.25,.97),panel.border = element_rect(colour = "black",fill=rgb(0,0,0,0))
  )+
  xlab("Size of Training-Set")+
  ylab("")+
  # scale_y_continuous("R2 [wt-% TOC]",breaks=seq(0,1,.1))+
  # facet_wrap(~name,scales="free_y")#+
  facet_grid(rows=vars(factor(name,levels = c("rmse","bias","rpd","R2","linsCCC"),labels=c("RMSE","Bias","RPD","R2","Lin's CCC"))),
             cols=vars(factor(dataset,levels=c("Test sets","DE-2023-BDF_archive"))),
             scales="free_y")->train_size_all_val
train_size_all_val
ggsave(train_size_all_val,filename = "train_size_all_val.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=10,width=8,device="png")



##### stat table ####


bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%pivot_longer(cols = c(rmse,
                                                                              rpd,
                                                                              R2,
                                                                              #linsCCC,
                                                                              bias))%>%
  group_by(dataset,model,size,name)%>%
  summarise(across(value,.fns = list(min=min,max=max,mean=mean,median=median)))%>%view


##### rmse ratio acc. to Ng etal 2020
bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1],
         rep=str_split_fixed(model_type,"_",4)[,4])%>%pivot_longer(cols = c(rmse,
                                                                            rpd,
                                                                            R2,
                                                                            #linsCCC,
                                                                            bias))%>%
  select(dataset,name,rep,value,model,size)%>%
  pivot_wider(names_from = c(model,name),values_from = value)%>%
  filter(dataset=="DE-2023-BDF_archive")%>%
  mutate(pls_svm=pls_rmse/svmLin_rmse,
         pls_cubist=pls_rmse/cubist_rmse,
         pls_rf=pls_rmse/rf_rmse,
         cubist_svm=cubist_rmse/svmLin_rmse,
         cubist_rf=cubist_rmse/rf_rmse,
         rf_svm=rf_rmse/svmLin_rmse)%>%
  pivot_longer(cols=c(pls_svm,pls_cubist,pls_rf,cubist_svm,cubist_rf,rf_svm))%>%
  ggplot(aes(x=size,y=value,group=size))+
  geom_hline(yintercept = 1)+
  geom_boxplot()+
  facet_wrap(~name)




#### pred for MM400 / GT300 subsets ####

# all_pred=gt300_data%>%select(!contains("spc")) #exclude spc to save space
all_pred_train_mm400gt300=read_rds(paste0(data_dir,"/Sean_Environment/R_main/model_out/all.mm400.gt300.pred_train.size.models"))
if(F){
require(progress)
pb=progress::progress_bar$new(total=800,format = "[:bar] :percent (:current/:total) | :elapsed (:elapsedfull) | ETA: :eta" )
for (model_type_i in c("pls","cubist","svm","rf")){
  for (repeat_i in str_subset(list.dirs(paste0(data_dir,"/Sean_Environment/R_main/models/train_size/"),full.names = T),
                              model_type_i)){
    for (i in list.files(repeat_i,full.names = T)){
      mm400_i=paste0("mm400_r",str_sub(repeat_i,str_length(repeat_i)),"_",basename(i))
      gt300_i=paste0("gt300_r",str_sub(repeat_i,str_length(repeat_i)),"_",basename(i))
      pb$tick()
      if(!(mm400_i%in%names(all_pred_train_mm400gt300)&gt300_i%in%names(all_pred_train_mm400gt300))){
        mod_i=readRDS(i)
        all_pred_train_mm400gt300[[mm400_i]]=predict(mod_i,gt300_data$spc_sg_snv_rs4.mm400)
        all_pred_train_mm400gt300[[gt300_i]]=predict(mod_i,gt300_data$spc_sg_snv_rs4.gt300)
      }
      
    }
    
  }
  
}

saveRDS(all_pred_train_mm400gt300,paste0(data_dir,"/Sean_Environment/R_main/model_out/all.mm400.gt300.pred_train.size.models"))

}

eval_train_mm400gt300=c()
for (i in names(select(all_pred_train_mm400gt300,contains("mm400")|contains("gt300")))){
  eval_train_mm400gt300=
    bind_rows(
      eval_train_mm400gt300,
      tibble(id=i,
             evaluate_model_adjusted(all_pred_train_mm400gt300,obs="TOC [wt-%]",pred=i)
      )
    )
  print(i)
}
eval_train_mm400gt300=mutate(eval_train_mm400gt300,
          dataset=str_split_fixed(id,"_",2)[,1],
          rep=str_remove(str_split_fixed(id,"_",3)[,2],"r"),
          model=str_split_fixed(id,"_",4)[,3],
          size=str_split_fixed(id,"_TOC_",2)[,2]
          )%>%mutate(model=str_remove(model,"ear"),
                     size=as.numeric(size),
                     dataset=case_match(dataset,
                                        "gt300"~"DE-2023-BDF_archive GT300",
                                        "mm400"~"DE-2023-BDF_archive MM400"))


eval_train_mm400gt300%>%pivot_longer(cols = c(rmse,
                                              rpd,
                                              R2,
                                              #linsCCC,
                                              bias))%>%
  group_by(dataset,model,size,name)%>%summarise(
    across(.cols = value,.fns = list(mean=mean,min=min,max=max,median=median)))%>%
  #filter(!dataset=="DE-2023-BDF_archive")%>%
  
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=value_min,
                    ymax=value_max,
                    y=value_median,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=value_median,
                group=model))+
  
  geom_hline(data=tibble(name="bias"),aes(yintercept = 0))+
  
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(panel.grid.major.y = element_line(linetype = "dotted",color="grey"),
        #legend.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.25,.97),panel.border = element_rect(colour = "black",fill=rgb(0,0,0,0))
  )+
  xlab("Size of Training-Set")+
  ylab("")+
  # scale_y_continuous("R2 [wt-% TOC]",breaks=seq(0,1,.1))+
  # facet_wrap(~name,scales="free_y")#+
  facet_grid(rows=vars(factor(name,levels = c("rmse","bias","rpd","R2","linsCCC"),labels=c("RMSE","Bias","RPD","R2","Lin's CCC"))),
             cols=vars(factor(dataset,levels=c("DE-2023-BDF_archive GT300","DE-2023-BDF_archive MM400"))),
             scales="free_y")->train_size_mm400_gt300
train_size_mm400_gt300
ggsave(train_size_mm400_gt300,filename = "train_size_mm400_gt300.png",
       path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
       height=10,width=8,device="png")


all_pred_train_mm400gt300%>%
  select(`TOC [wt-%]`,contains("mm400")|contains("gt300"))%>%
  pivot_longer(cols = contains("mm400")|contains("gt300"),names_to = "id")%>%
  mutate(
    dataset=str_split_fixed(id,"_",2)[,1],
    rep=str_remove(str_split_fixed(id,"_",3)[,2],"r"),
    model=str_split_fixed(id,"_",4)[,3],
    size=str_split_fixed(id,"_TOC_",2)[,2])%>%
  mutate(model=str_remove(model,"ear"),
           size=as.numeric(size))%>%
  select(-id)%>%
  pivot_wider(names_from = rep,values_from = value)%>%
  rowwise%>%
  mutate(meanpred=mean(c(`1`,`2`,`3`,`4`,`5`)))%>%
  ggplot(aes(x=`TOC [wt-%]`,y=meanpred,col=size))+
  geom_point()+
  geom_abline(slope = 1)+
  facet_nested_wrap(~model)+
  scale_color_viridis_c(alpha = .1)

# END ####  
