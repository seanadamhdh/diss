
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
    
  
  if(!require(ggh4x)){
    install.packages("ggh4x")
    require(ggh4x)
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
ggsave(plot=rel_dB_effect+theme(legend.position = "none"),filename="Relative_dB_effect.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =4,height = 4)
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
    `0-10 cm`=if_else(u3<=.1,1,0),
    `0-30 cm`=if_else(u3<=.3,1,0),
    `10-30 cm`=if_else(o3>=.1&u3<=.3,1,0),
    `0-50 cm`=if_else(u3<=.5,1,0),
    `30-50 cm`=if_else(o3>=.3&u3<=.5,1,0),
    `0-50 cm`=if_else(u3<=.5,1,0),
    `50+ cm`=if_else(u3>.5,1,0)
  )%>%pivot_longer(cols = c("0-10 cm","0-30 cm","10-30 cm","30-50 cm","0-50 cm","50+ cm"),
                   names_to = "set",
                   values_to = "inSet")%>%filter(inSet==1)->data_1cm

# n of interpolated segments (not the real n)
data_1cm%>%group_by(site_id,set)%>%summarise(n=n())->count_df_interpolated

# actual samples
data%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q","P")&Depth_bottom>0)%>%
  mutate(
    `0-10 cm`=if_else(Depth_bottom<=.1,1,0),
    `0-30 cm`=if_else(Depth_bottom<=.3,1,0),
    `10-30 cm`=if_else(Depth_top>=.1&Depth_bottom<=.3,1,0),
    `0-50 cm`=if_else(Depth_bottom<=.5,1,0),
    `30-50 cm`=if_else(Depth_top>=.3&Depth_bottom<=.5,1,0),
    `50+ cm`=if_else(Depth_bottom>.5,1,0)
  )%>%
  group_by(site_id)%>%
  summarise(across(c(`0-10 cm`,`0-30 cm`,`10-30 cm`,`30-50 cm`,`0-50 cm`,`50+ cm`),.fns = sum))%>%
  pivot_longer(cols =  c("0-10 cm","0-30 cm","10-30 cm","30-50 cm","0-50 cm","50+ cm"),
               names_to = "set",
               values_to = "n")->count_df


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



ggstatsplot::grouped_ggbetweenstats(data = data_1cm%>%filter(set%in%c("0-10 cm","10-30 cm", "30-50 cm")),x=site_id,y=`TOC [wt-%]`,grouping.var = set,type="np")#%>%




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

BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  transmute(x=as.numeric(max(DateEvent,na.rm = T)-DateEvent)/(365.25 * 24 * 60 * 60)
            ,y=`TOC [wt-%]`-`CORG  [wt-%]`)%>%
  ggplot(aes(x=x,y=y))+
  
  geom_bin2d(drop=F,binwidth=c(.5,.1))+
  scale_fill_gradient2("n",low = "white",midpoint = 3,
                       mid = colorblind_safe_colors()[6],
                       high=colorblind_safe_colors()[1],trans="log1p",na.value = 0,
                       breaks=c(0,1,2,5,10,20,50))+
  #geom_point(shape=4,size=.5)+
  geom_hline(yintercept = 0,col="black",linetype="dotted")+
  geom_smooth(method="glm",se=T,col="red",fill="red",alpha=.1,linetype="dashed")+
  xlab("Sample age [years]")+
  ylab("Difference in TOC to CORG [wt-%]")+
  theme(panel.background = element_rect(fill = "white"))+
  # geom_abline(data=lm(data=BDF_SSL%>%filter(Campaign=="DE-2023-BDF_archive")%>%
  #                transmute(x=(max(DateEvent,na.rm = T)-DateEvent)/(365.25 * 24 * 60 * 60)
  #                          ,y=`TOC [wt-%]`-`CORG  [wt-%]`),formula=y~x)%>%
  #               confint()%>%t%>%as_tibble(),
  #             aes(intercept = `(Intercept)`,slope=x))+
  theme_pubr()->age_diff
  
#plots
ggsave(plot=age_diff+theme(legend.position = "none"),filename="CORG_TOC_Age.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)
#legend
ggsave(plot=get_legend(age_diff),filename="CORG_TOC_age_legend.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 4,height = 4)

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



pc=pca(spc,ncomp = 50,method = "nipals")

#saveRDS(pc,paste0(code_dir,"GitLab/phd_code/R_main/temp/BDF_zeitreihe_spc_snv"))

pca_all_snv=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/pca_allBDF_sg-snv"))
pc$calres$scores%>%as_tibble()%>%cbind(BDF_Zeitreihe_spc)->pc_merge



pc_merge=BDF_Zeitreihe_spc
pc_merge=cbind(pc_merge,pc$calres$scores)
pc_merge_unnest=pc_merge%>%unnest(pc)

# correlating spectra derived pc with variables in BDF database + soliTOC
as_tibble(cor(select_if(pc_merge_unnest,.predicate = function(x){is.numeric(x)&tmp_fun(x)}),use="pairwise.complete.obs",method="spearman"))%>%select(`Comp 1`,`Comp 2`,`Comp 3`)%>%cbind(var=select_if(pc_merge_unnest,.predicate = function(x){is.numeric(x)&tmp_fun(x)})%>%names)->all_cor


left_join(all_prep_flag,tibble(sample_id=pc_merge$sample_id,pc=select(pc_merge,all_of(names(pc_merge)[which(str_detect(names(pc_merge),"Comp"))]))),by=c("Name"="sample_id"))->pc_merge_prep


pc_merge_unnest=unnest(pc_merge_prep,pc)

pc_corTable_data=select(pc_merge_unnest,
                        `Comp 1`,`Comp 2`,`Comp 3`,
                        Ct,CORG,TC,TOC,TOC400,ROC,TIC900,
                        Nt,Pt,K_t,Fe_t,Al_t,`T`,U,S)

png(paste0(code_dir,"GitLab/phd_code/R_main/temp/pca123_corrplot.png"),
    width=1000,height=1000)
corrplot::corrplot.mixed(cor(pc_corTable_data,use="pairwise.complete.obs",method = "spearman"),lower.col = "black")
dev.off()


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


### RELOAD ####

pls_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/pls_pred"))

cubist_pred=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/cubist_pred"))

mbl_frisch=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_frisch"))

mbl_zeitreihe=readRDS(paste0(data_dir,"/Sean_Environment/R_main/model_out/mbl_zeitreihe"))



all_pred=left_join(pls_pred,cubist_pred)%>%left_join(bind_rows(mbl_frisch,mbl_zeitreihe))
all_data=left_join(BDF_SSL,spc_tmp)%>%left_join(all_pred,by=c("LabelEvent"="ID"))



saveRDS(spc_tmp,paste0(data_dir,"/Sean_Environment/R_main/model_out/spc_tmp"))
saveRDS(all_data,paste0(data_dir,"/Sean_Environment/R_main/model_out/all_data"))


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
## processing effect ####


### all testing ####
pls_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/PLS_75_25_var_dependent_preProcess/evaluation"))
cubist_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Cubist_75-25-var-dependant_preProcess/evaluation"))
mbl_eval=readRDS(paste0(data_dir,"/Sean_Environment/R_main/models/2024 models/Mbl_models_better-split/evaluation"))

bind_rows(
tibble(type="pls",pls_eval$new_eval_table),
tibble(type="cubist",cubist_eval$new_eval_table),
tibble(type="mbl",mbl_eval$new_eval_table)
)->all_test_eval


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


# END ####  
