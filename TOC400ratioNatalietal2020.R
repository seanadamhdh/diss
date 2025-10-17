# Set seed for reproducibility
set.seed(42)

# Generate x values from 0.1 to 100 (log scale)
x_vals <- tibble(TOC=runif(500,0,10),ROC_frac=(sqrt(.2+rnorm(500,0.1,sd = .3)^2)%>%ifelse(.>1,1,.)))%>%
  mutate(ROC_ratio=ROC_frac/(1-ROC_frac),TOC400_ratio=(1-ROC_frac)/ROC_frac)
log_xroc <- log10(x_vals$ROC_ratio)
log_xtoc <- log10(x_vals$TOC400_ratio)

# Generate δ13C_TOC values (‰), decreasing trend with added noise
d13C_vals <- approx(range(log_x1), c(-31, -15), xout = log_x)$y + rnorm(length(log_x), mean = 0, sd = 2) +runif(50,-2.5,2.5)

# Create data frame
df <- data.frame(
  x_vals,
  log10_TOC400_ROC = log_x,
  d13C_TOC = -45-d13C_vals
)

# View first few rows
head(df)

# Optional: Plot
plot(log10(df$TOC400_ratio), df$d13C_TOC,
     xlab = "log10(TOC400/ROC)",
     ylab = expression(delta^13*C[Toc]~("\u2030")),
     main = "Synthetic δ13C vs log10(TOC400/ROC)",
     pch = 19, col = "darkblue")
abline(lm(d13C_TOC ~ TOC400_ratio, data = df), col = "red", lwd = 2)


ggplot(df,aes(x=TOC400_ratio,y=d13C_TOC))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_log10()










##############################

#' COMPARE WITH Natali etal 2020

BDF_SSL%>%mutate(ROCratio=`ROC [wt-%]`/`TOC [wt-%]`,TOC400ratio=`TOC400 [wt-%]`/`ROC [wt-%]`,soil=if_else((Depth_top+Depth_bottom)/2>.5,"s","t"))->bdf_ratios
  ggplot(aes(x=TOC400ratio))+geom_boxplot()+facet_grid(col=vars(`Land use`),rows=vars(if_else(Depth_top+Depth_bottom>1,"s","t")))




############################

#' compare soil reference measurements Elementar



bind_rows(
  c(set="elementar",soil="par",TOC400=.361,ROC=.299,TIC=3.28),
  c(set="elementar",soil="vul",TOC400=12.2,ROC=.5,TIC=.05),
  c(set="elementar",soil="kal",TOC400=1.48,ROC=.32,TIC=.4),
  c(set="elementar",soil="sch",TOC400=1.82,ROC=3.99,TIC=2.95)
)%>%
  mutate(TOC400=as.numeric(TOC400),ROC=as.numeric(ROC),TIC=as.numeric(TIC))%>%
  mutate(ROCratio = ROC / (TOC400 + ROC),
         TOC400ratio=TOC400/ROC)->elementar_ratios
  
bind_rows(
  c(set="Natali20",soil="top",stat="low",TOC400=.86,ROC=.28,TIC900=.13,TOC400ratio=2.5,ROCratio=.21),
  c(set="Natali20",soil="top",stat="high",TOC400=1.94,ROC=.72,TIC900=2.25,TOC400ratio=3.7,ROCratio=.29),
  c(set="Natali20",soil="sub",stat="low",TOC400=.12,ROC=.12,TIC900=1.14,TOC400ratio=.9,ROCratio=.28),
  c(set="Natali20",soil="sub",stat="high",TOC400=1.13,ROC=1.14,TIC900=2.36,TOC400ratio=2.6,ROCratio=.53)
)%>%
  mutate(TOC400=as.numeric(TOC400),ROC=as.numeric(ROC),TIC900=as.numeric(TIC900),TOC400ratio=as.numeric(TOC400ratio),ROCratio=as.numeric(ROCratio))%>%
  pivot_wider(names_from = stat,values_from = c(TOC400,ROC,TIC900,ROCratio,TOC400ratio))->natali20_ratios

#TOC400ratio
bdf_ratios%>%
  ggplot()+
  geom_rect(data = natali20_ratios,aes(xmin=TOC400ratio_low,xmax=TOC400ratio_high,ymin=-.5,ymax=.5,fill=soil),alpha=.2)+
  geom_vline(data=elementar_ratios,aes(xintercept=TOC400ratio,col=soil))+
  geom_boxplot(aes(x=TOC400ratio))+
  facet_wrap(~if_else((Depth_top+Depth_bottom)/2>.3,"s","t"))+
  coord_cartesian(xlim=c(0,35),ylim=c(-.5,.5),expand = F)+
  ggthemes::scale_color_colorblind()+
  ggthemes::scale_fill_colorblind()+
  scale_x_sqrt()
  DIN_mat <- read_excel("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/DIN19539 reference materials.xlsx")
  

#ROCratio
bdf_ratios%>%
  ggplot()+
  geom_rect(data = natali20_ratios,aes(xmin=ROCratio_low*100,xmax=ROCratio_high*100,ymin=-.5,ymax=.5,fill=soil),alpha=.2)+
  
  geom_vline(data=elementar_ratios,aes(xintercept=ROCratio*100,col=soil,linetype=soil),linewidth=1)+

  geom_density(aes(x=ROCratio*100,fill=soil),alpha=.2)+
  
  geom_vline(data=bdf_ratios%>%group_by(soil)%>%summarise(ROCratio=mean(ROCratio*100,na.rm=T)),
             aes(xintercept =ROCratio,col=soil,linetype=soil),linewidth=1)+

  geom_vline(data=DIN_mat%>%
               mutate(ROCratio=ROC/(TOC400+ROC)*100,
                      soil=case_match(`Matrix`,
                                      "Schlacke"~"sch2",
                                      "Baggerschlamm, Sediment"~"bag",
                                      "Klärschlamm, beaufschlagter Boden"~"kla",
                                      "Sandboden"~"san",
                                      "Auenboden"~"aue",
                                      "Bodenaushub"~"bod",
                                      .default = "else"
                      )),aes(xintercept=ROCratio,col=soil,linetype=soil),
             linewidth=1)+
  
  geom_label(data=elementar_ratios%>%mutate(
    label=case_match(soil,
             "kal"~"Elementar Limestone marsh",
             "par"~"Elementar Loess soil",
             "vul"~"Elementar Vulcanic ash soil",
             "sch"~"Elementar Blast furnace slag")),
             aes(x=ROCratio*100,y=0,label=label,fill=soil,vjust=c(1,1,1,0)),angle=90,hjust=0,alpha=.2)+
  
  geom_label(data=DIN_mat%>%
               mutate(ROCratio=ROC/(TOC400+ROC)*100,
                      soil=case_match(`Matrix`,
                                      "Schlacke"~"sch2",
                                      "Baggerschlamm, Sediment"~"bag",
                                      "Klärschlamm, beaufschlagter Boden"~"kla",
                                      "Sandboden"~"san",
                                      "Auenboden"~"aue",
                                      "Bodenaushub"~"bod",
                                      .default = "else"
                      ))%>%
               filter(soil!="else"),
             aes(x=ROCratio,y=.25,label=paste("DIN",`EN-Matrix`),fill=soil,vjust=c(1,1,0,1,1,1)),hjust=1,angle=90,alpha=.2)+
  
  
  geom_label(data=bdf_ratios%>%group_by(soil)%>%
               summarise(ROCratio=mean(ROCratio*100,na.rm=T))%>%
               mutate(label=if_else(soil=="t","BDF Topsoil (<=30 cm)","BDF Subsoil (>30 cm)")),
             aes(x=ROCratio,y=0,label=label,fill=soil,vjust=c(1,1)),hjust=0,angle=90,alpha=.2)+
  
  geom_label(data=natali20_ratios%>%mutate(label=if_else(soil=="top","Natali et al. 2020 Topsoil","Natali et al. 2020 Subsoil")),
             aes(x=(ROCratio_low*50+ROCratio_high*50),y=.125,label=label,fill=soil),hjust=.5,angle=90,alpha=.2)+
  
  #facet_wrap(~if_else((Depth_top+Depth_bottom)/2>.3,"s","t"))+
  coord_cartesian(xlim=c(0,70),ylim=c(0,.25),expand = F)+
  scale_color_manual("",breaks=c("t",
                                 "s",
                                 
                                 "kal",
                                 "par",
                                 "vul",
                                 "san",
                                 "aue",
                                 "bod",
                                 
                                 "bag",
                                 "kla",
                                 
                                 "sch",
                                 "sch2",
                              
                                 "else"
                                 ),
                     values=c(colorblind_safe_colors()[c(
                                                       1,
                                                       2,
                                                       
                                                       3,
                                                       3,
                                                       3,
                                                       3,
                                                       3,
                                                       3,
                                                       
                                                       4,
                                                       4,
                                                       
                                                       5,
                                                       5
                                                      
                                                       )],rgb(0,0,0,0)),
                     labels=c("BDF Topsoil (Avg. Depth <= 30 cm)",
                              "BDF Subsoil (Avg. Depth > 30 cm)",
                              "Elementar Limestone marsh",
                              "Elementar Loess soil",
                              "Elementar Vulcanic ash soil",
                              "DIN Sandy soil",
                              "DIN Alluvial soil",
                              "DIN Excavated soil material",
                              
                              "DIN Dredged sediment",
                              "DIN Sewage sludge",
                              
                              "DIN Blast furnace slag",
                              "Elementar Blast furnace slag",
                              
                              ""
                     ))+
  scale_linetype_manual("",
                        breaks=c("t",
                                 "s",
                                 
                                 "kal",
                                 "par",
                                 "vul",
                                 "san",
                                 "aue",
                                 "bod",
                                 
                                 "bag",
                                 "kla",
                                 
                                 "sch",
                                 "sch2",
                                 
                                 "else"
  ),
  values=c(
    1,
    1,
    
    1,
    2,
    3,
    4,
    5,
    6,
    
    1,
    2,
    
    1,
    2,
  
    1),
  labels=c("BDF Topsoil (Avg. Depth <= 30 cm)",
           "BDF Subsoil (Avg. Depth > 30 cm)",
           "Elementar Limestone marsh",
           "Elementar Loess soil",
           "Elementar Vulcanic ash soil",
           "DIN Sandy soil",
           "DIN Alluvial soil",
           "DIN Excavated soil material",
           
           "DIN Dredged sediment",
           "DIN Sewage sludge",
           
           "DIN Blast furnace slag",
           "DIN Elementar Blast furnace slag",
           
           ""
  ))+
  scale_fill_manual("",breaks=c("t",
                                 "s",
                                 
                                 "kal",
                                 "par",
                                 "vul",
                                 "san",
                                 "aue",
                                 "bod",
                                 
                                 "bag",
                                 "kla",
                                 
                                 "sch",
                                 "sch2",
                                 
                                 "top",
                                 "sub",
                                
                                
                                 "else"
  ),
  values=c(colorblind_safe_colors()[c(
    1,
    2,
    
    3,
    3,
    3,
    3,
    3,
    3,
    
    4,
    4,
    
    5,
    5,
    
    7,
    8,
    
    1
    
  )],rgb(0,0,0,0)),
  labels=c("BDF Topsoil (Avg. Depth <= 30 cm)",
           "BDF Subsoil (Avg. Depth > 30 cm)",
           "Elementar Limestone marsh",
           "Elementar Loess soil",
           "Elementar Vulcanic ash soil",
           "DIN Sandy soil",
           "DIN Alluvial soil",
           "DIN Excavated soil material",
           
           "DIN Dredged sediment",
           "DIN Sewage sludge",
           
           "DIN Blast furnace slag",
           "Elementar Blast furnace slag",
           
           "Natali2020 Topsoil",
           "Natali2020 Subsoil",
           
           ""
  ))+
  xlab("ROCratio [%]")+
  ylab("Density")+
  theme_pubr()+
  theme(legend.position = "none")->ROCratio_literature
ROCratio_literature
ggsave(plot=ROCratio_literature,filename="ROCratioLiterature.png",
       path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =10,height = 4)
#

  # table ####
bind_rows(
 bdf_ratios%>%transmute(set="BDF",
                       soil=soil,
                       TOC400=`TOC400 [wt-%]`,
                       ROC=`ROC [wt-%]`,
                       TIC900=`TIC900 [wt-%]`,
                       TOC=`TOC [wt-%]`,
                       TC=`TC [wt-%]`,
                       TOC400ratio=TOC400ratio,
                       ROCratio=ROCratio)%>%
  group_by(set,soil)%>%
  summarise(across(where(~is.numeric(.x)),
                   .fns = c(mean=~mean(.,na.rm=T),min=~min(.,na.rm=T),max=~max(.,na.rm=T)))),
  
  elementar_ratios%>%mutate(TOC_mean=TOC400+ROC,TC_mean=TOC_mean+TIC)%>%
    rename(TOC400_mean=TOC400,
           ROC_mean=ROC,
           TIC900_mean=TIC,
           TOC400ratio_mean=TOC400ratio,
           ROCratio_mean=ROCratio),
  
  natali20_ratios%>%
    rename_with(~str_replace(.x,"high","max"))%>%
    rename_with(~str_replace(.x,"low","min"))

)%>%
  transmute(set,soil,ROCratio_mean*100,ROCratio_min*100,ROCratio_max*100)%>%view

 
# plot ratio tic corr ####

BDF_SSL%>%filter(`ROC [wt-%]`!=0)%>%transmute(ROC=`ROC [wt-%]`,ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100,TIC900=`TIC900 [wt-%]`,set="BDF",soil=Campaign,cat=.2,shape="a")%>%
  bind_rows(
  
    elementar_ratios%>%transmute(set,soil,ROC=ROC,ROCratio=ROCratio*100,TIC900=TIC,cat=1,shape="b"),
  
      natali20_ratios%>%
        rename_with(~str_replace(.x,"high","max"))%>%
        rename_with(~str_replace(.x,"low","min"))%>%
      transmute(set="Natali2020",
                ROC=(ROC_min+ROC_max)/2,
                ROCratio=(ROCratio_min+ROCratio_max)/2*100,
                TIC900=(TIC900_min+TIC900_max)/2,
                cat=1,shape="b")
    )%>%
  
  bind_rows(
            DIN_mat%>%transmute(ROC=ROC,
                                ROCratio=ROC/(ROC+TOC400)*100,
                                TIC900,
                                set=Quelle,
                                soil=Matrix,
                                cat=1,shape="b")
            )%>%
    ggplot(aes(x=TIC900,y=ROC))+
  geom_abline(slope = 1)+
    geom_point(aes(shape=shape,col=set,size=cat,alpha=2*cat),stroke=2)+
  
  geom_smooth(method="glm",aes(col=set),linewidth=1,se=F)+
  scale_color_manual("",values=colorblind_safe_colors()[c(2,3,4,8)]) + 
  scale_shape_manual("",values = c(16,4))+
  scale_x_log10()+
  scale_y_log10()+
  theme_pubr()+
  theme(legend.position = "right")

ggsave(plot=ROCratio_literature,filename="ROCratioLiterature.png",
       path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =10,height = 4)

#ROC vs TIC literature ####
BDF_SSL%>%filter(`ROC [wt-%]`!=0)%>%transmute(TOC400=`TOC400 [wt-%]`,ROC=`ROC [wt-%]`,ROCratio=`ROC [wt-%]`/`TOC [wt-%]`*100,TIC900=`TIC900 [wt-%]`,set="BDF",soil=Campaign,cat=.2,shape="a")%>%
  bind_rows(
    
    elementar_ratios%>%transmute(set,soil,TOC400,ROC,ROCratio=ROCratio*100,TIC900=TIC,cat=1,shape="b"),
    
    natali20_ratios%>%
      rename_with(~str_replace(.x,"high","max"))%>%
      rename_with(~str_replace(.x,"low","min"))%>%
      transmute(set="Natali2020",
                TOC400=(TOC400_min+TOC400_max)/2,
                ROC=(ROC_min+ROC_max)/2,
                ROCratio=(ROCratio_min+ROCratio_max)/2*100,
                TIC900=(TIC900_min+TIC900_max)/2,
                cat=1,shape="b")
  )%>%
  
  bind_rows(
    DIN_mat%>%transmute(TOC400,
                        ROC,
                        ROCratio=ROC/(ROC+TOC400)*100,
                        TIC900,
                        set=Quelle,
                        soil=Matrix,
                        cat=1,shape="b")
  )%>%
  ggplot(aes(x=ROC,y=TOC400))+
  geom_abline(slope = 1)+
  geom_point(aes(shape=shape,col=set,size=cat,alpha=2*cat),stroke=2)+
  
  geom_smooth(method="glm",aes(col=set),linewidth=1,se=F)+
  scale_color_manual("",values=colorblind_safe_colors()[c(2,3,4,8)]) + 
  scale_shape_manual("",values = c(16,4))+
  scale_x_log10()+
  scale_y_log10()+
  theme_pubr()+
  theme(legend.position = "right")->p























