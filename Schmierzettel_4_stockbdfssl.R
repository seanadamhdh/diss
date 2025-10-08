
ggarrange(
  BDF_SSL%>%filter(LabelEvent%>%substr(2,2)%in%c("R","Q")&Depth_top>=0&Position!="Profil_4")%>%
    left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name"))%>%
    mutate(
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    mutate(TOC_budget_atroAll=case_when(
      Tiefe_bis<=30~TOC_budget_atroAll,
      Tiefe_bis>30&Tiefe_von<30~
        TOC_budget_atroAll/(Tiefe_bis-Tiefe_von)*(30-Tiefe_von),
      Tiefe_bis>30~0
    ))%>%
    #filter(Tiefe_bis<=30)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(
      TOC_sum_atroAll=sum(TOC_budget_atroAll)
    )%>%
    group_by(BDF,Typ)%>%
    summarise(
      TOC_sum_atroAll_mean=mean(TOC_sum_atroAll),
      TOC_sum_atroAll_max=max(TOC_sum_atroAll),
      TOC_sum_atroAll_min=min(TOC_sum_atroAll),
      TOC_sum_atroAll_n=length(TOC_sum_atroAll)
    )%>%
    # reference data 
    rbind(data.frame(
      (Lagerungsdichte_Referenz_neu%>%
         left_join(select(TRD_neu%>%filter(!is.na(BDF)),
                          all_of(c("BDF-Fläche"="BDF","Horizont","soliTOC"))),
                   by=c("BDF-Fläche","Horizont","Tiefe von", "Tiefe bis"))%>%
         mutate(TOC_budget=soliTOC$TOC/100 #gC/g
                *`TRD g/cm3`# gC/cm3
                *1000#kg/m³
                *10#t/ha*m
                *(abs(Hz_bis-Hz_von)/100))%>%
         #aggregating budget to maxDepth
         mutate(TOC_budget=case_when(
           Hz_bis<=30~TOC_budget,
           Hz_bis>30&Hz_von<30~
             TOC_budget/(Hz_bis-Hz_von)*(30-Hz_von),
           Hz_bis>30~0
         )
         )%>%
         
         group_by(BDF=`BDF-Fläche`,Position=rep(c(1:5),13))%>%
         summarise(TOC_budget_sum=sum(TOC_budget))%>%
         summarise(
           TOC_sum_atroAll_mean=mean(TOC_budget_sum),
           TOC_sum_atroAll_max=max(TOC_budget_sum),
           TOC_sum_atroAll_min=min(TOC_budget_sum),
           TOC_sum_atroAll_n=length(TOC_budget_sum)
         )),
      Typ=rep("Referenz neu",4)
    ),
    data.frame(
      BDF=c("02","23","30","35","23"),
      Typ=c(rep("zReferenz",4),"Quicksampler"),
      Position=rep(NA,5),
      # sums for top 30 cm
      TOC_sum_atroAll_mean=c(57.4,86.2,127.6,46.4,NA)
    ))%>%
    
    ggplot(aes(x=BDF,group=paste(Typ),y=TOC_sum_atroAll_mean))+
    geom_col(position = "dodge",col="black",aes(fill=Typ))+
    geom_errorbar(aes(ymin=TOC_sum_atroAll_min,ymax=TOC_sum_atroAll_max),position = position_dodge(width=.9),width=.5)+
    geom_text(angle=90,aes(x=BDF,group=Typ,y=10,label=ifelse(is.na(TOC_sum_atroAll_n),"",paste0("n=",TOC_sum_atroAll_n))),hjust = .5,vjust=.5,
              position=position_dodge(width=.925),size = unit(4,"pt"))+
    theme_pubr()+
    scale_fill_manual(breaks=c("Quicksampler","Rammkern","Referenz neu","zReferenz"),values=colorblind_safe_colors[c(2:4,7)],
                      labels=c("Quicksampler","Push core","Soil rings","Reference"))+
    scale_y_continuous(expression("TOC [T C "*ha^-1*"]"),limits = c(0,220))+
    xlab("BDF")+
    ggtitle(label=paste0("0-",30," cm"),subtitle="dB105,ges"),
  
  
  BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0&Tiefe_von>=0&Position!="Profil_4")%>%
    left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name"))%>%
    mutate(
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    mutate(TOC_budget_atroAll=case_when(
      Tiefe_bis<=50~TOC_budget_atroAll,
      Tiefe_bis>50&Tiefe_von<50~
        TOC_budget_atroAll/(Tiefe_bis-Tiefe_von)*(50-Tiefe_von),
      Tiefe_bis>50~0
    ))%>%
    #filter(Tiefe_bis<=maxDepth)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(
      TOC_sum_atroAll=sum(TOC_budget_atroAll)
    )%>%
    group_by(BDF,Typ)%>%
    summarise(
      TOC_sum_atroAll_mean=mean(TOC_sum_atroAll),
      TOC_sum_atroAll_max=max(TOC_sum_atroAll),
      TOC_sum_atroAll_min=min(TOC_sum_atroAll),
      TOC_sum_atroAll_n=length(TOC_sum_atroAll)
    )%>%
    # reference data 
    rbind(data.frame(
      (Lagerungsdichte_Referenz_neu%>%
         left_join(select(TRD_neu%>%filter(!is.na(BDF)),
                          all_of(c("BDF-Fläche"="BDF","Horizont","soliTOC"))),
                   by=c("BDF-Fläche","Horizont","Tiefe von", "Tiefe bis"))%>%
         mutate(TOC_budget=soliTOC$TOC/100 #gC/g
                *`TRD g/cm3`# gC/cm3
                *1000#kg/m³
                *10#t/ha*m
                *(abs(Hz_bis-Hz_von)/100))%>%
         #aggregating budget to maxDepth
         mutate(TOC_budget=case_when(
           Hz_bis<=50~TOC_budget,
           Hz_bis>50&Hz_von<50~
             TOC_budget/(Hz_bis-Hz_von)*(50-Hz_von),
           Hz_bis>50~0
         )
         )%>%
         
         group_by(BDF=`BDF-Fläche`,Position=rep(c(1:5),13))%>%
         summarise(TOC_budget_sum=sum(TOC_budget))%>%
         summarise(
           TOC_sum_atroAll_mean=mean(TOC_budget_sum),
           TOC_sum_atroAll_max=max(TOC_budget_sum),
           TOC_sum_atroAll_min=min(TOC_budget_sum),
           TOC_sum_atroAll_n=length(TOC_budget_sum)
         )),
      Typ=rep("Referenz neu",4)
    ),
    data.frame(
      BDF=c("02","23","30","35","23"),
      Typ=c(rep("zReferenz",4),"Quicksampler"),
      Position=rep(NA,5),
      # sums for top 30 cm
      TOC_sum_atroAll_mean=c(57.4,86.2,127.6,46.4,NA)
    ))%>%
    
    ggplot(aes(x=BDF,group=paste(Typ),y=TOC_sum_atroAll_mean))+
    geom_col(position = "dodge",col="black",aes(fill=Typ))+
    geom_errorbar(aes(ymin=TOC_sum_atroAll_min,ymax=TOC_sum_atroAll_max),position = position_dodge(width=.9),width=.5)+
    geom_text(angle=90,aes(x=BDF,group=Typ,y=10,label=ifelse(is.na(TOC_sum_atroAll_n),"",paste0("n=",TOC_sum_atroAll_n))),hjust = .5,vjust=.5,
              position=position_dodge(width=.925),size = unit(4,"pt"))+
    theme_pubr()+
    scale_fill_manual(breaks=c("Quicksampler","Rammkern","Referenz neu","zReferenz"),values=colorblind_safe_colors[c(2:4,7)],
                      labels=c("Quicksampler","Push core","Soil rings","Reference"))+
    scale_y_continuous(expression("TOC [T C "*ha^-1*"]"),limits = c(0,220))+
    xlab("BDF")+
    ggtitle(label=paste0("0-",50," cm"),subtitle="dB105,ges"),
  common.legend = T
)#->TOC_budget_as_report    

