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



all_test_eval%>%filter(variable%>%str_detect("ratio",negate = T))%>%ggplot(aes(x=fct_reorder(variable,rpd,.desc = T),y=rpd,group=paste(variable,type),fill=type))+geom_boxplot()+theme_pubclean()+theme(axis.text.x = element_text(angle=90,hjust=1))








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




#### test eval plot  ####
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
                                   
                                   median.bias=median(bias,na.rm=T),
                                   min.bias=min(bias,na.rm=T),
                                   max.bias=max(bias,na.rm=T),
                                   
                                   
                                   median.time=median(time/tuning_grid_size,na.rm=T),
                                   min.time=min(time/tuning_grid_size,na.rm=T),
                                   max.time=max(time/tuning_grid_size,na.rm=T),
                                   
                                   median.total.time=median(time,na.rm=T),
                                   min.total.time=min(time,na.rm=T),
                                   max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  # geom_hline(yintercept = c(
  #   seq(.1,.7,.1),
  #   log10(c(.1,.3,1,3,10,30,100,300))/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2))
  # ),
  # col="grey",linetype="dotted")+
  # # 
   geom_hline(yintercept = .0)+
  # 
  geom_errorbar(aes(ymin=min.rpd,
                    ymax=max.rpd,
                    y=median.rpd,
                    group=model),
                linewidth=.25)+
  geom_line(aes(y=median.rpd,
                group=model))+
  geom_smooth(aes(y=median.rpd,
                  group=model),se=F)+
  # 
  # geom_errorbar(aes(ymin=log10(min.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
  #                   ymax=log10(max.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
  #                   y=log10(median.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
  #                   group=model),
  #               linewidth=.25)+
  # geom_line(aes(y=log10(median.time)/10-.2-(((log10(300))/10-.2)-((log10(100))/10-.2)),
  #               group=model))+
  # scale_y_continuous("RMSE [wt-% TOC]",
  #                    breaks=seq(0.1,.7,.1),
  #                    sec.axis = sec_axis("Time per tuning-grid element [sec]",
  #                                        transform=~10**((.+.2+(((log10(300))/10-.2)-((log10(100))/10-.2)))*10),
  #                                        breaks=c(.1,.3,1,3,10,30,100)))+
  # coord_cartesian(ylim = c(-.35,.75))+
   ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  # xlab("Traingsset-size")+
   scale_linetype_discrete("",breaks=c("rmse","time"),labels=c("RMSEP","Time"))+
  theme_pubr()+
  theme(axis.title.y.left = element_text(hjust=.725),
        axis.title.y.right = element_text(hjust=1),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95))#+scale_y_log10()

















##############################################################



### results ####


#### training times ####
out_test%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(model,size)%>%summarise(
    median.time=median(time/tuning_grid_size,na.rm=T),
    min.time=min(time/tuning_grid_size,na.rm=T),
    max.time=max(time/tuning_grid_size,na.rm=T),
    
    median.total.time=median(time,na.rm=T),
    min.total.time=min(time,na.rm=T),
    max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=min.time,
                    ymax=max.time,
                    y=median.time,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=median.time,
                group=model))+
  
  scale_y_log10("Time per tuning-grid element [sec]",
                breaks=c(.1,.3,1,3,10,30,100,300,1000,3000))+
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(axis.title.x = element_blank(),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95))->t->plt_train_time_per_elem

out_test%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(model,size)%>%summarise(
    median.time=median(time/tuning_grid_size,na.rm=T),
    min.time=min(time/tuning_grid_size,na.rm=T),
    max.time=max(time/tuning_grid_size,na.rm=T),
    
    median.total.time=median(time,na.rm=T),
    min.total.time=min(time,na.rm=T),
    max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=min.total.time,
                    ymax=max.total.time,
                    y=median.total.time,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=median.total.time,
                group=model))+
  
  scale_y_log10("Total training time [sec]",
                breaks=c(.1,.3,1,3,10,30,100,300,1000,3000))+
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(axis.title.x = element_blank(),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(.5,.95))->t->plt_train_time



ggpubr::annotate_figure(ggarrange(plt_train_time_per_elem,plt_train_time,common.legend = T),bottom ="Trainingsset-size")%>%
  
  
  ggsave(filename = "train_size_times.png",
         path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         height=6,width=8,device="png")




#### rmse eval plot  ####

bind_rows(tibble(out_test,dataset="Test sets"),
          tibble(out,dataset="DE-2023-BDF_archive"))%>%
  mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
  group_by(dataset,model,size)%>%summarise(
    median.rmse=median(rmse,na.rm=T),
    min.rmse=min(rmse,na.rm=T),
    max.rmse=max(rmse,na.rm=T),
    
    median.total.time=median(time,na.rm=T),
    min.total.time=min(time,na.rm=T),
    max.total.time=max(time,na.rm=T)
  )%>%
  ggplot(aes(x=size,col=model))+
  
  geom_errorbar(aes(ymin=min.rmse,
                    ymax=max.rmse,
                    y=median.rmse,
                    group=model),
                linewidth=.25)+
  
  geom_line(aes(y=median.rmse,
                group=model))+
  
  
  ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
  theme_pubr()+
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        #legend.position = "top",
        legend.direction = "horizontal",
        legend.position = "inside",
        legend.position.inside = c(.25,.9)
        )+
  xlab("Size of Training-Set")+
  scale_y_continuous("RMSE [wt-% TOC]",breaks=seq(0,1,.1))+
  facet_wrap(~factor(dataset,levels=c("Test sets","DE-2023-BDF_archive")))->plt_train_time_rmse


plt_train_time_rmse%>%
  
  ggsave(filename = "train_size_times_rmse.png",
         path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         height=6,width=8,device="png")



  
  
  #### bias eval plot  ####
  
  bind_rows(tibble(out_test,dataset="Test sets"),
            tibble(out,dataset="DE-2023-BDF_archive"))%>%
    mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
    group_by(dataset,model,size)%>%summarise(
      median.bias=median(bias,na.rm=T),
      min.bias=min(bias,na.rm=T),
      max.bias=max(bias,na.rm=T),
      
      median.total.time=median(time,na.rm=T),
      min.total.time=min(time,na.rm=T),
      max.total.time=max(time,na.rm=T)
    )%>%
    ggplot(aes(x=size,col=model))+
    
    geom_hline(yintercept = 0)+
    
    geom_errorbar(aes(ymin=min.bias,
                      ymax=max.bias,
                      y=median.bias,
                      group=model),
                  linewidth=.25)+
    
    geom_line(aes(y=median.bias,
                  group=model))+
    
    
    ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
    theme_pubr()+
    theme(panel.grid.major.y = element_line(linetype = "dotted"),
          #legend.position = "top",
          legend.direction = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(.25,.1)
    )+
    xlab("Size of Training-Set")+
    scale_y_continuous("Bias [wt-% TOC]",breaks=seq(-1,.5,.1))+
    facet_wrap(~factor(dataset,levels=c("Test sets","DE-2023-BDF_archive")))->plt_train_time_bias
  
  
  plt_train_time_bias%>%
  
  ggsave(filename = "train_size_times_bias.png",
         path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         height=6,width=8,device="png")
  
  
  
  
  
  
  #### rpd eval plot  ####
  
  bind_rows(tibble(out_test,dataset="Test sets"),
            tibble(out,dataset="DE-2023-BDF_archive"))%>%
    mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
    group_by(dataset,model,size)%>%summarise(
      median.rpd=median(rpd,na.rm=T),
      min.rpd=min(rpd,na.rm=T),
      max.rpd=max(rpd,na.rm=T),
      
      median.total.time=median(time,na.rm=T),
      min.total.time=min(time,na.rm=T),
      max.total.time=max(time,na.rm=T)
    )%>%
    ggplot(aes(x=size,col=model))+
    
    geom_errorbar(aes(ymin=min.rpd,
                      ymax=max.rpd,
                      y=median.rpd,
                      group=model),
                  linewidth=.25)+
    
    geom_line(aes(y=median.rpd,
                  group=model))+
    
    
    ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
    theme_pubr()+
    theme(panel.grid.major.y = element_line(linetype = "dotted"),
          #legend.position = "top",
          legend.direction = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(.75,.9)
    )+
    xlab("Size of Training-Set")+
    scale_y_continuous("RPD [wt-% TOC]",breaks=seq(0,20,1))+
    facet_wrap(~factor(dataset,levels=c("Test sets","DE-2023-BDF_archive")))->plt_train_time_rpd
  
  
  plt_train_time_rpd%>%
  
  ggsave(filename = "train_size_times_rpd.png",
         path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         height=6,width=8,device="png")
  
  
  
  
  
  
  
  
  #### R2 eval plot  ####
  
  bind_rows(tibble(out_test,dataset="Test sets"),
            tibble(out,dataset="DE-2023-BDF_archive"))%>%
    mutate(model=str_split_fixed(model_type,"_",2)[,1])%>%#pull(model)
    group_by(dataset,model,size)%>%summarise(
      median.R2=median(R2,na.rm=T),
      min.R2=min(R2,na.rm=T),
      max.R2=max(R2,na.rm=T),
      
      median.total.time=median(time,na.rm=T),
      min.total.time=min(time,na.rm=T),
      max.total.time=max(time,na.rm=T)
    )%>%
    ggplot(aes(x=size,col=model))+
    
    geom_errorbar(aes(ymin=min.R2,
                      ymax=max.R2,
                      y=median.R2,
                      group=model),
                  linewidth=.25)+
    
    geom_line(aes(y=median.R2,
                  group=model))+
    
    
    ggthemes::scale_color_colorblind("",breaks=c("cubist","pls","rf","svmLin"),labels=c("Cubist","PLS","RF","SVM"))+
    theme_pubr()+
    theme(panel.grid.major.y = element_line(linetype = "dotted"),
          #legend.position = "top",
          legend.direction = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(.2,.1)
    )+
    xlab("Size of Training-Set")+
    scale_y_continuous("R2 [wt-% TOC]",breaks=seq(0,1,.1))+
    facet_wrap(~factor(dataset,levels=c("Test sets","DE-2023-BDF_archive")))->plt_train_time_R2
  
  
  plt_train_time_R2%>%
  
  ggsave(filename = "train_size_times_R2.png",
         path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",
         height=6,width=8,device="png")
  
  
  
  #### all ####
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
  
  
    ###################
    