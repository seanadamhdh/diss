

# source load chunck of load-and-eval_BDF-SSL.R


all1=pull_set_soliTOC("//zfs1.hrz.tu-freiberg.de/fak3ibf/Hydropedo/lab_data/soliTOC/BDF_run1v6edit.xlsx",set_id = c("[:alpha:]"),set_memo = NA)

all1%>%filter(Methode%in%c("DIN19539","DIN19539GS"))%>%
  filter(str_detect(Name,"DIN"))%>%
  pivot_longer(cols = c(#TOC400,
                        ROC,
                        TIC900#,TOC,TC
                        ))%>%
  ggplot(aes(x=Methode,y=value,fill=Methode))+
  geom_boxplot()+
  facet_wrap(~name,scales="free")+
  theme_pubclean()+
  ylab("ROC, TIC900 [wt-%]")+
  xlab("")+
  #scale_y_log10()+
  scale_fill_manual("Repeated measurements of DIN standards",values = colorblind_safe_colors()[c(2,4)])->p1
p1  
ggsave(plot=p1,filename="ROC_TIC_Variants_STANDARD.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 6,height = 4)


all1%>%filter(Methode%in%c("DIN19539","DIN19539GS"))%>%
  filter(str_detect(Name,"DIN"))%>%
  pivot_longer(cols = c(TOC400,ROC,TIC900,TOC,TC))%>%
  summarise_metrics(grouping_variables = c("Methode","name"),variables = "value")%>%
  mutate(across(where(~is.numeric(.)),~signif(.,3)))%>%
  filter(name%in%c("ROC","TIC900"))%>%write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/rocticVariantDINstd.csv")
