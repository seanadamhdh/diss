# load #####

Lagerungsdichte_Referenz_neu <- read_excel(paste0(data_dir,"/Sean_Environment/BDF/BDF-SSL/1_data/3_physik/Adam-2024_VZ-TRD.xlsx"))


pivot_wider(Lagerungsdichte_Referenz_neu%>%
              mutate(No=rep(c(1:5),13))%>%
              select(-Stechzylindernummer,-Größe),
            names_from=No,
            names_prefix = "VZ_", 
            values_from = `TRD g/cm3`)->VZ_ref

### TRD_prep ####
BDF_SSL%>%
  filter(substr(LabelEvent,2,2)%in%c("Q","R","P"))%>%
  filter(is.na(Flag))%>%
  
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
  )%>%
  bind_rows(
    left_join(
      Lagerungsdichte_Referenz_neu%>%
        transmute(
          LG_sample,
          VZno=Stechzylindernummer,
          Hz_von,
          Hz_bis,
          vol=`Größe`,
          `Soil horizon`=Horizont,
          site_id=paste0("BDF",`BDF-Fläche`),
          Depth_top=`Tiefe von`/100,
          Depth_bottom=`Tiefe bis`/100,
          `dB_105 [g/cm3]`=`TRD g/cm3`,
          Typ="VZ"),
      BDF_SSL%>%filter(str_starts(LabelEvent,"LG"))%>%
        select(`TOC [wt-%]`,LabelEvent,site_id,
               `Soil horizon`,Profile,Depth_top,Depth_bottom
        )%>%
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
    )%>%
      mutate(Device=if_else(Typ=="VZ","Soil ring",NA)))%>%
  # stocks in T ha-1 cm-1
  mutate(across(.cols = c(`dB_40 [g/cm3]`,
                          `dB_40FB [g/cm3]`,
                          `dB_105 [g/cm3]`,
                          `dB_105FB [g/cm3]`,
                          `FSS_40 [g/cm3]`,
                          `FSS_105 [g/cm3]`),
                .fns = ~.*`TOC [wt-%]`,
                .names = "TOCstock_{.col}"),
         )->TRD_prep





cm_aggregate(TRD_prep%>%
               
               group_by(site_id, Depth_top, Depth_bottom) %>%
              mutate(
                Profile = if_else(
                  Typ %in% c("Q") & Profile == "Profil",
                  paste0("Profil", row_number()),
                  Profile
                )
              ) %>%
               ungroup()%>%
               
               group_by(site_id, LG_sample,Depth_top, Depth_bottom) %>%
               mutate(
                 Profile = if_else(
                   Typ %in% c("VZ") & Profile == "Profil",
                   paste0("Profil", row_number()),
                   Profile
                 )
               )%>%ungroup(),
             depth_top_col = "Depth_top",
             depth_bottom_col = "Depth_bottom",
             group_list = c("site_id","Typ","Profile","Soil horizon"),
             aggregate_list = c("Hz_von",
                                "Hz_bis",
                                "dB_40 [g/cm3]",
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
             res_out = .1)->TRD_prep_agg_profile




TRD_prep%>%filter(Typ=="VZ")%>%
  group_by(site_id, LG_sample,Hz_von/100,Hz_bis/100,`Soil horizon`,Profile) %>%
  mutate(
    Profile = if_else(
      Typ %in% c("VZ") & Profile == "Profil",
      paste0("Profil", row_number()),
      Profile
    ))%>%
  summarise(`TOCstock_dB_105 [g/cm3]`=mean(`TOCstock_dB_105 [g/cm3]`,na.rm = T),
            n=length(unique(Profile))) %>%
  rename(
    o3 = `Hz_von/100`,
    u3 = `Hz_bis/100`
  )->VZ_agg



compute_TOCstock(TRD_prep_agg_profile%>%filter(Typ!="VZ"), depth_from = 0,depth_to = 0.3,
                 depth_top_var = "o3",depth_bottom_var = "u3",
                 stock_var = "TOCstock_dB_105 [g/cm3]",
                 grouping_vars = c("site_id","Profile","Typ"))


# Calculate TOCstock for 0–0.3 m and 0–0.5 m
#30
toc_0_30_VZ <- compute_TOCstock(VZ_agg, depth_from = 0,depth_to = 0.3,
                             depth_top_var = "o3",depth_bottom_var = "u3",
                             stock_var = "TOCstock_dB_105 [g/cm3]",
                             grouping_vars = c("site_id","Profile"))%>%mutate(Typ="VZ")

toc_0_30_rest=compute_TOCstock(TRD_prep_agg_profile%>%filter(Typ!="VZ"), depth_from = 0,depth_to = 0.3,
                 depth_top_var = "o3",depth_bottom_var = "u3",
                 stock_var = "TOCstock_dB_105 [g/cm3]",
                 grouping_vars = c("site_id","Profile","Typ"))

toc_0_30=bind_rows(toc_0_30_VZ,toc_0_30_rest)

#50
toc_0_50_VZ <- compute_TOCstock(VZ_agg, depth_from = 0,depth_to = 0.5,
                                depth_top_var = "o3",depth_bottom_var = "u3",
                                stock_var = "TOCstock_dB_105 [g/cm3]",
                                grouping_vars = c("site_id","Profile"))%>%mutate(Typ="VZ")

toc_0_50_rest=compute_TOCstock(TRD_prep_agg_profile%>%filter(Typ!="VZ"), depth_from = 0,depth_to = 0.5,
                               depth_top_var = "o3",depth_bottom_var = "u3",
                               stock_var = "TOCstock_dB_105 [g/cm3]",
                               grouping_vars = c("site_id","Profile","Typ"))

toc_0_50=bind_rows(toc_0_50_VZ,toc_0_50_rest)


# toc_0_50 <- compute_TOCstock(VZ_agg,depth_from = 0,depth_to =  0.5)
# 
# toc_0_30_all=compute_TOCstock(TRD_prep_agg_profile,stock_var = "TOCstock_dB_105 [g/cm3]",grouping_vars = c("site_id","Profile","Typ"))
# # Combine
# toc_combined <- bind_rows(toc_0_30, toc_0_50)

# 
toc_0_30%>%
   ggplot(aes(x=site_id,y=TOCstock,group=paste(site_id,Typ),col=Typ))+geom_boxplot()
##### stock to 30 cm all ####
bind_rows(
  bind_rows(toc_0_30%>%mutate(set="0_30"),
            toc_0_50%>%mutate(set="0_50"))%>%
    group_by(site_id,Typ,set)%>%
  mutate(mean_TOCstock=mean(TOCstock,na.rm=T),
                            min_TOCstock=min(TOCstock,na.rm=T),
                            max_TOCstock=max(TOCstock,na.rm=T))%>%
  bind_rows(tibble(site_id=rep("BDF23",12),
                   Typ=rep("Q",12),
                   mean_TOCstock=rep(NULL,12),
                   min_TOCstock=rep(NULL,12),
                   max_TOCstock=rep(NULL,12),
                   TOCstock=rep(NULL,12),
                   set=c(rep("0_30",6),rep("0_50",6))
                   ))
    )%>%
  filter(Typ!="P")%>%
    
  ggplot(aes(x=site_id,group=Typ,
             y=mean_TOCstock, # stock in T ha-1 cm-1 ...* cm to convert to entire stock in depth range
             fill=Typ))+
  geom_col(position="dodge",col="black")+
  geom_errorbar(aes(ymin = min_TOCstock,
                    ymax= max_TOCstock),
                position=position_dodge(width=.9),width=.5)+
  scale_fill_manual("Sampling method",
                    breaks=c("P","Q","R","VZ"),
                    labels=c("Sampling spade","Quicksampler","Push core","Soil ring"),
                    values=alpha(colorblind_safe_colors()[1:4],.5))+
  theme_pubclean()+
  
  scale_y_continuous(expression("TOC stock [T h"*a^-1*" c"*m^-1*"]"),
                     breaks=seq(0,200,25))+
  xlab("BDF site")+
  facet_wrap(~set,labeller=labeller(name=stocklabeller))#->tocstocks
  
  ggsave(tocstocks,filename="TOCstocks.png",width=8,height = 4,
         device = "png",path = "C:/Users/adam/Desktop/UNI/PhD/DISS/plots/")



# Assume your data is in a dataframe called df
unique_groupings=expand.grid(site_id=unique(TRD_prep$site_id),
                             Typ=unique(TRD_prep$Typ),
                             depth_class=unique(TRD_prep$depth_class))



plt_agg=function(X,var_="TOCst"){
ggplot(X) +
  geom_rect(aes(
    xmin = as.numeric(factor(Profile)) - 0.4,
    xmax = as.numeric(factor(Profile)) + 0.4,
    ymin = o3,
    ymax = u3,
    fill = `dB_105 [g/cm3]`,
  ),
  col="black") +
  geom_hline(yintercept = c(0,.1,.3,.5))+
  
  scale_y_reverse() +
  scale_fill_viridis_c(option = "C") +
  facet_grid(Typ ~ site_id, scales = "free_x", space = "free_x") +
  labs(
    x = "Profile",
    y = "Depth (cm)"
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
}





#### interpolation ####
fill_soil_profile_gaps <- function(data,
                                   depth_top = "top",
                                   depth_bottom = "bottom",
                                   value_cols = c("TOCstock"),
                                   group_vars = c("site_id", "profile_id"),
                                   min_gap = 0.01) {

  
  top_sym <- sym(depth_top)
  bottom_sym <- sym(depth_bottom)
  group_syms <- syms(group_vars)
  
  value_syms <- syms(value_cols)
  
  data %>%
    rename(top = !!top_sym, bottom = !!bottom_sym) %>%
    group_by(!!!group_syms) %>%
    arrange(top) %>%
    group_modify(function(df, keys) {
      df <- df %>% mutate(interpolated = FALSE)
      
      gaps <- which(abs(df$top[-1] - df$bottom[-nrow(df)]) > min_gap)
      
      if (length(gaps) == 0) return(df)
      
      interpolated_rows <- map_dfr(gaps, function(i) {
        top_gap <- df$bottom[i]
        bottom_gap <- df$top[i + 1]
        thickness <- bottom_gap - top_gap
        
        if (thickness <= 0) return(NULL)
        
        # Linear interpolation for each value column
        interpolated_values <- map_dfc(value_cols, function(vcol) {
          above <- df[[vcol]][i]
          below <- df[[vcol]][i + 1]
          thick_above <- df$bottom[i] - df$top[i]
          thick_below <- df$bottom[i + 1] - df$top[i + 1]
          
          # Avoid division by zero
          dens_above <- ifelse(thick_above > 0, above / thick_above, NA)
          dens_below <- ifelse(thick_below > 0, below / thick_below, NA)
          
          avg_density <- mean(c(dens_above, dens_below), na.rm = TRUE)
          
          tibble(!!vcol := avg_density * thickness)
        })
        
        tibble(
          top = top_gap,
          bottom = bottom_gap,
          interpolated = TRUE
        ) %>%
          bind_cols(interpolated_values)
      })
      
      bind_rows(df, interpolated_rows) %>%
        arrange(top)
    }) %>%
    ungroup() %>%
    rename(!!depth_top := top, !!depth_bottom := bottom)
}












##### testing #####


data=TRD_prep
depth_from=0
depth_to=.13
depth_top_var="Depth_top"
depth_bottom_var="Depth_bottom"
stock_var="TOCstock_dB_105 [g/cm3]"
grouping_vars=c("site_id","Profile","Typ")
 
  
  grouping_vars=syms(grouping_vars)
  data %>%
    rename(stock=.data[[stock_var]],
           Depth_top=.data[[depth_top_var]],
           Depth_bottom=.data[[depth_bottom_var]])%>%
    mutate(
      thickness = Depth_bottom - Depth_top
    )%>%
    mutate(
      # Calculate horizon-thickness and overlap
      p_min = pmin(Depth_bottom, depth_to),
      p_max = pmax(Depth_top, depth_from),
      delta = p_min - p_max,
      overlap = pmax(delta, 0),  # Set negative overlaps to 0
      weight = overlap / thickness, #m/m
      weighted_stock = stock * weight 
    )%>%
    group_by(!!!grouping_vars) %>%
    summarise(TOCstock = mean(weighted_stock,na.rm=T), #weighted avg stock (g/cm³) or T ha-1 cm-1
                (depth_to-depth_from)* # times depth increment (in m)
                100, # m->cm; Unit is T ha-1 for depth increment
              .groups = "drop") %>%
    mutate(depth_interval = paste0(depth_from, "_", depth_to, "m"))
  
  
  
  
  
  ######## copy paste from BDFfrischeval ##########
  ######### individual bars ####
  (BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0)%>%
      left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name")))%>%
    mutate(
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    filter(Tiefe_bis<=30)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(TOC_sum_atroAll=sum(TOC_budget_atroAll))%>%
    # reference data 
    rbind(
      data.frame(
        BDF=c("02","23","30","35","23"),
        Typ=c(rep("Referenz",4),"Quicksampler"),
        Position=rep(NA,5),
        # sums for top 30 cm
        TOC_sum_atroAll=c(57.4,86.2,127.6,46.4,NA))
    )%>%
    ggplot(aes(x=paste(BDF),group=paste(Typ,Position),y=TOC_sum_atroAll))+geom_col(position = "dodge",col="black",aes(fill=Typ))+
    geom_text(angle=90,aes(x=BDF,group=paste(Typ,Position),y=10,label=Position,hjust = 0,vjust=.5),
              position=position_dodge(width=.925),size = unit(3,"pt"))+
    theme_pubr()+
    scale_fill_manual(breaks=c("Quicksampler","Rammkern","Referenz"),values=colorblind_safe_colors[2:4])+
    ylab(expression("TOC [T C "*ha^-1 *cm^-1*"]"))+
    xlab("BDF")
      
  
  
  (BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0)%>%left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name")))%>%
    mutate(
      
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    filter(Tiefe_bis<=50)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(TOC_sum_atroAll=sum(TOC_budget_atroAll))%>%
    # reference data 
    rbind(
      data.frame(
        BDF=c("02","23","30","35","23"),
        Typ=c(rep("Referenz",4),"Quicksampler"),
        Position=rep(NA,5),
        # sums for top 30 cm
        TOC_sum_atroAll=c(75,92.7,206.6,52.3,NA))
    )%>%
    ggplot(aes(x=paste(BDF),group=paste(Typ,Position),y=TOC_sum_atroAll))+geom_col(position = "dodge",col="black",aes(fill=Typ))+
    geom_text(angle=90,aes(x=BDF,group=paste(Typ,Position),y=10,label=Position,hjust = 0,vjust=.5),
              position=position_dodge(width=.925),size = unit(3,"pt"))+
    theme_pubr()+
    scale_fill_manual(breaks=c("Quicksampler","Rammkern","Referenz"),values=colorblind_safe_colors[2:4])+
    ylab(expression("TOC [T C "*ha^-1 *cm^-1*"]"))+
    xlab("BDF")

   ####### same as report #####
  
  ggarrange(
    BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0&
                              Position!="Profil_4"&   # one mess-up
                              !(Typ=="Rammkern"&Position=="Profil")  # 1 core at Profile BDF30, rm, otherwise additional core with no vals (=0) @ profile
    )%>%
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
    
    
    BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0&Tiefe_von>=0&
                              Position!="Profil_4"&
                              !(Typ=="Rammkern"&Position=="Profil")  )%>%
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
        # sums for top 50 cm
        TOC_sum_atroAll_mean=c(75,92.7,206.6,52.3,NA)
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
  )->TOC_budget_as_report    
  
  
  ggsave(plot=TOC_budget_as_report,filename="TOC_budget_0-30_0-50.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width = 8,height = 5)
  
  
  
  # stat ####
  maxDepth=30
  BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0&Tiefe_von>=0&
                            Position!="Profil_4"&
                            !(Typ=="Rammkern"&Position=="Profil"))%>%
    left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name"))%>%
    mutate(
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    mutate(TOC_budget_atroAll=case_when(
      Tiefe_bis<=maxDepth~TOC_budget_atroAll,
      Tiefe_bis>maxDepth&Tiefe_von<maxDepth~
        TOC_budget_atroAll/(Tiefe_bis-Tiefe_von)*(maxDepth-Tiefe_von),
      Tiefe_bis>maxDepth~0
    ))%>%
    #filter(Tiefe_bis<=maxDepth)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(
      TOC_sum_atroAll=sum(TOC_budget_atroAll)
    )%>%
    # reference data 
    rbind(data.frame(
      Lagerungsdichte_Referenz_neu%>%
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
          Hz_bis<=maxDepth~TOC_budget,
          Hz_bis>maxDepth&Hz_von<maxDepth~
            TOC_budget/(Hz_bis-Hz_von)*(maxDepth-Hz_von),
          Hz_bis>maxDepth~0
        )
        )%>%
        group_by(BDF=`BDF-Fläche`,Position=paste0("VZ_",rep(c(1:5),13)))%>%
        summarise(TOC_sum_atroAll=sum(TOC_budget)),
      Typ=rep("Referenz neu",4)
    ))%>%mutate(maxDepth=30)->TOC_budget_table30
  
  maxDepth=50
  BDF_frisch$TRD%>%filter(sample_id%>%substr(2,2)%in%c("R","Q")&Tiefe_von>=0&Tiefe_von>=0&
                            Position!="Profil_4"&
                            !(Typ=="Rammkern"&Position=="Profil"))%>%
    left_join(BDF_frisch$soliTOC,by=c("sample_id"="Name"))%>%
    mutate(
      TOC_budget_atroAll=TOC/100 #gC/g
      *Atro_density_all# gC/cm3
      *1000#kg/m³
      *10#t/ha*m
      *(abs(Tiefe_von-Tiefe_bis)/100)
    )%>%
    mutate(TOC_budget_atroAll=case_when(
      Tiefe_bis<=maxDepth~TOC_budget_atroAll,
      Tiefe_bis>maxDepth&Tiefe_von<maxDepth~
        TOC_budget_atroAll/(Tiefe_bis-Tiefe_von)*(maxDepth-Tiefe_von),
      Tiefe_bis>maxDepth~0
    ))%>%
    #filter(Tiefe_bis<=maxDepth)%>%
    group_by(BDF,Typ,Tiefe_von,Tiefe_bis,Position)%>%
    summarise(across(contains("TOC_budget"),.fns=mean))%>%#(TOC_budget,na.rm = T))%>%
    group_by(BDF,Typ,Position)%>%
    summarise(
      TOC_sum_atroAll=sum(TOC_budget_atroAll)
    )%>%
    # reference data 
    rbind(data.frame(
      Lagerungsdichte_Referenz_neu%>%
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
           Hz_bis<=maxDepth~TOC_budget,
           Hz_bis>maxDepth&Hz_von<maxDepth~
             TOC_budget/(Hz_bis-Hz_von)*(maxDepth-Hz_von),
           Hz_bis>maxDepth~0
         )
         )%>%
         group_by(BDF=`BDF-Fläche`,Position=paste0("VZ_",rep(c(1:5),13)))%>%
         summarise(TOC_sum_atroAll=sum(TOC_budget)),
      Typ=rep("Referenz neu",4)
    ))%>%mutate(maxDepth=50)->TOC_budget_table50
  
   

  TOC_budget_table=bind_rows(TOC_budget_table30,
            TOC_budget_table50)%>%group_by(,add=T,maxDepth)
  
   left_join(
     summarise_metrics(TOC_budget_table,parameters = "TOC_sum_atroAll"),
     summarise(TOC_budget_table,n=n())
   )->TOC_budget_table_summary
   
   TOC_budget_table_summary%>%view
    
   TOC_budget_table_summary%>%write_excel_csv("C:/Users/adam/Desktop/UNI/PhD/DISS/tables/TOC_budget_summary.csv")
   