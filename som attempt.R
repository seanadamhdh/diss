#som attempt


require(kohonen)
require(ChemometricsWithR)


som_grid <- somgrid(xdim=10, ydim=10, topo="hexagonal")
som_model <- supersom(data = all_data%>%filter(!str_detect(Campaign,"field")&!is.na(spc_sg_snv_rs4[,1]))%>%
                        pull(spc_sg_snv_rs4),
                      #Y=Data_table_reference%>%as.matrix(),
                      grid = som_grid,rlen = 500,alpha=c(.1,.01))


# no change in clusters
# som_model_pc=supersom(data = pc$calres$scores,
#                       #Y=Data_table_reference%>%as.matrix(),
#                       grid = som_grid,rlen = 500,alpha=c(.1,.01))

plot(som_model, type = "codes",code)  # Visualizes spectral codebook for each node
plot(som_model, type = "mapping",
     property = all_data%>%filter(!str_detect(Campaign,"field")&!is.na(spc_sg_snv_rs4[,1]))%>%pull(site_id)%>%factor)
plot(som_model,type="changes")
plot(som_model, type = "property",
     property = all_data%>%filter(!str_detect(Campaign,"field")&!is.na(spc_sg_snv_rs4[,1]))%>%
       #pull(`Al_t [mg/kg]`)
       pull(`TOC [wt-%]`))


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


ggplot(plot_data, aes(x = x, y = y, color =`TOC [wt-%]`,shape = `Land use`)) +
  geom_jitter(width = 0.2, height = 0.2, size = 3) +
  theme_minimal() +
  coord_fixed() +
  labs(x = "SOM X", y = "SOM Y")+
  scale_color_viridis_c(na.value = rgb(0,0,0,0),alpha = .5)


ggplot(plot_data, aes(x = x, y = y, fill =`TOC [wt-%]`))+#,shape = `Land use`)) +
  geom_density_2d_filled()+#(width = 0.2, height = 0.2, size = 3) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "SOM Mapping in ggplot2", x = "SOM X", y = "SOM Y")+scale_color_viridis_c(na.value = rgb(0,0,0,0),alpha = .5)
#########



names(Data_table_reference%>%select(where(is.numeric)))


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

