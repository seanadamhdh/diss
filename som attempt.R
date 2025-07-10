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

plot(som_model, type = "codes")  # Visualizes spectral codebook for each node
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


ggplot(plot_data, aes(x = x, y = y, color =`TOC [wt-%]`,shape = `Land use`)) +
  geom_jitter(width = 0.2, height = 0.2, size = 3) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "SOM Mapping in ggplot2", x = "SOM X", y = "SOM Y")+scale_color_viridis_c(na.value = rgb(0,0,0,0),alpha = .5)


