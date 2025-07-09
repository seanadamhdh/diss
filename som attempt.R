#som attempt


require(kohonen)
require(ChemometricsWithR)


som_grid <- somgrid(xdim=6, ydim=5, topo="hexagonal")
som_model <- supersom(data = spc_tmp$spc_sg_snv_rs4,
                      #Y=Data_table_reference%>%as.matrix(),
                      grid = som_grid)

plot(som_model, type = "codes")  # Visualizes spectral codebook for each node
plot(som_model, type = "mapping")
plot(som_model,type="changes")
plot(som_model, type = "property",property = all_data$`TOC [wt-%]`)

plot(som_model, type = "property",property = all_data$Depth_bottom)
