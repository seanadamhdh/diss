library(dplyr)
library(tibble)

bind_rows(
  tibble(
    depth_1 = c(0,10,30,50,70),
    depth_2 = c(10,30,50,70,100),
    program = "BZE_LW",
    type = "o"
  ),
  tibble(
    depth_1 = c(-5,0,5,10,30,60,90,140),
    depth_2 = c(0,5,10,30,60,90,140,200),
    program = "BZE_Forst",
    type = c(rep("o",7), "f")
  ),
  tibble(
    depth_1 = c(0,10,20,30,60),
    depth_2 = c(10,20,30,60,120),
    program = "BDF_G",
    type = c(rep("o",4), "f")
  ),
  tibble(
    depth_1 = c(0,30,60),
    depth_2 = c(30,60,120),
    program = "BDF_A",
    type = c(rep("o",2), "f")
  ),
  tibble(
    depth_1 = c(0,5,15,30,60),
    depth_2 = c(5,15,30,60,100),
    program = "ICOS",
    type = "o"
  ),
  tibble(
    depth_1 = c(0,5,15,30,60,100),
    depth_2 = c(5,15,30,60,100,150),
    program = "SoilGrids",
    type = "o"
  ),
  tibble(
    depth_1 = c(0,10,30),
    depth_2 = c(10,30,50),
    program = "FAO, LUCAS",
    type = c(rep("o",2), "f")
  ) %>%
    add_row(
      depth_1 = 50,
      depth_2 = 100,
      program = "FAO, LUCAS",
      type = "f"
    ),
  tibble(
    depth_1 = c(0,20,40,60,80,100,150),
    depth_2 = c(20,40,60,80,100,150,200),
    program = "HWSDv2",
    type = "o"
  ),
  tibble(
    depth_1 = c(0,30),
    depth_2 = c(30,100),
    program = "ISCN",
    type = c("o", "f")
  ),
  tibble(
    depth_1 = c(0,20),
    depth_2 = c(20,80),
    program = "DK, UK",
    type = c("o", "f")
  ),
  tibble(
    depth_1 = c(0,10,20,30),
    depth_2 = c(10,20,30,50),
    program = "USA",
    type = c(rep("o",3), "f")
  )
)->depths

ggplot(depths,aes(x = program)) +

  # Background gradient by adding a full-depth geom_rect for visual gradient

  geom_rect(data = expand.grid(
    program = unique(depths$program),
    depth = seq(-5,0, by = 1)),
    aes(ymin = depth, ymax = depth + 1,
        xmin = as.numeric(factor(program)) - 0.5,
        xmax = as.numeric(factor(program)) + 0.5),
        fill = rgb(.22,.11,.0,1),
    inherit.aes = FALSE) +

  geom_rect(data = expand.grid(
    program = unique(depths$program),
    depth = seq(0, 205, by = 1)),
    aes(ymin = depth, ymax = depth + 1,
        xmin = as.numeric(factor(program)) - 0.5,
        xmax = as.numeric(factor(program)) + 0.5,
        fill = 1/(depth+20)),
    inherit.aes = FALSE) +

  geom_rect(aes(
    ymin = depth_1, ymax = depth_2,
    xmin = as.numeric(factor(program)) - 0.4,
    xmax = as.numeric(factor(program)) + 0.4,
    linetype=type
    ),
  color="black",
  fill=fill_alpha("grey",.2)) +
  scale_y_reverse() +
  scale_x_discrete(position = "top")+
  scale_fill_gradient2(low=rgb(.7,.6,.5,1),mid=rgb(1,.8,.5,1),high=rgb(.25,.12,.0,1))+
  scale_linetype_manual("",breaks=c("o","f"),values=c("solid","dotted"),labels=c("Required","Optional"))+
  labs(y = "Depth (cm)",x="") +
  coord_cartesian(ylim=c(195,-0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.1),
        panel.grid.major.x=element_line(color="black"),
        panel.grid.major.y=element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y.left = element_line(),
        axis.ticks.length.y.left = unit(5, "pt"),
        text = element_text(size=15)
        )+
  guides(fill="none")->sampling_depth_literature


ggsave(plot=sampling_depth_literature,filename="Sampling_depth_literature.png",path="C:/Users/adam/Desktop/UNI/PhD/DISS/plots/",width =6,height = 8)




