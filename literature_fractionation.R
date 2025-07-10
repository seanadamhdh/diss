

tibble(
  ID_long=c("Elliot1986","Six2000a","Sanderman2014","LopezSangil2013","Sollins1984","Sollins2009","Golchin1994",
            "Six1998a","Six1998b","Shaymukhametov1984","Dichon2016","Steffens2009","Balesdent1987","Mikutta2006",
            "Rovira2012","PoeplauXXXX","Leitfeld2001","Six2000b","Zimmermann2007","Kaiser2016"),
  ID=c("A1","A2","P1","P2","D1","D2","D3","AD1","PD1","PD2","PD3","PD4","PD5","C1","C2","M1","M2","M3","M4","M5"),
  diff=c(1,1,2,1,1,3,1,2,2,2,2,2,3,2,2,2,2,2,3,3),
  red=c(3,2,3,2,2,2,2,2,1,2,3,2,2,1,3,3,3,2,2,1),
  rec=c(3,3,3,3,3,2,3,2,2,2,2,2,3,3,3,3,3,3,3,2),
  rep=c(3,3,3,2,2,1,2,2,2,2,2,1,2,3,3,1,3,2,2,1),
  imb=c(3,3,1,2,1,2,1,3,2,3,2,2,2,1,3,1,2,3,2,2),
  time=c(2,3,3,2,3,1,3,2,3,1,1,1,3,3,3,3,1,3,1,1))%>%
  mutate(time_h_min=case_match(time,
                               1~25, # 7-25
                               2~7, # 4-7
                               3~4 #.5-4
  ),time_h_max=case_match(time,
                          1~7, # 7-25
                          2~4, # 4-7
                          3~.5 #.5-4
  ),
  group=str_remove(ID,"\\d+"))%>%
    mutate(x=(.32*diff+.07*red+.2*rec+.2*rep+.13*imb+.08*time))%>%
    ggplot()+geom_errorbar(aes(xmin = time_h_min,xmax=time_h_max,y=x,col=group))+
  geom_smooth(aes(x=(time_h_min+time_h_max)/2,y=x,col=group),method="glm",se=F)
  
  