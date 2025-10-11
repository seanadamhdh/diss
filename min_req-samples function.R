






nreq=function(
  # sample variance
  s=.5,
  # req. accuracy as max. acceptable deviation from the mean
  d=.1,
  # sample mean
  avg=2,
  # nreq diff where to stop
  delta=.5,
  #safeguard
  max_i=100,
  verbose=T
){
  n_req0=Inf
  n_req1=1
  i=1
  if(verbose){
  cat("\ti\tn_req0\t\tn_req1\t\tt\t\tdelta\n")
  cat("\t",i,
      "\t",format(n_req0,digits=1),
      "\t\t",format(n_req1,digits=1),
      "\t\t",format(t,digits=3,nsmall=3),
      "\t\t",Inf,
      "\n")}
  
  while(abs(n_req1-n_req0)>=delta&i<=max_i){
  i=i+1
  #students t-value
  t = qt(p=.95,df=n_req1)
  
  
  n_req0=n_req1
  # calc nreq
  n_req1=(t^2*s^2)/(d*avg)^2

  if(verbose){cat("\t",i,
      "\t",format(n_req0,digits=1),
      "\t\t",format(n_req1,digits=1),
      "\t\t",format(t,digits=3,nsmall=3),
      "\t\t",format(abs(n_req1-n_req0)),
      "\n")}
      
  }
  if(verbose){cat("Required samples: ",ceiling(n_req1))}
  
  return(n_req1)
}

out=c()
s_range=seq(.05,5,.05)
avg_range=seq(0.1,5,.1)
d_range=seq(.05,.5,.05)
tot=length(s_range)*length(avg_range)*length(d_range)
pb=progress::progress_bar$new(total=tot,format = "[:bar]\nt:current/:total\(:elapsed)\t ETA: :eta")
for (s in s_range){
  for( avg in avg_range){
    for (d in d_range){
      out=bind_rows(out,
                tibble(s=s,
                       avg=avg,
                       d=d,
                       nreq=nreq(s = s,d = d,avg = avg,verbose = F)))
      pb$tick()
       }
    }
}



ggplot(out,aes(x=s,y=nreq))+geom_point()


