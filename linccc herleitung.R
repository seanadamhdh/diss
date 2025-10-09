


x=iris$Petal.Length
y=iris$Petal.Width

DescTools::CCC(x,y)$rho.c$est
DescTools::CCC(x,y)$C.b
cor(x,y)



v=sd(x)/sd(y)
u=(mean(x)-mean(y))/sqrt(sd(x)*sd(y))

#C.b
((v+1/v+u^2)/2)^-1

# C.b written out
(((sd(x)/sd(y))+1/(sd(x)/sd(y))+((mean(x)-mean(y))/sqrt(sd(x)*sd(y)))^2)/2)^-1

#C.b written out, simplified
2  /  (  (sd(x)/sd(y))  +  1 / (sd(x)/sd(y))  +   (mean(x)-mean(y))^2 / (sd(x)*sd(y))   )


r= cov(x,y) / (sd(x)*sd(y))




#pC 
2  /  ( ( (sd(x)/sd(y)) ) +  (1 / (sd(x)/sd(y)) ) +   ((mean(x)-mean(y))^2 / (sd(x)*sd(y))  ) )  * cov(x,y) / (sd(x)*sd(y))



# pC simplified
2 * cov(x,y) /  (  ((sd(x)/sd(y)) * (sd(x)*sd(y) ) + ( (sd(x)*sd(y) / (sd(x)/sd(y)) ) +  ( (mean(x)-mean(y))^2 )  )))


# pC simplified 2
2 * cov(x,y) /  (               var(x)             +              var(y)              +  ( (mean(x)-mean(y))^2 )  )




