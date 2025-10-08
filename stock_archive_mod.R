
train=stock_data%>%filter(!is.na(`dB [g/cm3]`))%>%mutate(Y=`dB [g/cm3]`,X=spc_sg_snv_rs4)
test=stock_data%>%filter(Campaign%>%str_detect("field"))

## set cubist tuning framework ####
### parameter grid ####
trainGrid<-expand.grid(C=10**seq(-4,0,.1))

### cross-validation ####
fitControl <- trainControl(
  ## 10-fold CV
  method = "cv",
  number = 10,
  ## print progress
  verboseIter = TRUE,
  returnData = F,
  allowParallel = T)

moddB=train(y=train$Y,
                  x=as.matrix(train$X),
                  method="svmLinear",
                  tuneGrid = trainGrid,
                  trControl = fitControl,
  )



train=stock_data%>%filter(!is.na(`TOC [wt-%]`)&!Campaign%>%str_detect("field"))%>%mutate(Y=`TOC [wt-%]`,X=spc_sg_snv_rs4)

modTOC=train(y=train$Y,
            x=as.matrix(train$X),
            method="svmLinear",
            tuneGrid = trainGrid,
            trControl = fitControl,
)


train=stock_data%>%filter(!is.na(`TOC [wt-%]`)&!Campaign%>%str_detect("field")&!is.na(`dB [g/cm3]`))%>%mutate(Y=`TOC [wt-%]`*`dB [g/cm3]`,X=spc_sg_snv_rs4)

modstock=train(y=train$Y,
             x=as.matrix(train$X),
             method="svmLinear",
             tuneGrid = trainGrid,
             trControl = fitControl,
)



test=mutate(test,
            dbpred=predict(moddB,spc_sg_snv_rs4),
            tocpred=predict(modTOC,spc_sg_snv_rs4),
            stockpred=predict(modstock,spc_sg_snv_rs4))




ggarrange(
ggplot(test)+
  geom_abline(slope=1)+
  geom_point(aes(x=`TOC [wt-%]`*`dB_105 [g/cm3]`,y=tocpred*dbpred,col=site_id))+
  theme_pubr(),


ggplot(test)+
  geom_abline(slope=1)+
  geom_point(aes(x=`TOC [wt-%]`*`dB_105 [g/cm3]`,y=stockpred,col=site_id),shape=4)+
  theme_pubr(),
common.legend = T
)
  