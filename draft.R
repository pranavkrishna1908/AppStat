Data
plot(Data)



Data$OperatingSystems = as.factor(Data$OperatingSystems)
Data$Browser = as.factor(Data$Browser)
Data$Region = as.factor(Data$Region)
Data$TrafficType = as.factor(Data$TrafficType)
colnames(Data) = c(colnames(Data)[1:17],'purchase')
y =  Data$purchase
plot(Data$Administrative,y)#people opening too many administrative pages dont 
plot(Data$Informational,y)







Data$OperatingSystems = as.factor(Data$OperatingSystems)
Data$Browser = as.factor(Data$Browser)
Data$Region = as.factor(Data$Region)
Data$TrafficType = as.factor(Data$TrafficType)
colnames(Data) = c(colnames(Data)[1:17],'purchase')
AUC_eval <- function(gmodel,Data){
  set.seed(517)
  Folds <- matrix(sample(1:dim(Data)[1]), ncol=5)
  AUC <- rep(0,5)
  for(k in 1:5){
    train <- Data[-Folds[,k],]
    test <- Data[Folds[,k],]
    my_gm <- glm(gmodel$formula, family="binomial", data=train)
    test_pred <- predict(my_gm, newdata = test, type="response")
    AUC[k] <- auc(test$purchase,test_pred)
  }
  return(mean(AUC))
}
gm1 <- glm(purchase~., family="binomial", data=Data)
print(AUC_eval(gm3,Data))
Anova(gm1, type = 2)
gm2 = glm(purchase~ProductRelated_Duration + ExitRates+PageValues+TrafficType, family="binomial", data=Data)
anova(gm1,gm2,test = 'LR')
gm3 = stepAIC(gm1,k = log(nrow(Data)))#bic for model slection
Anova(gm3, type = 2)
gm3
by_cyl <- Data %>% group_by(Month)
group



Data %>%
  mutate(res=resid(gm3),backgrd=as.numeric(backgrd),
         educatn=as.numeric(educatn)) %>%
  pivot_longer(-res) %>% ggplot(aes(y=res,x=value)) +
  facet_wrap(~ name, scales = "free") + geom_point() + geom_smooth()

