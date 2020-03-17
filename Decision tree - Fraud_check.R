fraud_check <- read.csv(file.choose())
View(fraud_check)
ifelse(fraud_check$Taxable.Income<=30000,"risky","good")
fraud_check$Taxable.Income <- ifelse(fraud_check$Taxable.Income<=30000,"risky","good")
library(tree)
install.packages("party")
library(party)
class(fraud_check$Taxable.Income)
fraud_check$Taxable.Income<-factor(fraud_check$Taxable.Income)
fraud_tree <- ctree(Taxable.Income~.,data=fraud_check)
plot(fraud_tree)
text(fraud_tree,pretty =0)

library(Boruta)
library(mlbench)
library(caret)
library(C50)
library(lattice)
library(ggplot2)


set.seed(123)
boruta<- Boruta(Taxable.Income~., data = fraud_check, doTrace = 2, maxRun = 500)
print(boruta)
plot(boruta, las = 2)
plotImpHistory(boruta)
attStats(boruta)

set.seed(222)
ind <- sample(2, nrow(fraud_check), replace = TRUE, prob = c(0.7,0.3))
training <- fraud_check[ind==1,]
testing <- fraud_check[ind==2,]


pred_dec <- as.data.frame(predict(fraud_tree,fraud_check))
pred_dec

head(pred_dec)
head(fraud_check$Taxable.Income)
confusionMatrix(pred_dec,fraud_check$Taxable.Income)

library(gmodels)
crossTable(fraud_check$Taxable.Income,pred_dec)
mean(pred_dec$final==fraud_check$Taxable.Income)
View(pred_dec)


?C5.0
model <- C5.0(Taxable.Income~.,data=training,trials = 40)
??inTraininglocal
summary(model)
pred <- predict.C5.0(model,testing[,-3])

a <- table(testing$Taxable.Income,pred)
sum(diag(a)/sum(a))
plot(model)

acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(fraud_check,p=.85,list=F)
  training1<-fraud_check[inTraininglocal,]
  testing1<-fraud_check[-inTraininglocal,]
  
  fittree<-C5.0(training1$Taxable.Income~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-3])
  a<-table(testing$Taxable.Income,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc



########################In feauture selection method, with the help of Boruta package, I have observed that there is no confirmed of important and tentatvie attributes for all the variable########
######I have found that in 13 iteration is perfored in 5.175032 mins in that no attributes deemed important and 10 attributes is confirmed unimportant##########
