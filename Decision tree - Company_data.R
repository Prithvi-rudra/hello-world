company_data <- read.csv(file.choose())
View(company_data)
summary(company_data)

library(party)
library(gmodels)
library(C50)
library(lattice)
library(ggplot2)
library(caret)
library(Boruta)
library(mlbench)

class(company_data)
company_data<- factor(company_data)
company_tree <- ctree(Sales~.,data = company_data)
plot(company_tree)

set.seed(123)
boruta<- Boruta(Sales~., data = company_data, doTrace = 2, maxRun = 500)
print(boruta)
plot(boruta, lax = 2)
plotImpHistory(boruta)
attStats(boruta)

set.seed(222)
inTraininglocal <- createDataPartition(company_data$Sales,p=0.75,list=F)
training <- company_data[inTraininglocal,]
testing <- company_data[inTraininglocal,]
getNonRejectedFormula(boruta)


company_pred <- as.data.frame(predict(company_tree,company_data))
company_pred
company_pred["final"] <- NULL



CrossTable(company_data$Sales,company_pred)
mean(company_pred$final==company_data$Sales)
View(company_data)

inTraininglocal <- createDataPartition(company_data$Sales,p=0.75,list=F)
training <- company_data[inTraininglocal,]
testing <- company_data[inTraininglocal,]
?C5.0
c5.0 <- factor

model <- C5.0(company_data[-1],company_data$Sales)
company_data$Sales <- as.factor(company_data$Sales)
str(company_data$Sales)

summary(model)
pred <- predict.C5.0(model,testing[-1])
a <- table(testing$Sales,pred)
sum(diag(a)/sum(a))
plot(model)
str(company_data)


acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(company_data,p=.85,list=F)
  training1<-company_data[inTraininglocal,]
  testing<-company_data[-inTraininglocal,]
  
  fittree<-C5.0(training1$Sales~.,data=training1)
  pred<-predict.C5.0(fittree,testing[,-1])
  a<-table(testing$Sales,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc



########################In feauture selection method, with the help of Boruta package, I have found that in 46 iteration is perfored in 16.85301 secs in that 7 confirmed of important attributes for Advertising, Age, CompPrice, Income, Price and 2more########
######3 attributes is confirmed unimportant such as Education, Population, Urban##########