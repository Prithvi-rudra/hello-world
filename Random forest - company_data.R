company_data <- read.csv(file.choose())
View(company_data)

library(Boruta)
library(mlbench)
library(caret)
library(randomForest)



summary(company_data)
str(company_data)
company_data$Sales <- as.factor(company_data$Sales)
table(company_data$Sales)

set.seed(123)

buruto <- Boruta(Sales~., data = company_data, doTrace = 2, maxRuns = 500)
print(buruto)
plot(buruto, las = 2, cex.axis = 0.7)
plotImpHistory(buruto)
bor<- TentativeRoughFix(buruto)
attStats(buruto)

set.seed(222)
ind <- sample(2, nrow(company_data),replace = TRUE, prob = c(0.7,0.3))
company_train <- company_data[ind==1,]
company_test <- company_data[ind==2,]

library(randomForest)
set.seed(222)
rf <- randomForest(Sales~., data = company_data)
rf
print(rf)
attributes(rf)
rf$confusion

library(caret)
predict1 <- predict(rf,company_train)
predict1
head(predict1)
head(company_train$Sales)
confusionMatrix(predict1,company_train$Sales)

predict2 <- predict(rf,company_test)
predict2
head(predict2)
head(company_test$Sales)
confusionMatrix(predict2,company_test$Sales) 

plot(rf)
getNonRejectedFormula(buruto)

legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
company_forest <- randomForest(Sales~.,data = company_data, importance=TRUE)
varImpPlot(rf)
varImpPlot(rf,sort = T,n.var = 10, main = "Top 10 - Variable Importance")
importance(rf)
varUsed(rf)
plot(company_forest)
company_acc <- mean(company_data$Sales==predict(company_forest))
company_acc
varImpPlot(company_forest)
hist(treesize(rf), main = "no. of nodes for the tree",col = "green")
varImpPlot(rf)
varImpPlot(rf,sort = T,n.var = 10, main = "Top 10 - Variable Importance")
importance(rf)
varUsed(rf)
company_forest <- randomForest(Sales~.,data = company_data, importance=TRUE)
plot(company_forest)

x <- company_train[,2:11]
y <- company_train[,1]
t <- tuneRF(x,y, stepFactor = 1.5,plot = TRUE,ntreeTry = 500, doBest = FALSE, trace = TRUE,improve = 0.05)
?tuneRF


########################In feauture selection method, with the help of Boruta package, I have observed that there is no confirmed of important and tentatvie attributes for all the variable########
######I have found that in 33 iteration is perfored in 1.529002 mins in that no attributes deemed important and 10 attributes is confirmed unimportant##########