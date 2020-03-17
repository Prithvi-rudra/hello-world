fraud_check <- read.csv(file.choose())
View(fraud_check)
ifelse(fraud_check$Taxable.Income<=30000,"risky","good")
fraud_check$Taxable.Income <- ifelse(fraud_check$Taxable.Income<=30000,"risky","good")
str(fraud_check)
fraud_check$Taxable.Income <- as.factor(fraud_check$Taxable.Income)
table(fraud_check$Taxable.Income)

library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

set.seed(123)

boruta<- Boruta(Taxable.Income~., data = fraud_check, doTrace = 2, maxRun = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
bor<- TentativeRoughFix(boruta)
attStats(boruta)


set.seed(123)
ind <- sample(2, nrow(fraud_check),replace = TRUE, prob = c(0.7,0.3))
fraud_train <- fraud_check[ind==1,]
fraud_test <- fraud_check[ind==2,]

library(randomForest)
set.seed(121)
fc <- randomForest(Taxable.Income~.,data = fraud_check)
print(fc)
attributes(fc)
fc$confusion

library(caret)
p1 <- predict(fc,fraud_train)
p1
head(p1)
table(head(p1))
confusionMatrix(p1,fraud_train$Taxable.Income)

p2 <- predict(fc,fraud_test)
p2
head(p2)
table(head(p2))

plot(fc)



legend("topright", colnames(fc$err.rate),col=1:4,cex=0.8,fill=1:4)
fraud_forest <- randomForest(Taxable.Income~.,data = fraud_check, importance=TRUE)
varImpPlot(fc)
varImpPlot(fc,sort = TRUE,n.var = 0.5, main = "Income Tax")
importance(fc)
varUsed(fc)
plot(fraud_forest)

fraud_acc <- mean(fraud_check$Taxable.Income==predict(fraud_forest))
fraud_acc
varImpPlot(fraud_forest)

hist(treesize(fc), main = "no. of nodes for the tree",col = "green")

########################In feauture selection method, with the help of Boruta package, I have observed that there is no confirmed of important and tentatvie attributes for all the variable########
######I have found that iin 131 iteration is perfored in 4.397008 mins in that no attributes deemed important and 10 attributes is confirmed unimportant##########