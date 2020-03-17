affair <- read.csv(file.choose())
View(affair)
summary(affair)
affair <- na.omit(affair)
model <- glm(gender~.,data = affair,family = "binomial")
prob <- predict(model,type=c("response"),affair)
prob
confusion <- table(prob>0.5,affair$gender)
confusion
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
library(ROCR)
rocrpred <- prediction(prob,affair$gender)
rocrperf <- performance(rocrpred,"tpr","fpr")
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))