salary_test <- read.csv(file.choose())
View(salary_test)

table(salary_test$Salary)
library(tm)
library(naivebayes)
library(ggplot2)
library(caret)
library(e1071)
library(psych)
install.packages('evaluate')
library(evaluate)
install.packages("digest")
library(digest)

salary_train <- read.csv(file.choose())

str(salary_train)
View(salary_train)
salary_train$educationno=as.factor(salary_train$educationno)
class(salary_train$educationno)

salary_test <- read.csv(file.choose())
View(salary_test)
str(salary_test)
salary_test$educationno=as.factor(salary_test$educationno)
class(salary_test$educationno)


# Plot and ggplot 
library(ggplot2)
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


plot(salary_train$workclass,salary_train$Salary)

plot(salary_train$education,salary_train$Salary)
plot(salary_train$educationno,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$race,salary_train$Salary)
plot(salary_train$sex,salary_train$Salary)


ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalgain, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalloss, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$hoursperweek, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(salary_train$native,salary_train$Salary)

#Density Plot 

ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'black')

ggtitle("Age - Density Plot")

ggplot(data=salary_train,aes(x = salary_train$workclass, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("Workclass Density Plot")

ggplot(data=salary_train,aes(x = salary_train$education, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'red')

ggtitle("education Density Plot")

ggplot(data=salary_train,aes(x = salary_train$educationno, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'yellow')

ggtitle("educationno Density Plot")




ggplot(data=salary_train,aes(x = salary_train$maritalstatus, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("maritalstatus Density Plot")


ggplot(data=salary_train,aes(x = salary_train$occupation, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("occupation Density Plot")

ggplot(data=salary_train,aes(x = salary_train$sex, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("sex Density Plot")

ggplot(data=salary_train,aes(x = salary_train$relationship, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Relationship Density Plot")

ggplot(data=salary_train,aes(x = salary_train$race, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')



ggtitle("Race Density Plot")

ggplot(data=salary_train,aes(x = salary_train$capitalgain, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Capitalgain Density Plot")

ggplot(data=salary_train,aes(x = salary_train$capitalloss, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Capitalloss Density Plot")

ggplot(data=salary_train,aes(x = salary_train$hoursperweek, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("Hoursperweek Density Plot")

ggplot(data=salary_train,aes(x = salary_train$native, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggtitle("native Density Plot")



# Naive Bayes Model 
Model <- naiveBayes(salary_train$Salary ~ ., data = salary_train)
Model


Model_pred <- predict(Model,salary_test)
mean(Model_pred==salary_test$Salary)

Model_pred


confusionMatrix(Model_pred,salary_test$Salary)





prop.table(table(salary_test$Salary))

                         
salary_test$Salary <- as.numeric(as.character(salary_test$Salary))


ifelse(salary_test$Salary<=50,"no","yes")

x <- data.frame(matrix(c(1,0,1,0,1,1),nrow=3))
x[x==0] <- 'no'
x[x==1] <- 'yes'

CA <- ifelse((salary_test$Salary == "Yes"),1,ifelse((salary_test$Salary== "No"),2 ))


moreinfo <- data.frame(Salary = c("Yes", "No"),stringsAsFactors = FALSE)


  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
levels(salary_test$Salary)


convert_counts

salary_train <- apply(salary_train, MARGIN = 2, convert_counts)
salary_test  <- apply(salary_test, MARGIN = 2, convert_counts)
salary_test <- as.data.frame(salary_test)
str(salary_test$Salary)
View(salary_test)
View(salary_train)

library(e1071)
salary_classifier <- naiveBayes(Salary~.,data = salary_data)
salary_classifier
?naiveBayes
s
salary_test_pred <- predict(salary_classifier, salary_data)
salary_test_pred
table(salary_test_pred)
prop.table(table(salary_test_pred))
table(salary_test_pred)

library(gmodels)
CrossTable(salary_test_pred,salary_data$Salary  ,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
str(salary_data)

library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

df <- data.frame(A = factor(salary_data[1:14]),
                 B = 1:14, C = as.logical(c(1, 1, 0, 0, 1)),
                 D = letters[1:14],
                 E = paste(salary_data[1:14], letters[1:14]),
                 stringsAsFactors = FALSE)




set.seed(1234)
ind <- sample(2, nrow(salary_data),replace = TRUE, prob = c(0.7,0.3))
training <- salary_data[ind==1,]
testing <- salary_data[ind==2,]

library(caret)

inTraining <- createDataPartition(salary_data$Salary, p = .8, list = FALSE)
training <- salary_data[ inTraining,]
testing  <- salary_data[-inTraining,]

labeltraining = as.numeric(training[[14]])
datatraining = as.matrix(training[1:13])

labeltesting  = as.numeric(testing [[14]])
datatesting  = as.matrix(testing [1:13])

str(salary_data)
salary_data <- factor(salary_data)

xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)

datatesting<- factor(datatesting)
labeltraining <- factor(labeltraining)

salary <- factor(salary)


xgtesting <- xgb.DMatrix(data = datatesting, label = labeltesting)

















trainm <- sparse.model.matrix(Salary~., data = training)
View(training)
head(trainm)  
train_label <- training[,"Salary"]   
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(Salary~.,data=testing)
head(testm)
test_label <- testing[,"Salary"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

nc <- length(unique(train_label))
nc
xgb_param <- list("objective" = "multi:softmax", "bst:eta" = 0.005,"bst:max_depth" = 4,"num_class"=4,"nthread" = 4,"gamma" =0.5,"min_child_weight" = 3)

bst_model <- xgb.train(params = xgb_param, data = train_matrix, nrounds = 100)

bst <- xgboost(data = train_matrix, label = train_label, max_depth = 2, eta = 1,
               nrounds = 100)




ypred <- predict(model, xgtesting)

mean<-mean(labeltesting==ypred)

mean <- c(mean,mean(labeltesting==ypred) )


summary(mean)