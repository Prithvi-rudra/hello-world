salary_data <- read.csv(file.choose())
View(salary_data)
summary(salary_data)
salary_train <- salary_data[1:9000,]
salary_test <- salary_data[1:10000,]
salary_train
salary_test
library(kernlab)
salary_classifier <- ksvm(Salary~.,data = salary_train, kernal = "vanilladot")
salary_classifier
salary_prediction <- predict(salary_classifier, salary_test)
head(salary_prediction)
View(salary_test)
table(salary_prediction,salary_test$Salary)
agreement <- salary_prediction==salary_test$Salary
table(agreement)
prop.test(table(agreement))
salary_classifier_rbf <- ksvm(Salary~.,data=salary_train, kernal = "rbfdot")
salary_prediction_rbf <- predict(salary_classifier_rbf,salary_test)
table(salary_prediction_rbf,salary_test$Salary)
agreement_rbf <- salary_prediction_rbf==salary_test$Salary
table(agreement_rbf)
prop.test(table(agreement_rbf))
