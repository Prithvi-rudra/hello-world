salary <- read.csv(file.choose())
View(salary)
summary(salary)
salary_train <- salary[1:24999,]
salary_test <- salary[25000:26653,]
salary_test
salary_train
library(kernlab)
salary_classifiers <- ksvm(Salary~.,data=salary_train, kernal = "vanilladot")
salary_prediction <- predict(salary_classifiers, salary_test)
head(salary_prediction)
table(salary_prediction,salary_test$Salary)
agreement <- salary_prediction==salary_test$Salary
table(agreement)
prop.test(table(agreement))
salary_classifiers_rbf <- ksvm(Salary~.,data=salary_train,kernal="rbfdot")
salary_prediction_rbf <- predict(salary_classifiers_rbf,salary_test)
table(salary_prediction_rbf,salary_test$Salary)
agreement_rbf <- salary_prediction_rbf==salary_test$Salary
table(agreement_rbf)
prop.test(table(agreement_rbf))
