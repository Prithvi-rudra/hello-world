forest <- read.csv(file.choose())
View(forest)
summary(forest)
forest_train <- forest[1:349,]
forest_test <- forest[350:517,]
forest_train
forest_test
install.packages("kernlab")
library(kernlab)
forest_classifiers <- ksvm(month~.,data = forest_train, kernal = "vanilladot")
forest_prediction <- predict(forest_classifiers,forest_test)
head(forest_prediction)
table(forest_prediction,forest_test$month)
agreement <- forest_prediction==forest_test$month
table(agreement)
prop.test(table(agreement))
forest_classifiers_rbf <- ksvm(month~.,data = forest_train, kernal = "rbfdot")
forest_classifiers_rbf
forest_prediction_rbf <- predict(forest_classifiers_rbf,forest_test)
table(forest_prediction_rbf,forest_test$month)
agreement_rbf <- forest_prediction_rbf==forest_test$month
table(agreement_rbf)
prop.test(table(agreement_rbf))