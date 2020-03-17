zoo <- read.csv(file.choose())
View(zoo)
summary(zoo)
zoo <- zoo[-1]
View(zoo)
str(zoo)
round(prop.table(table(zoo))*100,digits = 1)
normalize <- function(x){return((x-min(x)/(max(x)-min(x))))}
zoo_n <- as.data.frame(lapply(zoo[1:17],normalize))
summary(zoo_n)
zoo_train <- zoo_n[1:70,]
zoo_test <- zoo_n[71:101,]
zoo_train_labels <- zoo_n[1:70,1]
zoo_train_labels
zoo_train_labels <- zoo_train_labels["diagnosis"]
zoo_test_labels <- zoo_n[71:101,1]
zoo_test_labels
zoo_test_labels <- zoo_test_labels["diagnosis"]
library(class)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl=zoo_train_labels, k = 21)
crossTable(x=zoo_test_labels ,y=zoo_test_pred, prop.chisq = FALSE)
zoo_z <- as.data.frame(scale(zoo))
summary(zoo_z)

