glass <- read.csv(file.choose())
View(glass)
summary(glass)
str(glass)
table(glass$Type)
colnames(glass) <- c("refractive index","Sodium","Magnesium","Aluminum","Silicon","Potassium","Calcium","Barium","Iron","types")
View(glass)
round(prop.table(table(glass$types))*100, digits = 1)
normalize <- function(x){return((x-min(x)/(max(x)-(min(x)))))}
glass_n <- as.data.frame(lapply(glass[1:10],normalize))
summary(glass_n)      
glass_train <- glass_n[1:199,]
glass_test <- glass_n[200:214,]
glass_train_labels <- glass_n[1:199,1]
glass_train_labels
glass_train_labels <- glass_train_labels["diagnosis"]
glass_test_labels <- glass_n[200:214,1]
glass_test_labels  
glass_test_labels <- glass_test_labels["diagnosis"]
library(class)
glass_test_pred <- knn(train = glass_train, test = glass_test, cl=glass_train_labels,k = 21)
glass_z <- as.data.frame(scale(glass))
View(glass_z)
