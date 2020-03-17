wine <- read.csv(file.choose())
View(wine)
str(wine)
summary(wine)
View(wine[-1])
cor(wine)


install.packages("psych")
library(psych)

library(scales)
library(devtools)
library(remotes)
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)
install_github("vqv/ggbiplot", force = TRUE)


pairs.panels(wine, gap = 0, bg = c("red","green","yellow"), pch = 21)

PC <- prcomp(wine, center = TRUE, scale. = TRUE)




attributes(pc)

pc$center

pc$scale

mean(wine$Malic)

print(pc)

pairs.panels(pc$x, gap=0, bg = c("red","yellow","blue")[wine$Alcohol], pch = 21)

loadings(pc)
plot(pc)
screeplot(pc, type = "line", main = "Scree Plot")

biplot(pc)

install.packages("ggbiplot", force = TRUE)
library(ggbiplot)


pc$scores
pc$scores[1:14]


mydata <- cbind(data,pc$scores[,1:14])
clus_data <- mydata[8:10]
View(mydata)
normalized_data <- scale(clus_data)
d <- dist(normalized_data,method = "euclidean")
fit <- hclust(d,method = "complete")

prd <- predict(pc, wine)
prd

library(nnet)
wine$Alcohol<- relevel(wine$Alcohol)
wine <- multinom(Alcohol~., data = wine)

plot(fit)
plot(fit, hang=-1)
group <- cutree(fit,k=10)
rect.hclust(fit,k=10,border="red")
membership <- as.matrix(group)
final <- data.frame(data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

fa1 <- factanal(wine, factors = 3)
fa1

fa2 <- factanal(wine, factors = 3, rotation = "varimax")
fa2

fa3 <- factanal(wine, factors = 3, rotation = "varimax", scores = "regression")
fa3


###################hierarchial clustering###########

z <- wine[,-1]
View(z)

z <- scale(wine[,-1])   ###normalize the data
d <- dist(z)                    ##calculate the euclidean distance
print(d, digits = 3)

hc.c <- hclust(d)
plot(hc.c)
plot(hc.c, hang = -1)
rect.hclust(hc.c, k = 7, border = "red")


hc.a <- hclust(d)       ###dendrogram fro average linkage
plot(hc.a)
plot(hc.a, hang = -1)
rect.hclust(hc.a, k = 7, border = "red")


member.c <- cutree(hc.c, 3)  ##membership for the clusters
member.a <- cutree(hc.a, 3)
table(member.c,member.a)

membership <- as.matrix(member.c, member.a)

aggregate(z, list(membership), mean)

final <- data.frame(membership,wine)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)




###########kmeans################

library(plyr)
x <- runif(50)
y <- runif(50)
data <- cbind(x,y)
plot(data)
View(data)

k <- kmeans(z, 3)
k
k$cluster
k$centers

k <- kmeans(data,4)
str(k)
library(animation)
k <- kmeans.ani(data,4)
k$centers
normalize_data <- scale(EastWestAirlines[1:12])
d <- dist(normalize_data,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit)
plot(fit,hang=-1)
group <- cutree(fit,k=7)
rect.hclust(fit,k=7,border="red")
membership <- as.matrix(group)
final2 <- data.frame(membership,wine)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)


