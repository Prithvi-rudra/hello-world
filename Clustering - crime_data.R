crime_data <- read.csv(file.choose())

View(crime_data)
summary(crime_data)
attach(crime_data)
str(crime_data)
head(crime_data)
pairs(crime_data)
plot(crime_data)
plot(Murder~Rape, data = crime_data)
plot(crime_data$Murder, crime_data$Assault)

Z <- crime_data[,-1]
M <- apply(Z,2, mean)
S <- apply(Z,2,sd)
z <- scale(Z,M,S)

####### euclidean method############
d <- dist(z, method = "euclidean")

print(d, digits = 3)

hc.c <- hclust(d, method = "single",)
plot(hc.c, labels = X)                         ###complete linkage
plot(hc.c, hang = -1)
rect.hclust(hc.c, k = 5, border = "red")

hc.a <- hclust(d)
plot(hc.a, labels = X)                         ###complete linkage
plot(hc.a, hang = -1)
rect.hclust(hc.a, k = 5, border = "red")

member.c <- cutree(hc.c, 3)  ##membership
member.a <- cutree(hc.a, 3)

table(member.a, member.c)

aggregate(z, list(member.c), mean)
aggregate(z, list(member.a), mean)


library(cluster)
plot(silhouette(cutree(hc.c,3), d))

with(crime_data, text(Murder~Assault, labels = X, pos = 4, cex = .7))

membership <- as.matrix(member.a, member.c)

final <- data.frame(crime_data,membership)

View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
############ Manhattan method

d <- dist(z, method = "Manhattan")

print(d, digits = 3)

hc.c <- hclust(d, method = "single",)
plot(hc.c, labels = X)                         ###complete linkage
plot(hc.c, hang = -1)
rect.hclust(hc.c, k = 10, border = "red")

hc.a <- hclust(d)
plot(hc.a, labels = X)                         ###complete linkage
plot(hc.a, hang = -1)
rect.hclust(hc.a, k = 8, border = "red")

member.c <- cutree(hc.c, 3)  ##membership
member.a <- cutree(hc.a, 3)

table(member.a, member.c)

aggregate(z, list(member.c), mean)
aggregate(z, list(member.a), mean)


library(cluster)
plot(silhouette(cutree(hc.c,3), d))

with(crime_data, text(Murder~Assault, labels = X, pos = 4, cex = .7))

membership <- as.matrix(member.a, member.c)

final <- data.frame(crime_data,membership)

View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)



###########kmeans################
library(ggmap)
library(plyr)
x <- runif(50)
y <- runif(50)
data <- cbind(x,y)
plot(data)
km <- kmeans(data,4)
str(km)
library(animation)

k <- kmeans(z,3)
k
k$cluster
k$centers

k <- kmeans.ani(data,4)
?hclust
normalized_data <- scale(crime_data[2:5])

#################COMPLETE METHOD#####################
distance <- dist(normalized_data,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit)
plot(fit, hang=-1)
group <- cutree(fit, k=5)
rect.hclust(fit,k=5,border = "red")
membership <- as.matrix(group)
final <- data.frame(crime_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

####################  SINGLE METHOD
fit <- hclust(d,method = "single")
plot(fit)
plot(fit, hang=-1)
group <- cutree(fit, k=5)
rect.hclust(fit,k=5,border = "red")
membership <- as.matrix(group)
final <- data.frame(crime_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

##################   average METHOD

fit <- hclust(d,method = "average")
plot(fit)
plot(fit, hang=-1)
group <- cutree(fit, k=5)
rect.hclust(fit,k=5,border = "red")
membership <- as.matrix(group)
final <- data.frame(crime_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]


##################   mcquitty METHOD

fit <- hclust(d,method = "mcquitty")
plot(fit)
plot(fit, hang=-1)
group <- cutree(fit, k=5)
rect.hclust(fit,k=5,border = "red")
membership <- as.matrix(group)
final <- data.frame(crime_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

set.seed(123)
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

n = 100
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)

mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


install.packages("fpc")
library(fpc)
pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(d, pamk.best$nc))
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(d, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

#### Another approach to diagnosing how many clusters suit the data. In this case we try 1 to 10 groups######

require(vegan)
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(d), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters
plot(d_clust)

install.packages("apcluster")

library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), d)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
1# 4
heatmap(d.apclus)
plot(d.apclus, d)



library(cluster)
clusGap(d, kmeans, 10, B = 100, verbose = interactive())    #########output from Edwin Chen's implementation of the gap statistic

library(NbClust)
nb <- NbClust(d, diss=NULL, distance = "euclidean",
              method = "kmeans", min.nc=2, max.nc=15, 
              index = "alllong", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

d_dist <- dist(as.matrix(d))   # find distance matrix 
plot(hclust(d_dist)) 

install.packages("pvclust")
library(pvclust)
library(MASS)
data(Boston)
boston.pv <- pvclust(Boston)
plot(boston.pv)

elbow.k <- function(mydata){
  dist.obj <- dist(mydata)
  hclust.obj <- hclust(dist.obj)
  css.obj <- css.hclust(dist.obj,hclust.obj)
  elbow.obj <- elbow.batch(css.obj)
  k <- elbow.obj$k
  return(k)
}

install.packages("factoextra")
library(factoextra)   
fviz_nbclust(mtcars, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

###########IN THE ELBOW METHOD, 4 clusters solution suggested###############

