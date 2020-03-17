Plastic <- read.csv(file.choose())
View(Plastic)
summary(Plastic)
x <- data.frame(outer(rep(month.abb,length=60),month.abb,"==") +0)
colnames(x) <- month.abb
View(x)
Plastic_Sales <- cbind(x,Plastic)
View(Plastic_Sales)
colnames(Plastic_Sales)
Plastic_Sales["t"] <- c(1:60)
Plastic_Sales["log_Sales"] <- log(Plastic_Sales["Sales"])
Plastic_Sales["t_square"] <- Plastic_Sales["t"]*Plastic_Sales["t"]
View(Plastic_Sales)
attach(Plastic_Sales)

train <- Plastic_Sales[1:29,]
test <- Plastic_Sales[30:60,]

linear_model <- lm(Sales~t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval = "predict", newdata = test))
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear

expo_model <- lm(log_Sales~t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = "predict", newdata = test))
rmse_expo <- sqrt(mean((test$Sales-expo_pred$fit)^2, na.rm = T))
rmse_expo

quad_model <- lm(Sales~t+t_square, data = train)
summary(quad_model)
quad_pred <- data.frame(predict(quad_model, interval = "predict", newdata = test))
rmse_quad <- sqrt(mean((test$Sales-quad_pred$fit)^2, na.rm = T))
rmse_quad

sea_add_model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

Add_sea_Quad_model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

multi_sea_model <- lm(log_Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, interval="predict",newdata=test))
rmse_multi_sea <- sqrt(mean((test$Sales-multi_sea_pred$fit)^2, na.rm = T))
rmse_multi_sea

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- cbind("model","rmse")
View(table_rmse)

write.csv(Plastic_Sales, file="test_data", row.names = F)
getwd()

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Plastic_Sales)
summary(Add_sea_Quad_model_final)

write.csv(Coke_Sales, file="test_pred", row.names = F)
getwd()


test_data <- read.csv(file.choose())
View(test_data)
pred_new <- predict(Add_sea_Quad_model_final, newdata = test_data, interval = 'predict')
pred_new
plot(Add_sea_Quad_model_final)

acf(Add_sea_Quad_model_final$residuals, lag.max = 10)

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))
A$residuals
ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

library(forecast)

errors_12 <- forecast(A, h = 12)

View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast

predicted_new_values <- pred_new + future_errors


library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth) # forsmoothing and MAPE
install.packages("tseries")
library(tseries)

Plastic_Sales1 <- HoltWinters(Plastic_Sales$Sales, alpha=0.2, beta=0.1, gamma=FALSE)

Plastic_Sales1

Plastic_Sales_predict<-data.frame(predict(Plastic_Sales1,n.ahead = 4))
Plastic_Sales_predict

Plastic_mape<-MAPE(Plastic_Sales_predict$fit,test)*100
hwab_mape

plot(forecast(Plastic_Sales1,h=4))

Plastic_Sales_predict <- predict(Plastic_Sales1, n.ahead = 10, prediction.interval = TRUE)
Plastic_Sales_predict


Plastic_Sales1$fitted

plot(0,type='n',axes=FALSE,ann=FALSE)

print(data, newpage = F)

library(MASS)

Plastic_strptime <- strptime(as.character(Plastic$Sales),format="%Y-%m-%d %H:%M:%S")
Plastic_strptime

Plastic1 <- as.POSIXct(Plastic$Sales,tz = "UTC")
class(Plastic$Sales)

?POSIXct
str(Plastic)
names(Plastic)

install.packages("lubridate")

library(mvtnorm)
library(libcoin)
library(rpart)
library(grid)
library(partykit)           
library(gbm)                 
library(doParallel)     
library(pROC)                 
library(corrplot)  
library(psych)
library(Hmisc)
library(lubridate)

all(Plastic ==Plastic)

require(gridExtra)

library(gridExtra)

plot1 <-qplot(Plastic$Sales,Plastic$Month,xlab='Month',ylab='sales',geom="line")+theme_grey(base_size = 18) 
plot1

plot2 <-qplot(Plastic$Sales[1:60],Plastic$Month[1:60],xlab='month',ylab='sales',geom="line")+theme_grey(base_size = 18) 
plot2

grid.arrange(plot1, plot2, nrow=2)
dev.off()

png('Sales_histogram_boxplot2_Jan29.png',width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,1))
hist(Plastic$Sales,main="",xlab = "months",breaks = 40,
     col='lightblue',xlim=c(0,1200),ylim=c(0,9000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

boxplot(Plastic$Sales,
        boxfill = "lightblue",horizontal=TRUE,ylim=c(0,1200),xlab="months",frame=F,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

install.packages("SimInf")

library(SimInf)

n <- 1000
u0 <- data.frame(S = rep(999, n), I = rep(1, n), R = rep(0, n))

tspan <- seq(from = 1, to = 180, by = 7)

model <- SIR(u0 = u0, tspan = tspan, beta = 0.16, gamma = 0.077)

set.seed(123)
result <- run(model = model, threads = 1)
result

plot(result)
plot(result, node = 1:10, range = FALSE)

head(trajectory(model = result, node = 1))

events

u0 <- data.frame(S = rep(0, 5), I = rep(0, 5), R = rep(0, 5))

add <- data.frame(event = "enter", time = rep(1:10, each = 5), node = 1:5, dest = 0, n = 1:5, proportion = 0, select = 1, shift = 0)

infect <- data.frame(event = "enter", time = 25, node = 5,
                      dest = 0, n = 1, proportion = 0, select = 2, shift = 0)

move <- data.frame(event = "extTrans", time = 35:45, node = c(5, 5, 5, 5, 4, 4, 4, 3, 3, 2, 1), dest = c(4, 3, 3, 1, 3, 2, 1, 2, 1, 1, 2), n = 5, proportion = 0, select = 4, shift = 0)
remove <- data.frame(event = "exit", time = c(70, 110), node = rep(1:5, each = 2), dest = 0, n = 0, proportion = 0.2,select = 4, shift = 0)

events = rbind(add, infect, move, remove)
model <- SIR(u0 = u0, tspan = 1:180, events = events, beta = 0.16, gamma = 0.077)
set.seed(3)
result <- run(model, threads = 1)
plot(result, node = 1:5, range = FALSE)

set.seed(123)
mean(replicate(n = 1000, {
nI <- trajectory(run(model = model, threads = 1), node = 1:4)$I
sum(nI) > 0
}))


write.csv(predicted_new_values, file = "predicted_new_values1.csv", row.names = F)
getwd()
