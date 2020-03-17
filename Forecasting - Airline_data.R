library(readxl)

Airlines_Data <- read_excel(file.choose())
View(Airlines_Data)
summary(Airlines_Data)
x <- data.frame(outer(rep(month.abb,length=96), month.abb,"==") +0)
colnames(x) <- month.abb
View(x)
flight_passenger <- cbind(Airlines_Data,x)
View(flight_passenger)
colnames(flight_passenger)
plot(flight_passenger)
hist(flight_passenger$Passengers)
flight_passenger["t"] <- c(1:96)
flight_passenger["log_Passengers"] <- log(flight_passenger["Passengers"])
flight_passenger["t_square"] <- flight_passenger["t"]*flight_passenger["t"]
attach(flight_passenger)
View(flight_passenger)
train <- flight_passenger[1:44,]
test <- flight_passenger[45:96,]

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear

expo_model <- lm(log_Passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

Quad_model <- lm( Passengers~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

Add_sea_Quad_model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

multi_sea_model <- lm(log_Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

write.csv(flight_passenger, file="Passengers.csv", row.names = F)

Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = flight_passenger)
summary(Add_sea_Quad_model_final)

Passengers <- read.csv(file.choose())
View(Passengers)
predict_new <- predict(Add_sea_Quad_model_final, newdata = Passengers, interval = "predict")
predict_new
predict_new <- as.data.frame(predict_new)

plot(Add_sea_Quad_model_final)

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

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
library(forecast)

errors_12 <- forecast(A, h = 12)

View(errors_12)

future_errors <- data.frame(errors_12)
class(future_errors)
future_errors <- future_errors$Point.Forecast


flight_Sales1 <- HoltWinters(flight_passenger, alpha=0.2, beta=0.1, gamma=FALSE)
flight_Sales1

flight_Sales_predict<-data.frame(predict(flight_Sales1,n.ahead = 4))
flight_Sales_predict

plot(forecast(flight_Sales1,h=4))

flight_Sales_predict <- predict(Plastic_Sales1, n.ahead = 10, prediction.interval = TRUE)
flight_Sales_predict

Plastic_Sales1$fitted

plot(0,type='n',axes=FALSE,ann=FALSE)

print(data, newpage = F)

library(MASS)

flight_strptime <- strptime(as.character(flight_passenger$Passengers),format="%Y-%m-%d %H:%M:%S")
flight_strptime


t <- structure(list(
  Datetime = structure(c(1L, 1L, 1L, 1L, 1L, 1L), 
                       .Label = "2016-10-19 00:00:00", 
                       class = "factor")), 
  .Names = "Datetime", 
  row.names = c(NA, -6L), 
  class = "data.frame")

flight1 <- as.POSIXct(t$Datetime, format="%Y-%m-%d %H:%M:%S")

str(flight_passenger)

names(flight_passenger)

all(Airlines_Data ==Airlines_Data)

library(gridExtra)

plot1 <-qplot(Airlines_Data$Passengers,Airlines_Data$Month,xlab='Month',ylab='sales',geom="line")+theme_grey(base_size = 18) 
plot1

plot2 <-qplot(Airlines_Data$Passengers[1:96],Airlines_Data$Month[1:96],xlab='month',ylab='sales',geom="line")+theme_grey(base_size = 18) 
plot2

grid.arrange(plot1, plot2, nrow=2)
dev.off()



png('Sales_histogram_boxplot2_Jan29.png',width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,1))
hist(cocacola$Sales,main="",xlab = "months",breaks = 40,
     col='lightblue',xlim=c(0,1200),ylim=c(0,9000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

boxplot(cocacola$Sales,
        boxfill = "lightblue",horizontal=TRUE,ylim=c(0,1200),xlab="months",frame=F,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()

u0 <- data.frame(S = rep(999, n), I = rep(1, n), R = rep(0, n))

tspan <- seq(from = 1, to = 180, by = 7)

model <- SIR(u0 = u0, tspan = tspan, beta = 0.16, gamma = 0.077)

set.seed(123)
result <- run(model = model, threads = 1)
result

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

predicted_new_values <- predict_new + future_errors

write.csv(predicted_new_values, file = "airline predicted_new_values.csv", row.names = F)
