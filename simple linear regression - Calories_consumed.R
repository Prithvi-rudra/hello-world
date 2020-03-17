calories_consumed <- read.csv(file.choose())
View(calories_consumed)
summary(calories_consumed)
plot(calories_consumed)
plot(calories_consumed$Weight.gained..grams.,calories_consumed$Calories.Consumed)
attach(calories_consumed)
library(lattice)
dotplot(calories_consumed$Weight.gained..grams.,main="Dot Plot of Weight.gained..grams")
dotplot(calories_consumed$Calories.Consumed,main="Dot Plot of Calories.Consumed")
cor(Calories.Consumed,Weight.gained..grams.)
reg <- lm(Calories.Consumed~Weight.gained..grams.,data = calories_consumed)
summary(reg)
confint(reg,level = 0.95)
pred <- predict(reg,interval="predict")
reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed),data = calories_consumed)
summary(reg_sqrt)
pred1 <- predict(reg_sqrt,interval="predict")
pred1 <- as.data.frame(pred1)
reg_log <- lm(Weight.gained..grams.~log(Calories.Consumed),data = calories_consumed)
summary(reg_log)
reg1 <- lm(log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed*Calories.Consumed),data = calories_consumed)
summary(reg1)
reg2 <- predict(reg1)
reg3 <- exp(reg2)
library(ggplot2)
ggplot(data = calories_consumed, aes(x = Calories.Consumed + I(Calories.Consumed^2) + I(Calories.Consumed^3), y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y=reg3))
plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)
reg_log <- lm(Weight.gained..grams. ~ log(Calories.Consumed))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calories_consumed))
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
plot(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed, log(Weight.gained..grams.))
reg_exp <- lm(log(Weight.gained..grams.) ~ Calories.Consumed)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
logat <- predict(reg_exp)
at <- exp(logat)
error = calories_consumed$Weight.gained..grams. - at
error
sqrt(sum(error^2)/nrow(calories_consumed))
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")
plot(Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)
cor(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

reg2degree <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = wc_at$AT - expy

sqrt(sum(err^2)/nrow(calories_consumed))  

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

ggplot(data = calories_consumed, aes(x = Calories.Consumed + I(Calories.Consumed^2), y = log(Weight.gained..grams.))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=logpol))
reg3degree<-lm(log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed*Calories.Consumed)+I(Calories.Consumed*Calories.Consumed*Calories.Consumed))
reg3degree<-lm(log(Weight.gained..grams.)~Calories.Consumed + I(Calories.Consumed*Calories.Consumed) + I(Calories.Consumed*Calories.Consumed*Calories.Consumed))
summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
ggplot(data = calories_consumed, aes(x = Calories.Consumed + I(Calories.Consumed^2) + I(Calories.Consumed^3), y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed+I(Calories.Consumed^2)+I(Calories.Consumed^3), y=expy3))
