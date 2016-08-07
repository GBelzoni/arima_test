#References online book https://www.otexts.org/fpp/using-r

library('fpp')
plot(melsyd[,"Economy.Class"], 
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")
xxx =melsyd

seasonplot(a10,ylab="$ million", xlab="Year", 
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(a10)
Acf(beer)
Acf
xxx= beer

plot((elec), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)


#Exercises chap4
#Q1
dataa = econsumption[-c(8),]
plot(Mwh ~ temp, data=dataa)
fit  <- lm(Mwh ~ temp, data=dataa)
summary(fit)
plot(residuals(fit) ~ temp, data=dataa)
fc = forecast(fit, newdata=data.frame(temp=c(10,35)))
summary(fc)

plot(fc)
resids = residuals(fit)
acf(resids)
hist(residuals(fit))

#has an outlier - removing probably gets better fit
#Errors aren't normal so probably deviate from mean differently from confidence interval

#Q2 
dataa = olympic[-c(1),]
dataa$logTime = log(dataa$time)
modOlympic <- lm(logTime ~ Year, data = dataa )
plot(log(time) ~ Year, data = dataa)
abline(modOlympic)
hist(modOlympic$residuals)
fc = forecast(modOlympic, newdata = data.frame(Year=c(2000,2004,2008,2012)))
summary(fc)
plot.forecast(fc)
exp(fc$lower)
c(43.84,44.00,43.75)

plot(modOlympic$residuals)

#CHAPTER 5 
#5.6
length(fuel)
Cityp <- pmax(fuel$City-25,0)
fit3 <- lm(Carbon ~ City + I(City^2) + I(City^3) + I(Cityp^3), data=fuel)
x <- 15:50; z <- pmax(x-25,0)
fcast3 = forecast(fit3,newdata = data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast3$mean,col="red")


Cityp <- pmax(fuel$City-25,0)
fit2 <- lm(Carbon ~ City + Cityp, data=fuel)
x <- 15:50; z <- pmax(x-25,0)
fcast2 <- forecast(fit2, newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast2$mean,col="red")

#Chap 5 - exercises
library(reshape2)
xxx=fancy
plot(fancy)
plot(log(fancy))
logFancy = log(fancy)

monthNum = 1:12#rep(1:12, length(fancy)/12)
years = 1987:1993

shopd = data.frame(matrix(0, nrow= length(years)*length(monthNum),2))

head(shopd)

i=1
for( yr in years ) {
  
  for( month in monthNum) {
    
    shopd[i,1] = yr
    shopd[i,2] = month
    i=1+i
  }
}

colnames(shopd) = c("year","month")
shopd$fancy = fancy
shopd$logfancy = logFancy


shopd$feb = (2 == monthNum)*1
shopd$mar = (3 == monthNum)*1
shopd$apr = (4 == monthNum)*1
shopd$may = (5 == monthNum)*1
shopd$jun = (6 == monthNum)*1
shopd$jul = (7 == monthNum)*1
shopd$aug = (8 == monthNum)*1
shopd$sep = (9 == monthNum)*1
shopd$oct = (10 == monthNum)*1
shopd$nov = (11== monthNum)*1
shopd$dec = (12 == monthNum)*1

shopd$festivalDummy = 1*(shopd$mar == 1 & shopd$year >= 1988)

shopd$trend = 1:dim(shopd)[1]



modelFancy = lm("logfancy ~ trend + feb + mar + apr + may + jun + jul +
                            aug + sep + oct + nov + dec + festivalDummy", data= shopd)
plot(modelFancy$residuals)






library(xts)
xts(fancy)

head(fancy)
xxx = start(fancy)

