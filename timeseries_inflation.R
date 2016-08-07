library(zoo)
library(xts)
library(forecast)
library(TTR)
library(lubridate)

data_raw = read.csv("./Inflation.csv")
plot(data_raw)
plot(data_raw$InflationIndex, type='l')
formdate = function(x) (paste("01-",x,sep=""))
dates = as.Date(formdate(data_raw$Date),format = "%d-%b-%Y")




zoo_data = zoo(data_raw$InflationIndex, order.by = dates)
plot(zoo_data)
plot(diff(log(zoo_data)))

xts_data = xts(data_raw$InflationIndex, order.by = as.yearqtr(dates))
xts_annual = to.yearly(xts_data)[,"xts_data.Close"]
xts_annual
plot(xts_annual)

xts_data_logdiff = diff(log(xts_data))
tail(xts_data_logdiff)
plot(xts_data_logdiff)
#xts_data_logdiff_annual = aggregate(xts_data_logdiff,by = year(dates), FUN=sum )
abline(h=0.05)
abline(h=0.00)

zoo2xts = function(zoodata){ as.xts(coredata(zoodata),order.by = index(zoodata))}
zoo2xts(xts_data_logdiff_annual)

ts_raw_dat = xts_data_logdiff
ts_data = ts(data= coredata(ts_raw_dat),frequency=4,c(1922,2))
plot(4*ts_data)

summary(ts_data)
adf.test(xts_data_logdiff[-1])


acf(xts_data_logdiff,na.action = na.pass)
seasonplot(ts_data)
decomp1=decompose(ts_data)
plot(decomp1)

arima1 = arima(xts_data_logdiff, c(5,0,5))
summary(arima1)
plot(arima1)
predict(arima1)
arima1$model

## Using intro timeseries

ts_data = ts(data_raw$InflationIndex, frequency=4, start= c(1922,2))
plot.ts(ts_data)
decomp_ts = decompose(ts_data, type = "mult")
plot(decomp_ts)

resid = na.exclude(decomp_ts$random)
acf(resid)
plot(resid)

#Do move ave holt wint
infl_mod1 = HoltWinters(ts(resid,frequency=4, start= c(1922,2)), beta=0, gamma=0)
infl_mod1 = HoltWinters(ts(resid,frequency=4, start= c(1922,2)))
infl_mod1
infl_mod1$SSE
plot(infl_mod1)
end(ts_data)
ts_dat_window = window(ts_data,start=c(1930,2),end=c(1990,2))
ts_dat_window_end = window(ts_data, start=c(1970,3))
plot(ts_dat_window)
infl_mod2 = HoltWinters(ts_dat_window, seasonal = "additive")

pred = predict(infl_mod2,n.ahead = 50)
ts.plot(ts_dat_window_end,pred, lty = 1:2)

infl_mod2

plot(infl_mod2)
sd(ts_data)
sd(infl_mod2$fitted)
infl_mod2$SSE

#Inflation analysis

#Check if unit root

#remove trends

#check seasonality

#do arima on remaining


#predict vs actual

#put trade on if  prediction is +x

#take trade off if prediction is -y


