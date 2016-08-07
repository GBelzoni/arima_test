births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
library(zoo)
library(xts)
library(TTR)
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
sourvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")


kingstimeseries = ts(kings)
birthtimeseries = ts(births,frequency=12,start=c(1946,1))
sourvenirtimeseries = ts(sourvenir, frequency = 12, start = c(1987,1))

plot.ts(birthtimeseries)

kingsSMA3 = SMA(kingstimeseries,n=3)
plot(kingsSMA3)

decomp1 = decompose(birthtimeseries)
birthtimeseriesSMA3 = SMA(birthtimeseries)
plot(decomp1)

plot(birthtimeseriesSMA3)

bts_m_trend = birthtimeseries - birthtimeseriesSMA3
plot(bts_m_trend)

j=1

monthAve = function(j){mean(bts_m_trend[seq(j,length(birthtimeseries),by = 12)],na.rm = T)}

monthAve(1)
plot(sapply(1:12,monthAve),type='l')

monthFactors = sapply(1:12,monthAve)
seasonComp = rep(monthFactors,14)
plot(seasonComp,type='l')
error = birthtimeseries - seasonComp - birthtimeseriesSMA3
plot(error)
plot(decomp1$seasonal)
plot(seasonComp,type='l')

