#Chapter 1
data("AirPassengers")
AP = AirPassengers
AP
layout(1)
plot(AP)
plot(aggregate(AP))
boxplot(AP~cycle(AP))
plot(AP~cycle(AP),type='p')

attach(enempli)
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Maine.dat"
Main.month = read.table(www,header=T)
plot(Main.month)
attach(Main.month)
class(Main.month)

Main.month.ts = ts(Main.month,start=c(1996,1),frequency = 12)
plot(Main.month.ts)
Main.Feb = window(Main.month.ts, start=c(1996,2), freq = T)
plot(Main.Feb)
Feb.ratio = mean(Main.Feb)/mean(Main.month.ts)

www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/USunemp.dat"
US.month = read.table(www, header= T)
US.month.ts = ts(US.month$USun, start= c(1996,1), end=c(2006,10),freq=12)
plot(US.month.ts)


www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat"
Global = scan(www)
Global.ts = ts(Global, st = c(1856,1), end = c(2005,12), fr =12)
frequency(Global.ts)
Global.annual = aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)

New.series = window(Global.ts, start=c(1970,1), end=c(2005,12))
New.time = time(New.series)
plot(New.series); abline(reg=lm(New.series~ New.time))

plot(stl(New.series,s.window = 7))

dec1 = decompose(New.series,type='mult')
plot(dec1)

#beer data
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE = read.table(www, header= T)
head(CBE)
plot(CBE)
Elect.ts = ts(CBE[,1],start=1958,freq=12)
Beer.ts = ts(CBE[,2],start=1958,freq=12)
Choc.ts = ts(CBE[,3],start=1958,freq=12)
plot(cbind(Elect.ts, Beer.ts, Choc.ts))

Beertime = time(Beer.ts)
Beer.annual = aggregate(Beer.ts)
plot(Beer.annual)
boxplot(Beer.ts ~ cycle(Beer.ts))

decomp_beer = decompose(Beer.ts)
plot(decomp_beer)
m_beer = decomp_beer$trend
beer_mps = decomp_beer$trend + decomp_beer$seasonal
plot(m_beer,ylim=c(50,200))
lines(beer_mps)
lines(Beer.ts,col='red')

########### CHAPTER 2
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/wave.dat"
waveht = read.table(www, header= T)
waveht_ts = ts(waveht)
plot(ts(waveht))
plot.ts(waveht_ts[1:60])
acf_wave = acf(waveht_ts)
acf_wave$acf[2]

acf(AirPassengers)
dec_AP = decompose(AirPassengers, type = "mult")
plot(dec_AP)
plot(na.exclude(dec_AP$random))
acf(na.exclude(dec_AP$random))

#CH2 Exercises
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/varnish.dat"
varnish = read.table(www, header= T)
varnish
cor(varnish)
plot(vanish)
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/guesswhat.dat"
guesswhat_raw= read.table(www, header= T)
guesswhat = guesswhat_raw
plot(guesswhat)
cor(guesswhat)
guesswhat[guesswhat$x<(-25),]

#ch2.2
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/ch2ex2.dat"
bottles = read.table(www, header= T)
serendipity = c(39,35,16,18,7,22,13,18,20,9,-12,-11,-19,-9,-2,16)
cagey = c(47,-26,42,-10,27,-8,16,6,-1,25,11,1,25,7,-5,3)
bottles = data.frame( serendipity, cagey)

plot(ts(bottles$serendipity))
plot(ts(bottles$cagey))

plot(bottles$serendipity[1:15], bottles$serendipity[2:16])
plot(bottles$cagey[1:15], bottles$cagey[2:16])

acf(bottles$serendipity)
acf(bottles$cagey)

plot(Global.ts)
decomp_global = decompose(Global.ts)
plot(decomp_global)
deseasoned = Global.ts - decomp_global$seasonal - decomp_global$trend
sd(Global.ts)
sd(na.exclude(decomp_global$random))
plot(decomp_global$trend)
lines(decomp_global$trend + decomp_global$seasonal, col='red')
acf(na.exclude(decomp_global$random))


###Ex2.4
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Fontdsdt.dat"
FontRes = read.table(www, header= T)
FontRes_ts = ts(FontRes, frequency = 12)
head(FontRes)
plot.ts(FontRes_ts[1:50])
decomp_fond = decompose(FontRes_ts)
plot(decomp_fond)
acf(na.exclude(decomp_fond$random))



#########Chapter 3 exercises
#q1
k = 100
w = 1:100
x = w + k * rnorm(100)
y = w + k * rnorm(100)
ccf(x,y)

time  = 1 : 370
x = sin(2*pi*time/37)
y = sin(2*pi*(time+4)/37)
plot(x, type = 'l'); lines(y, col='red')
ccf(x,y)


#3.6
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/wine.dat"
wine.dat = read.table(www, header= T)
wine_ts = ts(wine.dat$sweetw, start = c(1980,1),frequency = 12)
plot(wine_ts)
hw_wine = HoltWinters(wine_ts, seasonal = "mult")
plot(hw_wine)
hw_wine$SSE
sqrt(hw_wine$SSE/length(wine_ts))
sd(wine_ts)

gamma = 0.2
hw_manual_wine = HoltWinters(wine_ts,gamma = gamma, seasonal="mult")
plot(hw_manual_wine)
sqrt(hw_manual_wine$SSE/length(wine_ts))


ln_wine_ts = log(wine_ts)
#hw_log_wine = HoltWinters(ln_wine_ts, seasonal = "mult")
hw_log_wine = HoltWinters(ln_wine_ts)

plot(hw_log_wine)
sd(exp(ln_wine_ts))
logfitted =hw_log_wine$fitted[,c("xhat")]
SSEln = sum((ln_wine_ts - logfitted)*(ln_wine_ts - logfitted))
sqrt(SSEln/length(ln_wine_ts))

SSE2 = sum((wine_ts - exp(logfitted))*(wine_ts - exp(logfitted)))

sqrt(SSE2/length(wine_ts))

plot(wine_ts)
lines(exp(logfitted), col='red')

#SS1PE if preedicting x_{t+1}^{hat} = x_t
simpleDiff = wine_ts - lag(wine_ts,k=-1)
SSEsimple = sum(simpleDiff*simpleDiff)
sqrt(SSEsimple/length(simpleDiff))


#fit to 1:j
#predict 1 ahead
#report diff

recal_hw = function(j,data){
  
  thisdat = ts(data[1:j],start = c(1980,1),frequency = 12)
  
  thisHW = HoltWinters(thisdat, seasonal = "mult")
  pred = predict(thisHW,1)
  thisDiff = data[j] - pred
  
  return(thisDiff)
  #return(c(pred, thisHW$alpha))
  
}

periods = 25:(length(wine_ts)-1)


wine_ts[25:length(wine_ts)]
diffMultOptim = sapply(periods, recal_hw, data= wine_ts)
sqrt(sum(diffMultOptim*diffMultOptim)/length(diffMultOptim))



#####CHAPTER 4 #############
www = "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/ApprovActiv.dat"
Build.dat = read.table(www, header= T)
App_ts = ts(Build.dat$Approvals, start = c(1996,1), freq = 4)
Act_ts = ts(Build.dat$Activity, start = c(1996,1), freq = 4)
ts.plot(Act_ts,App_ts , lty = c(3,1))
head(Build.dat)
acf(ts.union(App_ts,Act_ts))



###Chapter 11 ###############
set.seed(10)

x =0.01874617
y =0
x =rnorm(100); y = rnorm(100)
for(i in 2:100){
  x[i] = x[i-1] + rnorm(1)
  y[i] = y[i-1] + rnorm(1)
  }

plot(x, type='l')
plot(x,y)


#VAR models
library(tseries)
library(lmtest)
data(USeconomic)
US.ar = ar(cbind(GNP,M1), method = 'ols', dmean=T, intercept=F)
US.ar$ar
summary(US.ar)
US.ar
plot(US.ar$resid)
ts_temp = US.ar$resid[,1]
library(xts)

ts_xts = as.zoo(as.xts(ts_temp))
colnames(ts_xts) = "data"
plot(ts_xts)
bptest(ts_xts)
