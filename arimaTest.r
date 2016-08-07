ts = arima.sim(n = 63, list(order=c(2,1,2),ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),sd = sqrt(0.1))
ts = arima.sim(n = 63, list(order=c(2,1,0),ar = c(0.2, 0.7),sd = sqrt(0.1)))
ts = arima.sim(n = 1000, list(order=c(2,1,0),ar = c(0.0, 0.7),sd = sqrt(0.1)))
ts = arima.sim(n = 100, list(order=c(2,1,0),ar = c(0.7, 0.0),sd = sqrt(0.1)))
ts = arima.sim(n = 1000, list(order=c(2,1,0),ar = c(-0.4, 0.1),sd = sqrt(0.1)))
ts = arima.sim(n = 1000, list(order=c(0,1,2),ma = c(0.5, 0.1),sd = sqrt(0.1)))

plot(ts)
plot(diff(ts))
sd(diff(ts))



acf(diff(ts))
pacf(diff(ts))

#Simulate arim model
ts = arima.sim(n = 10000, list(order=c(2,1,2),ar = c(0.9, -0.4), ma = c(-0.2, 0.25)),sd = sqrt(0.1))
plot(ts)

#Train an arima model
train = ts[0:(length(ts)/2)]
mod1 = Arima(train,order = c(2,1,2))

start_test = (length(ts)/2)+1
#Make test data to compare to
test = window(ts, start = start_test, end = start_test+500)

#predict using fitted model
predictions = predict(mod1,100)
plot(predictions$pred)
plot(predictions$pred,ylim=c(48, 48.6))
predictions$pred


#Simulate arima models
tss = list()
for( i in 1:500){
  tstmp = arima.sim(n = 100, list(order=c(2,1,2),ar = c(0.9, -0.4), ma = c(-0.2, 0.25)),sd = sqrt(0.1))
  tstmp = ts( tstmp, start= 5001)
  tstmp = tstmp + train[(length(ts)/2)]
  tss[[i]] = tstmp 
}



a = c(1)
b= 2
tss=c(tss,b)
tss[[2]]
library(scales)
plot(predictions$pred,ylim=c(40, 60))
for(i in 1:500){
  
  lines(tss[[i]],col=alpha('red',0.05))
  
  
}

lines(predictions$pred + predictions$se, col='blue')
lines(predictions$pred - predictions$se, col='blue')
lines(predictions$pred + 2*predictions$se, col='blue')
lines(predictions$pred - 2*predictions$se, col='blue')

plot(predictions$se)


#Rolling predictions
library(forecast)

refit = Arima(train, model=mod1)
plot(fitted(refit))
lines(test, color='r')

?predict.arima0
frc = forecast.Arima(mod1,xreg = test[0:4])

plot(fitted(mod1)[1:500])

fitted_val = fitted(mod1)
fitted_refit = fitted(refit) 

refit_vals = fitted_refit+ refit$residuals
refit_vals[1:10]
train[1:10]

(mod1$residuals + fitted)[1:10]

refit_test = Arima(test, model=mod1)

plot(test[1:20])

plot(ts[5002:5200])
lines(fitted(refit_test)[1:200], col = 'red')

predictions = c()

for(i in 1:length(test)){
  i=2
  j = 5000 + i
  this_train = ts[j-100:j]
  this_refit = Arima(this_train, model = mod1)
  this_pred = predict(this_refit,h=1)$pred[1]
  
  predictions = c(predictions,this_pred)
  
}


lines(predictions[1:200],col='blue')

train[500]

fitted_refit[1:10]
fitted[1:10]

#########################
diff_ts = ts
plot(diff_ts)

lag1 = diff_ts[2:length(ts)]
lag2 = diff_ts[3:length(ts)]
resids = mod1$residuals
resids1 = resids[2:length(resids)]
resids2 = resids[3:length(resids)]

model_data = data.frame(1:100)
model_data['lag1'] = lag1[1:100]
model_data['lag2'] = lag2[1:100]
model_data['res1'] = 0#resids1[1:100]
model_data['res2'] = 0#resids2[1:100]

coef = mod1$coef

myvals = apply(model_data,MARGIN = 1, function(x){ x['lag1']*coef['ar1'] + x['lag2']*coef['ar2']+ x['res1']*coef['ma1']+x['res2']*coef['ma2']})

model_data

plot(fitted(mod1)[3:100], type='l')
plot(ts[3:100])



lines(myvals[2:100],col='red')

lines(ts[2:100],col='blue')

fitted(mod1)[1:20]




mod1$model
plot(frc)
