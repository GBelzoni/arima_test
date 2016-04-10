library(zoo)
library(xts)

data_raw = read.csv("./Inflation.csv")
plot(data_raw)
plot(data_raw$InflationIndex, type='l')
formdate = function(x) (paste("01-",x,sep=""))
dates = as.Date(formdate(data_raw$Date),format = "%d-%b-%Y")


zoo_data = zoo(data_raw$InflationIndex, order.by = dates)
plot(zoo_data)

xts_data = xts(data_raw$InflationIndex, order.by =dates)
plot(xts_data)
