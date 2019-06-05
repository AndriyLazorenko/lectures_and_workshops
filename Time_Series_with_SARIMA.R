rm(list=ls(all=TRUE))

# Generate a stationary dataset

sim = arima.sim(list(ar=c(0.4,0.5), ma=c(0.9, -0.4)), n = 10000)
plot(sim)

# Load datasets
JJ = JohnsonJohnson
plot(JJ, main='Johnson&Johnson earnings per share', col='blue', lwd=3)

SUV<-read.csv("Presentations/TimeSeries/monthly-sales-for-a-souvenir-sho.csv")
suv<-ts(SUV$Sales)
plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)

Milk<-read.csv("Presentations/TimeSeries/monthly-milk-production-pounds-p.csv")
milk<-Milk$Pounds
plot(milk, main="Monthly milk production in pounds")

BTC<-read.csv("Presentations/TimeSeries/coinbaseUSD_1-min_data_2014-12-01_to_2019-01-09_filled.csv")
btc<-BTC$Open
plot(btc, main="Opening prices of BTC in USD equivalent")

# Transforming datasets into stationary ones
par(mfrow=c(2,2))

plot(JJ, main='Johnson&Johnson earnings per share', ylab='', col='blue', lwd=3)
plot(log(JJ), main='Log-transform of Johnson&Johnson earnings per share', ylab='', col='red', lwd=3)
plot(diff(log(JJ)), main='Differenced Log-transorm of Johnson&Johnson earnings per share', ylab='', col='brown', lwd=3)
plot(diff(diff(log(JJ)),4), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)
jj_transformed = diff(diff(log(JJ)),4)

plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transform of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transform without trend and seasonality', ylab='', col='green', lwd=3)
suv_transformed = diff(diff(log(suv)),12)

# Plots of ACF, PACF (without S taken into account)
par(mfrow=c(3,1))
plot(diff(log(JJ)), main='Log-return of Johnson&Johnson earnings per share')
acf(diff(log(JJ)), main='Autocorrelation Function of log-return of JJ')
pacf(diff(log(JJ)), main='Partial Autocorrelation Function of log-return of JJ')

plot(jj_transformed, main='Log-return of Johnson&Johnson earnings per share without seasonality')
acf(jj_transformed, main='ACF')
pacf(jj_transformed, main='PACF')

plot(diff(log(suv)), main='Differenced log-transform of sales')
acf(diff(log(suv)), main="ACF of Differenced log-transform of sales")
pacf(diff(log(suv)), main="PACF of Differenced log-transform of sales")

plot(suv_transformed, main='Log-transform of sales without trend and seasonality')
acf(suv_transformed, main='ACF')
pacf(suv_transformed, main='PACF')

# Determining best model for the time series JJ

d=1
DD=1
per=4
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=8){
          model<-arima(x=log(JJ), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# Residuals test
par(mfrow=c(1,1))
require(astsa)

sarima(log(JJ), 0,1,1,1,1,0,4)
sarima(log(suv), 1,1,0,0,1,1,12)

# Determining best model for the time series suv

d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}



# Forecasting

require(forecast)
model<- arima(x=log(suv), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
f = forecast(model,48)
plot(f)
plot.ts(c(suv,exp(f$mean)), main='Monthly sales + Forecast', ylab='', col='blue', lwd=3)

a<-sarima.for(log(JJ),8,0,1,1,1,1,0,4)
plot.ts(c(JJ,exp(a$pred)), main='Quaterly earnings per share + Forecast', ylab='', col='blue', lwd=3)

# Wiki dataset
require(dplyr)
WIKI <- read.csv("Presentations/TimeSeries/train_1.csv")
str(WIKI)
wiki = select(WIKI, -c("Page"))
wiki = t(wiki)

# Explore individual pages
plot(ts(wiki[,10]))
page = ts(wiki[,10])
plot(diff(page))

# Focus on overall pages attendence
all = rowSums(wiki, na.rm=TRUE)
plot(all)

# 85 % of data goes to train - the rest is test
tr_test_point <- floor(0.85 * length(all))
train <- all[1:tr_test_point]
test <- all[tr_test_point: length(all)]

par(mfrow=c(3,1))
plot(all, main="Overall daily attendence of Wikipedia")
acf(all, main="ACF")
pacf(all, main="PACF")

d = diff(all)
plot(d, main="Differenced overall daily attendence of Wikipedia")
acf(d, 50, main="ACF")
pacf(d, 50, main="PACF")

dd = diff(d, 7)
plot(dd, main="Differenced overall daily attendence of Wikipedia without seasonality")
acf(dd, 50, main="ACF")
pacf(dd, 50, main="PACF")

d=1
DD=1
per=7
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:4){
      for(j in 1:3){
        if(p+d+q+i+DD+j<=16){
          model<-arima(x=train, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# Swear on the residuals
sarima(train, 1,1,1,1,1,2,7)
sarima(train, 1,1,1,2,1,2,7)
sarima(train, 1,1,1,4,1,2,7)
sarima(train, 1,1,1,3,1,2,7)

forecast_num_days = length(test)

par(mfrow=c(3,1))
a<-sarima.for(train, 84,1,1,1,3,1,2,7)
plot.ts(c(train,a$pred), main='Historical users + Forecast', ylab='', col='blue', lwd=2)
plot.ts(all, main="Wikipedia actual traffic", ylab="", col="green", lwd=2)

par(mfrow=c(1,1))
ts.plot(ts(all), ts(c(train, a$pred)), gpars = list(col = c("green", "blue")))

# Metrics
rmse(a$pred, test)

# Try a couple more
par(mfrow=c(2,1))
a <- sarima.for(train, 84,1,1,1,4,1,2,7)
ts.plot(ts(all), ts(c(train, a$pred)), gpars = list(col = c("green", "blue")))
rmse(a$pred, test)

par(mfrow=c(2,1))
a <- sarima.for(train, 84,1,1,1,1,1,2,7)
ts.plot(ts(all), ts(c(train, a$pred)), gpars = list(col = c("green", "blue")))
rmse(a$pred, test)

par(mfrow=c(2,1))
a <- sarima.for(train, 84,1,1,1,2,1,2,7)
ts.plot(ts(all), ts(c(train, a$pred)), gpars = list(col = c("green", "blue")))
rmse(a$pred, test)

###
par(mfrow=c(2,1))
a <- sarima.for(log(train), 84,1,1,1,2,1,2,7)
ts.plot(ts(all), ts(c(train, exp(a$pred))), gpars = list(col = c("green", "blue")))
rmse(exp(a$pred), test)

# Simple exponential smoothing: motivation
par(mfrow=c(1,2))
hist(rain.data, main="Annual London Rainfall 1813-1912",
     xlab="rainfall in inches")
qqnorm(rain.data,main="Normal Plot of London Rainfall")
qqline(rain.data)

par( mfrow=c(3,1) )
plot.ts(rain.ts, main="Annual London Rainfall 1813-1912",
        xlab="year", ylab="rainfall in inches")
acf(rain.ts, main="ACF: London Rainfall")
pacf(rain.ts, main="PACF: London Rainfall")

auto.arima(rain.ts)

