library(data.table)
library(lubridate)
library(panelaggregation)
library(dplyr)
library(urca)
library(forecast)
library(tidyverse)

train=fread("GercekZamanliTuketim-01012017-08012021.csv")
test=fread("test-09012021-23012021.csv")

all_dt=rbind(train,test)

setnames(all_dt, "Tüketim Miktarı (MWh)", "consumption")
setnames(all_dt, "Tarih", "date")
setnames(all_dt, "Saat", "hour")
all_dt$consumption=gsub('\\.', '', all_dt$consumption)
all_dt$consumption=gsub('\\,', '.', all_dt$consumption)
all_dt$hour=gsub(':00', '', all_dt$hour)

all_dt$consumption=as.numeric(all_dt$consumption)
all_dt$hour=as.numeric(all_dt$hour)
all_dt$date=dmy(all_dt$date)
str(all_dt)

perf_dt=function(type,actual,forecast){
    name=type
    n=length(actual)
    error=actual-forecast
    mean=mean(actual)
    sd=sd(actual)
    FBias=sum(error)/sum(actual)
    MPE=sum(error/actual)/n
    MAPE=sum(abs(error/actual))/n
    RMSE=sqrt(sum(error^2))/n
    MAD=sum(abs(error))/n
    WMAPE=MAD/mean
    l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
    return(l)
}

df =all_dt %>% 
  group_by(date) %>% 
  summarise(consumption = mean(consumption))
dt=as.data.table(df)

library(plyr)

train=filter(dt,date<="2021-01-07")
test=filter(dt,date>"2021-01-07")

train_ts=ts(train$consumption,freq=7)
test_ts=ts(test$consumption,freq=7)

ts.plot(train_ts,xlab = "Date", ylab = "Consumption",main="Consumption from '2017-01-01' to '2021-01-23'")

unt_test=ur.kpss(train_ts) 
summary(unt_test)

consumption_decom=decompose(train_ts,type='add')
plot(consumption_decom)

unt_test_decomp=ur.kpss(consumption_decom$random) 
summary(unt_test_decomp)

plot(consumption_decom$random,ylab="Residuals after Decomposition",main="Residuals from '2017-01-01' to '2021-01-23'")

plot(acf(consumption_decom$random,na.action = na.pass, plot = FALSE),main="ACF plot for Decomposition")

plot(pacf(consumption_decom$random,na.action = na.pass, plot = FALSE),main="PACF plot for Decomposition")

train[,lag_7:=shift(consumption,7)]
train[,differ:=consumption-lag_7]

unt_test_differ=ur.kpss(train$differ) 
summary(unt_test_differ)

plot(acf(train$differ,na.action = na.pass, plot = FALSE),main="ACF plot for Difference in Consumption")

plot(pacf(train$differ,na.action = na.pass, plot = FALSE),main="PACF plot for Difference in Consumption")

plot(consumption_decom$random,ylab="Residuals after Decomposition",main="Consumption from '2017-01-01' to '2021-01-23'")

out_and<-tsoutliers(consumption_decom$random)

rep_val=out_and$replacements
ref_val=train[out_and$index]$adj_cons

train[out_and$index]

train[,adj_cons:=consumption]
train[out_and$index]$adj_cons=rep_val+ref_val

feat_dt=fread("C:/Users/bahad/GitHub/360project/all_features_df.csv")
feat_dt$date=as.Date(feat_dt$date)

train_feat=merge(train,feat_dt[,c(1,2)],by.x="date",by.y="date")
train_feat[out_and$index][holiday_val_factor==0]$holiday_val_factor=8

train_feat$month=month(train_feat$date)

train_feat[,year_day:=yday(date)]
train_feat[,lag_14:=shift(consumption, 14)]

train_feat[holiday_val_factor==1,consumption:=lag_7]
train_feat[1]$consumption=train_feat[366]$consumption

train_feat[year_day==366]$consumption=train_feat[year_day==366]$lag_7

train_feat[year_day==365]$consumption=train_feat[year_day==365]$lag_7

train_feat[holiday_val_factor==2]$consumption=train_feat[holiday_val_factor==2]$lag_7

for(i in 3:8){
    train_feat[holiday_val_factor==i]$consumption=train_feat[holiday_val_factor==i]$lag_14
}

sub_decomp=decompose(ts(train_feat$consumption,freq=7),type='add')
plot(sub_decomp)

out_new<-tsoutliers(sub_decomp$random)

train_feat[out_new$index,holiday_val:=rep(8,length(train_feat[out_new$index]$holiday_val))]

train_feat[holiday_val==8,consumption:=lag_14]

new_decomp=decompose(ts(train_feat$consumption,freq=7),type='add')
plot(new_decomp)

unt_test_final=ur.kpss(new_decomp$random) 
summary(unt_test_final)

tsdisplay(new_decomp$random,main="General Look of Input")

tsdisplay(new_decomp$random,main="General Look of Input")

ar1=arima(new_decomp$random,order=c(1,0,0))
ar1

tsdisplay(residuals(ar1),main="General Look of AR-1 Model")

ar2=arima(new_decomp$random,order=c(2,0,0))
ar2

tsdisplay(residuals(ar2),main="General Look of AR-2 Model")

ar3=arima(new_decomp$random,order=c(3,0,0))
ar3

tsdisplay(residuals(ar3),main="General Look of AR-3 Model")

pred_dt=rbind.fill(train_feat,test)
pred_dt=pred_dt[,c(1,2)]
pred_dt=as.data.table(pred_dt)

model_forecast_ar3 <- predict(ar3, n.head= 14)
last_trend_value_ar3 <-tail(new_decomp$trend[!is.na(new_decomp$trend)],1)
seasonality_ar3=new_decomp$seasonal[6:19]

ar3_forecast=as.numeric(model_forecast_ar3$pred)+last_trend_value_ar3+seasonality_ar3

test_start=as.Date("2021-01-08")

results_ar3=vector("list",14)

for(i in 1:14){
    current_test_date=test_start+i
    train_data=pred_dt[date<current_test_date]
    test_data=pred_dt[date==current_test_date]
    print(current_test_date)
    decomp=decompose(ts(train_data$consumption,freq=7),type="add")
    rand_val=decomp$random
    fit_mod=auto.arima(rand_val)
    test_data[,forecasted:=as.numeric(predict(ar3, n.ahead = 1)$pred + decomp$season[((i+5)%%7)+1] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    results_ar3[[i+1]]=test_data
}

res_dt_ar3=rbindlist(results_ar3)

res_dt_ar3$forescated_not=ar3_forecast

perf_dt("Predictions in AR(3) without Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forescated_not)
perf_dt("Predictions in AR(3) with Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forecasted)

tsdisplay(new_decomp$random,main="General Look of Input")

ma1=arima(new_decomp$random,order=c(0,0,1))
ma1

tsdisplay(residuals(ma1),main="General Look of MA-1 Model")

ma2=arima(new_decomp$random,order=c(0,0,2))
ma2

tsdisplay(residuals(ma2),main="General Look of MA-2 Model")

ma3=arima(new_decomp$random,order=c(0,0,3))
ma3

tsdisplay(residuals(ma3),main="General Look of MA-3 Model")

pred_dt=rbind.fill(train_feat,test)
pred_dt=pred_dt[,c(1,2)]
pred_dt=as.data.table(pred_dt)

model_forecast_ma3 <- predict(ma3, n.head= 14)
last_trend_value_ma3 <-tail(new_decomp$trend[!is.na(new_decomp$trend)],1)
seasonality_ma3=new_decomp$seasonal[6:19]

ma3_forecast=as.numeric(model_forecast_ma3$pred)+last_trend_value_ma3+seasonality_ma3

test_start=as.Date("2021-01-08")

results_ma3=vector("list",14)

for(i in 1:14){
    current_test_date=test_start+i
    train_data=pred_dt[date<current_test_date]
    test_data=pred_dt[date==current_test_date]
    print(current_test_date)
    decomp=decompose(ts(train_data$consumption,freq=7),type="add")
    rand_val=decomp$random
    fit_mod=auto.arima(rand_val)
    test_data[,forecasted:=as.numeric(predict(ma3, n.ahead = 1)$pred + decomp$season[((i+5)%%7)+1] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    results_ma3[[i+1]]=test_data
}

res_dt_ma3=rbindlist(results_ma3)

res_dt_ma3$forescated_not=ma3_forecast

perf_dt("Predictions in MA(3) without Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forescated_not)
perf_dt("Predictions in MA(3) with Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forecasted)

perf_dt("Predictions in AR(3) without Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forescated_not)
perf_dt("Predictions in AR(3) with Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forecasted)
perf_dt("Predictions in MA(3) without Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forescated_not)
perf_dt("Predictions in MA(3) with Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forecasted)

fitted=auto.arima(new_decomp$random,trace=T)

fitted

tsdisplay(residuals(fitted),main="General Look of ARIMA(0,0,2)(0,0,2)[7] Model")

fitted_new=arima(new_decomp$random,order=c(3,0,2),seasonal = c(0,0,2))
fitted_new

tsdisplay(residuals(fitted_new),main="General Look of ARIMA(3,0,2)(0,0,2) Model")

model_forecast <- predict(fitted_new, n.head= 14)
last_trend_value <-tail(new_decomp$trend[!is.na(new_decomp$trend)],1)
seasonality=new_decomp$seasonal[6:19]

forecasted=as.numeric(model_forecast$pred)+last_trend_value+seasonality

pred_dt=rbind.fill(train_feat,test)
pred_dt=pred_dt[,c(1,2)]
pred_dt=as.data.table(pred_dt)

test_start=as.Date("2021-01-08")

results=vector("list",14)

for(i in 1:14){
    current_test_date=test_start+i
    train_data=pred_dt[date<current_test_date]
    test_data=pred_dt[date==current_test_date]
    print(current_test_date)
    decomp=decompose(ts(train_data$consumption,freq=7),type="add")
    rand_val=decomp$random
    fit_mod=auto.arima(rand_val)
    test_data[,forecasted:=as.numeric(predict(fitted_new, n.ahead = 1)$pred + decomp$season[((i+5)%%7)+1] +
                                     tail(decomp$trend[!is.na(decomp$trend)],1))]
    results[[i+1]]=test_data
}

res_dt=rbindlist(results)

res_dt$forescated_not=forecasted

perf_dt("Predictions in Arima without Slicing Window", res_dt$consumption, res_dt$forescated_not)
perf_dt("Predictions in Arima with Slicing Window", res_dt$consumption, res_dt$forecasted)

perf_dt("Predictions in AR(3) without Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forescated_not)
perf_dt("Predictions in AR(3) with Slicing Window", res_dt_ar3$consumption, res_dt_ar3$forecasted)
perf_dt("Predictions in MA(3) without Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forescated_not)
perf_dt("Predictions in MA(3) with Slicing Window", res_dt_ma3$consumption, res_dt_ma3$forecasted)
perf_dt("Predictions in Arima(3,0,2)(0,0,2)  without Slicing Window", res_dt$consumption, res_dt$forescated_not)
perf_dt("Predictions in Arima(3,0,2)(0,0,2)  with Slicing Window", res_dt$consumption, res_dt$forecasted)


