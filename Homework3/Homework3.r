library(xlsx)
library(data.table)
library(ggplot2, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(xts, quietly=TRUE)
library(fpp2, quietly=TRUE)
library(GGally, quietly=TRUE)

price_index=read.xlsx("Consumer Price Index.xlsx",sheetIndex=1, header=TRUE)
price_index$Date=as.Date(as.yearmon(price_index$Date))
colnames(price_index)[2] <- "index"

interest_rate=read.xlsx("Interest Rate.xlsx",sheetIndex=1, header=TRUE)
interest_rate$Date=as.Date(as.yearmon(interest_rate$Date))

exchange_rate=read.xlsx("Exchange Rate.xlsx",sheetIndex = 1, header=TRUE)
exchange_rate$Date=as.Date(as.yearmon(exchange_rate$Date))

gold_reserve=read.xlsx("Gold Reserve.xlsx",sheetIndex = 1, header=TRUE)
gold_reserve$Tarih=as.Date(as.yearmon(gold_reserve$Tarih))
colnames(gold_reserve)[1] <- "Date"

sub_df<-merge(price_index,interest_rate,by="Date")
sub_df1<-merge(sub_df,exchange_rate,by="Date")
df<-merge(sub_df1,gold_reserve,by="Date")

df_ts <- xts(df[,-1], order.by=as.Date(df[,1], "%Y/%m/%d"))
glimpse(df_ts)

colnames(df_ts)[1]<-"price_index"
colnames(df_ts)[2]<-"interest_rate"
colnames(df_ts)[4]<-"gold_reserve"
colnames(df_ts)[3]<-"dollar_rate"

autoplot(df_ts[,c("price_index")])+ggtitle("Time vs Price Index (01/2003 to 12/2020)")+
  ylab("Price Index")+xlab("Date (Monthly)")

autoplot(df_ts[,c("interest_rate")])+ggtitle("Time vs Interest Rate (01/2003 to 12/2020)")+
  ylab("Interest Rate")+xlab("Date (Monthly)")

autoplot(df_ts[,c("gold_reserve")])+ggtitle("Time vs Gold Reserve (01/2003 to 12/2020)")+
  ylab("Gold Reserve Index")+xlab("Date (Monthly)")

autoplot(df_ts[,c("dollar_rate")])+ggtitle("Time vs Dollar Rate (01/2003 to 12/2020)")+
  ylab("Dollar Rate")+xlab("Date (Monthly)")

indexes<-c("Price Index","Interest Rate", "Dollar Rate", "Gold Reserve" )
plot(zoo(df_ts), main="General comparsion of Indexes ", xlab="Date (monthly)",ylab=indexes)

df_ts$price_index_normalized=(df_ts$price_index-min(df_ts$price_index))/(max(df_ts$price_index)-min(df_ts$price_index))
df_ts$interest_rate_normalized=(df_ts$interest_rate-min(df_ts$interest_rate))/(max(df_ts$interest_rate)-min(df_ts$interest_rate))
df_ts$gold_reserve_normalized=(df_ts$gold_reserve -min(df_ts$gold_reserve ))/(max(df_ts$gold_reserve )-min(df_ts$gold_reserve ))
df_ts$dollar_rate_normalized=(df_ts$dollar_rate -min(df_ts$dollar_rate ))/(max(df_ts$dollar_rate )-min(df_ts$dollar_rate ))

cols <- c("Price Index" = "red", "Interest Rate" = "blue", "Gold Reserve" = "yellow", "Dollar Index" = "green")

ggplot(df_ts)+geom_line(aes(x=Index, y=price_index_normalized,color="Price Index"))+
    geom_line(aes(x=Index, y=interest_rate_normalized ,color="Interest Rate"))+
    geom_line(aes(x=Index, y=gold_reserve_normalized , color= "Gold Reserve"))+
    geom_line(aes(x=Index, y=dollar_rate_normalized , color= "Dollar Index"))+
    ggtitle("Comparision of Normalized Indexes")+
  ylab("Normalized Values")+xlab("Date (Monthly)")+
  scale_color_manual(values = cols)+
  labs(colour = "Indexes")+ 
    theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )

ggplot(df_ts, aes(price_index))+ggtitle("Density of Price Index (01/2003 to 12/2020)")+
  ylab("Density")+xlab("Price Index")+geom_density(fill="lightblue")

ggplot(df_ts, aes(interest_rate))+ggtitle("Density of Interest Rate Index (01/2003 to 12/2020)")+
  ylab("Density")+xlab("Interest Rate Index")+geom_density(fill="lightblue")

ggplot(df_ts, aes(gold_reserve))+ggtitle("Density of Gold Reserve (01/2003 to 12/2020)")+
  ylab("Density")+xlab("Gold Reserve")+geom_density(fill="lightblue")

ggplot(df_ts, aes(dollar_rate))+ggtitle("Density of Dollar Index (01/2003 to 12/2020)")+
  ylab("Density")+xlab("Dollar Currency Rate")+geom_density(fill="lightblue")

ggplot() +
  geom_density(aes(price_index_normalized , fill = "Price Index"), alpha = .2, data = df_ts) +
  geom_density(aes(interest_rate_normalized  , fill = "Interest Rate"), , alpha = .2, data = df_ts) +
  geom_density(aes(gold_reserve_normalized  , fill = "Gold Reserve"), , alpha = .2, data = df_ts) +  
  geom_density(aes(dollar_rate_normalized  , fill = "Dollar Index"), , alpha = .2, data = df_ts) + 
  scale_fill_manual(name = "Indexes", values = c("Price Index" = "red", "Interest Rate"="blue","Gold Reserve" = "yellow", "Dollar Index" = "green" ))+
  labs(title = "Density Comparison for Normalized Values", x="Normalized Values for Datasets", y="Density")+ 
    theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )

df_ts<-df_ts[,0:4]

library(corrplot)
ggcorr(data.frame(df_ts), method = c("everything", "pearson")) 

M<-cor(data.frame(df_ts))
colnames(M)<- c("Price Index", "Interest Rate", "Dollar Index", "Gold Reserve" )
rownames(M)<- c("Price Index", "Interest Rate", "Dollar Index", "Gold Reserve" )
corrplot(M,
  method = "number",
  type = "upper" ,# show only upper side
)

cor(data.frame(df_ts), method = "pearson")

ggpairs(data.frame(df_ts))

dt=as.data.table(df_ts)

train=subset(dt,index<"2018-01-01")
test=subset(dt,index>="2018-01-01")

fit<- lm(price_index~dollar_rate+interest_rate+gold_reserve,train)
summary(fit)

checkresiduals(fit)

dt[,lag1:=shift(price_index,type="lag",n=1)]
dt[,lag12:=shift(price_index,type="lag",n=12)]

train=subset(dt,index<"2018-01-01")
test=subset(dt,index>="2018-01-01")

fit1<- lm(price_index~dollar_rate+interest_rate+lag1+lag12+gold_reserve,train)
summary(fit1)

checkresiduals(fit1)

plot(train[c(13:.N)][,list("Price Index"=price_index,residual=fit1$residual)])
plot(train[c(13:.N)][,list("Dollar Rate"=dollar_rate ,residual=fit1$residual)])
plot(train[c(13:.N)][,list("Interest Rate"=interest_rate ,residual=fit1$residual)])
plot(train[c(13:.N)][,list("Gold Reserve Index"=gold_reserve,residual=fit1$residual)])
plot(train[c(13:.N)][,list("Lag 1 value of Price Index"=lag1 ,residual=fit1$residual)])
plot(train[c(13:.N)][,list("Lag 12 value of Price Index"=lag12,residual=fit1$residual)])

pred=predict(fit1, newdata=test)

results_dt=data.table(Date=test$index,
                  act_price_index=test$price_index,
                  pred_price_index=pred)

ggplot(results_dt,aes(x=Date,y=act_price_index))+geom_line()+
    geom_point(aes(y=pred_price_index))+ggtitle("Predicted and Actual Price Index Comparison for Test Datasets")+ylab("Price Index")

SSE=sum((pred-test$price_index)^2)
SST=sum((test$price_index-mean(test$price_index))^2)
R=1-(SSE/SST)
RSME=sqrt(SSE/nrow(test))
print(paste("R value is:", R))
print(paste("RMSE value is:", RSME))

results_dt[.N]


