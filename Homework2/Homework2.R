library(xlsx)
library(ggplot2, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(xts, quietly=TRUE)
library(fpp2, quietly=TRUE)
library(GGally, quietly=TRUE)

price_index=read.xlsx("Price Index.xlsx",sheetIndex=1, header=TRUE)
#plot(price_index)
price_index$Tarih=as.Date(as.yearmon(price_index$Tarih))
str(price_index)

property_price_index=read.xlsx("Residential Property Price Index.xlsx",sheetIndex=1, header=TRUE)
#plot(property_price_index)
property_price_index$Tarih=as.Date(as.yearmon(property_price_index$Tarih))
str(property_price_index)

dollar_exchange_rate=read.xlsx("Dolar Exchange Rate.xlsx",sheetIndex = 1, header=TRUE)
#plot(dollar_exchange_rate)
dollar_exchange_rate$Tarih=as.Date(as.yearmon(dollar_exchange_rate$Tarih))
str(dollar_exchange_rate)

sub_df<-merge(price_index,property_price_index,by="Tarih")
df<-merge(sub_df,dollar_exchange_rate,by="Tarih")

df_ts <- xts(df[,-1], order.by=as.Date(df[,1], "%Y/%m/%d"))
glimpse(df_ts)

colnames(df_ts)[1]<-"producer_price_index"
colnames(df_ts)[2]<-"prop_price_index"
colnames(df_ts)[3]<-"dollar_rate"

autoplot(df_ts[,c("producer_price_index")])+ggtitle("Time vs Producer Price Index (01/2017 to 08/2020)")+
  ylab("Producer Price Index")+xlab("Date (Monthly)")

autoplot(df_ts[,c("prop_price_index")])+ggtitle("Time vs Property Price Index (01/2017 to 08/2020)")+
  ylab("Property Price Index")+xlab("Date (Monthly)")

autoplot(df_ts[,c("dollar_rate")])+ggtitle("Time vs Dollar Index(01/2017 to 08/2020)")+
  ylab("Dollar Currency Rate")+xlab("Date (Monthly)")#+geom_smooth()

indexes<-c("Producer Price Index","Property Price Index", "Dollar Index" )
plot(zoo(df_ts), main="General comparsion of Indexes ", xlab="Date (monthly)",ylab=indexes)

df_ts$producer_price_index_index_normalized=(df_ts$producer_price_index-min(df_ts$producer_price_index))/(max(df_ts$producer_price_index)-min(df_ts$producer_price_index))
df_ts$prop_price_index_normalized=(df_ts$prop_price_index-min(df_ts$prop_price_index))/(max(df_ts$prop_price_index)-min(df_ts$prop_price_index))
df_ts$dollar_rate_normalized=(df_ts$dollar_rate -min(df_ts$dollar_rate ))/(max(df_ts$dollar_rate )-min(df_ts$dollar_rate ))

cols <- c("Dollar Exchange Rate" = "red", "Producer Price Index" = "yellow", "Residency Price Index" = "blue")

ggplot(df_ts)+geom_line(aes(x=Index, y=producer_price_index_index_normalized,color="Dollar Exchange Rate"))+
  geom_line(aes(x=Index, y=prop_price_index_normalized ,color="Producer Price Index"))+
  geom_line(aes(x=Index, y=dollar_rate_normalized , color= "Residency Price Index"))+
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

ggplot(df_ts, aes(producer_price_index))+ggtitle("Density of Producer Price Index (01/2017 to 08/2020)")+
  ylab("Density")+xlab("Producer Price Index")+geom_density(fill="lightblue")

ggplot(df_ts, aes(prop_price_index))+ggtitle("Density of Residency Price Index(01/2017 to 08/2020)")+
  ylab("Density")+xlab("Residency Price Index")+geom_density(fill="lightblue")

ggplot(df_ts, aes(dollar_rate))+ggtitle("Density of Dollar Index(01/2017 to 08/2020)")+
  ylab("Density")+xlab("Dollar Currency Rate")+geom_density(fill="lightblue")

ggplot() +
  geom_density(aes(producer_price_index_index_normalized , fill = "Producer Price Index"), alpha = .2, data = df_ts) +
  geom_density(aes(prop_price_index_normalized  , fill = "Residency Price Index"), , alpha = .2, data = df_ts) +
  geom_density(aes(dollar_rate_normalized  , fill = "Dollar Currency Rate"), , alpha = .2, data = df_ts) +  
  scale_fill_manual(name = "Indexes", values = c("Producer Price Index" = "red", "Residency Price Index" = "green", "Dollar Index"="blue"))+
  labs(title = "Density Comparison for Normalized Values", x="Normalized Values for Datasets", y="Density")+ 
    theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )#+ scale_fill_discrete(breaks=c("Dollar Index","Residency Price Index","Producer Price Index"))

library(corrplot)
df_ts_req=df_ts[,c(2,3,4)]
colnames(df_ts_req)<- c("Producer Price Index", "Residency Price Index", "Dollar Index")
ggcorr(data.frame(df_ts_req), method = c("everything", "pearson")) 

M<-cor(data.frame(df_ts_req))
colnames(M)<- c("Producer Price Index", "Residency Price Index", "Dollar Index")
rownames(M)<- c("Producer Price Index", "Residency Price Index", "Dollar Index")
corrplot(M,
  method = "number",
  type = "upper" ,# show only upper side
)

cor(data.frame(df_ts_req), method = "pearson")

ggpairs(data.frame(df_ts_req))


