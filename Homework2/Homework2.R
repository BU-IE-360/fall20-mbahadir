setwd("C:/Users/bahad/GitHub/IE360/Homework2")
install.packages("xlsx")
library(xlsx)
library(fpp2)
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(xts)
install.packages("GGally")
library(GGally)



#Loading the datasets
price_index=read.xlsx("Price Index.xlsx",sheetIndex=1, header=TRUE)
plot(price_index)
price_index$Tarih=as.Date(as.yearmon(price_index$Tarih))
str(price_index)

unemployment_rate=read.xlsx("Unemployement Rate.xlsx",sheetIndex=1, header=TRUE)
plot(unemployment_rate)
unemployment_rate$Tarih=as.Date(as.yearmon(unemployment_rate$Tarih))
str(unemployment_rate)

dolar_exchange_rate=read.xlsx("Dolar Exchange Rate.xlsx",sheetIndex = 1, header=TRUE)
plot(dolar_exchange_rate)
dolar_exchange_rate$Tarih=as.Date(as.yearmon(dolar_exchange_rate$Tarih))
str(dolar_exchange_rate)



#Merge the datasets
sub_df<-merge(price_index,unemployment_rate,by="Tarih")
df<-merge(sub_df,dolar_exchange_rate,by="Tarih")
df

#Convert to the Time-Series
df_ts <- xts(df)

df_ts <- xts(df[,-1], order.by=as.Date(df[,1], "%Y/%m/%d"))
glimpse(df_ts)

colnames(df_ts)[1]<-"tufe_index"
colnames(df_ts)[2]<-"unemp_rate"
colnames(df_ts)[3]<-"dolar_rate"
df_ts



df_ts$tufe_index_normalized=(df_ts$tufe_index-min(df_ts$tufe_index))/(max(df_ts$tufe_index)-min(df_ts$tufe_index))
df_ts$unemp_rate_normalized=(df_ts$unemp_rate-min(df_ts$unemp_rate))/(max(df_ts$unemp_rate)-min(df_ts$unemp_rate))
df_ts$dolar_rate_normalized=(df_ts$dolar_rate -min(df_ts$dolar_rate ))/(max(df_ts$dolar_rate )-min(df_ts$dolar_rate ))

cols <- c("Dolar Exchange Rate" = "red", "Tufe Index" = "yellow", "Unemployement Rate" = "blue")

ggplot(df_ts)+geom_line(aes(x=Index, y=tufe_index_normalized,color="Dolar Exchange Rate"))+
  geom_line(aes(x=Index, y=unemp_rate_normalized ,color="Tufe Index"))+
  geom_line(aes(x=Index, y=dolar_rate_normalized , color= "Unemployement Rate"))+
  ggtitle("Normalized Indexes Comparision")+
  ylab("Normalized Values")+xlab("Date")+
  scale_color_manual(values = cols)+
  labs(colour = "Indexes")
  


autoplot(df_ts[,c("tufe_index")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")

autoplot(df_ts[,c("unemp_rate")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")

autoplot(df_ts[,c("dolar_rate")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")



ggplot(df_ts,)

#General looking with 3 different graph
plot(zoo(df_ts))



df_ts_req=df_ts[,c(1,2,3)]
df_ts_req
#Correlation between the variables
ggpairs(data.frame(df_ts_req))







#ggplot(price_index, aes(x=Tarih,y=TP.TUFE1YI.T1))+geom_line()+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
#  ylab("Price Index")+scale_x_date(date_breaks = "month")+ theme(axis.text.x = element_text(angle = 45,hjust = 1))
