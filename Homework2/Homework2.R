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


#Convert to the Time-Series
df_ts <- xts(df[,-1], order.by=as.Date(df[,1], "%Y/%m/%d"))
glimpse(df_ts)


autoplot(df_ts[,c("TP.TUFE1YI.T1")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")

autoplot(df_ts[,c("TP.TIG07")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")

autoplot(df_ts[,c("TP.DK.USD.S.YTL")])+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
  ylab("Price Index")

#General looking with 3 different graph
plot(zoo(df_ts))



#Correlation between the variables
ggpairs(data.frame(df_ts))







#ggplot(price_index, aes(x=Tarih,y=TP.TUFE1YI.T1))+geom_line()+ggtitle("Time vs Price Index(01/2017 to 08/2020)")+
#  ylab("Price Index")+scale_x_date(date_breaks = "month")+ theme(axis.text.x = element_text(angle = 45,hjust = 1))
