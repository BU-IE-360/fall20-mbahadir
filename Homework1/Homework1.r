library(zoo)
library(xlsx)
library(lubridate)
library(ggplot2)

EVDS=read.xlsx("EVDS.xlsx",sheetIndex=1, header=TRUE)
names(EVDS)
EVDS <- EVDS[-c(255, 256), ]
EVDS$Tarih=dmy(EVDS$Tarih)
EVDS$Week=week(EVDS$Tarih)
EVDS$month_year <-format(EVDS$Tarih,'%Y-%m')
summary(EVDS)
str(EVDS)

unemployement_df=read.csv("unemployement.csv", stringsAsFactors = FALSE) 
unemployement_df <- unemployement_df[-c(157, 158), ]
unemployement_df$Tarih=as.Date(as.yearmon(unemployement_df$Tarih))
summary(unemployement_df)
str(unemployement_df)

covid_df=read.csv("covid19-Turkey.csv")
covid_df$date=as.Date(covid_df$date)
covid_df$day=day(covid_df$date)
covid_df$month_year <-format(covid_df$date,'%Y-%m')
summary(covid_df)
str(covid_df)

ggplot(EVDS,aes(x=Week,y=TP.DK.USD.A.EF.YTL))+ geom_bar(stat='identity')+
  facet_wrap(~year(Tarih))+
  xlab("Years") + ylab("Dollar Exhange Rate")+ ggtitle("Dollar Exchange Rate in Turkey for 2016 to 2020")

ggplot(unemployement_df,aes(x=month(Tarih),y=issizlik))+ geom_bar(stat='identity')+ 
  facet_wrap(~year(Tarih))+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("Years") + ylab("Percentage of Unemployement")+ ggtitle("Percentage of Unemployment in Turkey from 2006 to 2018")

ggplot(covid_df,aes(x=day,y=dailyCases))+ geom_bar(stat='identity')+ 
  facet_wrap(~month(date))+
  xlab("Months") + ylab("Number of Covid Cases")+ ggtitle("Number of Covid Cases in Turkey from March to July in 2020")

dolar_sr=read.csv("dolar_search.csv")
str(dolar_sr)
dolar_sr$Date=ymd(dolar_sr$Date)
dolar_sr$month_year <-format(dolar_sr$Date,'%Y-%m')
dolar_sr$Week=week(dolar_sr$Date)

ggplot(dolar_sr,aes(x=factor(year(Date)),y=Search))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for Dolar Search")+
  theme(legend.position = "none")

ggplot(EVDS,aes(x=factor(year(Tarih)),y=TP.DK.USD.A.EF.YTL))+ geom_boxplot(aes(fill=factor(year(Tarih))))+ 
  xlab("Years") + ylab("Dollar Exhange Rate")+ ggtitle("Boxplot for Dollar Exchange Rate")+
  theme(legend.position = "none")

unemployement_sr=read.csv("is_search.csv")
str(unemployement_sr)
unemployement_sr$Date=as.Date(as.yearmon(unemployement_sr$Date))

ggplot(unemployement_sr,aes(x=year(Date),y=Search))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab("Number of Search for (is ilanı)")+ ggtitle("Boxplot for (is ilanı) Search")+
  theme(legend.position = "none")+scale_x_discrete(limits=c(2006:2018))

ggplot(unemployement_df,aes(x=year(Tarih),y=issizlik))+ geom_boxplot(aes(fill=factor(year(Tarih))))+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for Unemployement Rate")+
  theme(legend.position = "none")+scale_x_discrete(limits=c(2006:2018))

corona_sr=read.csv("Corona_search.csv")
str(corona_sr)
corona_sr$Date=ymd(corona_sr$Date)
corona_sr$month_year <-format(corona_sr$Date,'%Y-%m')
ggplot(corona_sr,aes(x=month_year,y=Search))+ geom_boxplot(aes(fill=factor(month(Date))))+ 
  xlab("Years") + ylab("Number of Search for (corona) ")+ ggtitle("Boxplot for corona Search")+
  theme(legend.position = "none")

ggplot(covid_df,aes(x=month_year,y=dailyCases))+ geom_boxplot(aes(fill=factor(month(date))))+ 
  xlab("Months") + ylab("Number of Covid Cases")+ ggtitle("Boxplot for Covid Cases")+
  theme(legend.position = "none")


