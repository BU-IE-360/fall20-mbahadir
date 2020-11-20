getwd()
############################
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate) 
install.packages("zoo")
library(zoo)
install.packages("xlsx")
library(xlsx)
###########################


#PART B
EVDS=read.xlsx("EVDS.xlsx",sheetIndex=1, header=TRUE)
summary(EVDS)
str(EVDS)
names(EVDS)
EVDS <- EVDS[-c(255, 256), ]
EVDS$Tarih=dmy(EVDS$Tarih)
EVDS$Week=week(EVDS$Tarih)
EVDS$month_year <-format(EVDS$Tarih,'%Y-%m')

str(EVDS)
ggplot(EVDS,aes(x=Week,y=TP.DK.USD.A.EF.YTL))+ geom_bar(stat='identity')+
  facet_wrap(~year(Tarih))+
  xlab("Years") + ylab("Dollar Exhange Rate")+ ggtitle("Dollar Exchange Rates in 2016-2020")+
  
###############################
unemployement_df=read.csv("unemployement.csv", stringsAsFactors = FALSE) 
unemployement_df <- unemployement_df[-c(157, 158), ]
summary(unemployement_df)
str(unemployement_df)
unemployement_df$Tarih=as.Date(as.yearmon(unemployement_df$Tarih))
names(unemployement_df)
ggplot(unemployement_df,aes(x=month(Tarih),y=issizlik))+ geom_bar(stat='identity')+ 
  facet_wrap(~year(Tarih))+
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Unemployement Rate in Turkey in 2006-2018")
###############################
covid_df=read.csv("covid19-Turkey.csv")
summary(covid_df)
str(covid_df)
covid_df$date=as.Date(covid_df$date)
covid_df$day=day(covid_df$date)
covid_df$month_year <-format(covid_df$date,'%Y-%m')
names(covid_df)
ggplot(covid_df,aes(x=day,y=dailyCases))+ geom_bar(stat='identity')+ 
  facet_wrap(~month(date))+
  xlab("Months") + ylab("Number of Covid Cases")+ ggtitle("Covid Cases in Turkey vs Time")
################################


#PART C


dolar_sr=read.csv("dolar_search.csv")
str(dolar_sr)
dolar_sr$Date=ymd(dolar_sr$Date)
dolar_sr$month_year <-format(dolar_sr$Date,'%Y-%m')
dolar_sr$Week=week(dolar_sr$Date)
ggplot(dolar_sr,aes(x=factor(year(Date)),y=Search))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for Dolar Search")

ggplot(EVDS,aes(x=factor(year(Tarih)),y=TP.DK.USD.A.EF.YTL))+ geom_boxplot(aes(fill=factor(year(Tarih))))+ 
  xlab("Date") + ylab("Exhange Rate")+ ggtitle("Boxplot for Dollar Exchange Rate")
################################
unemployement_sr=read.csv("is_search.csv")
str(unemployement_sr)
unemployement_sr$Date=as.Date(as.yearmon(unemployement_sr$Date))
ggplot(unemployement_sr,aes(x=Date,y=Search,group=year(Date)))+ geom_boxplot()+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for is ilanı Search")

ggplot(unemployement_df,aes(x=Tarih,y=issizlik,group=year(Tarih)))+ geom_boxplot()+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for Unemployement Rate")
################################
corona_sr=read.csv("Corona_search.csv")
str(corona_sr)
corona_sr$Date=ymd(corona_sr$Date)
corona_sr$month_year <-format(corona_sr$Date,'%Y-%m')
ggplot(corona_sr,aes(x=month_year,y=Search))+ geom_boxplot()+ 
  xlab("Years") + ylab("Percentage of Unemployement ")+ ggtitle("Boxplot for corona Search")

ggplot(covid_df,aes(x=month_year,y=dailyCases))+ geom_boxplot()+ 
  xlab("Months") + ylab("Number of Covid Cases")+ ggtitle("CBoxplot for Covid Cases")
################################


