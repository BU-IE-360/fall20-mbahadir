getwd()
setwd("/Users/baha/R")
births_df=read.csv("US_births_2000-2014_SSA.csv")
summary(births_df)
str(births_df)
install.packages("ggplot2")
library(ggplot2)
names(births_df)
typeof(births_df$births)
print(c(2000:2014))
ggplot(births_df,aes(x=year,y=births))+ geom_bar(stat='identity')+ 
  xlab("Years") + ylab("Number of Births")+ ggtitle("Birth vs Years")


