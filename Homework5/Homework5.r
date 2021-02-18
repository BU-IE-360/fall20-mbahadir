library(data.table)
library(ggplot2)
library(GGally)

data_sales=fread("sales.txt")
data_sales=as.data.frame(data_sales)

str(data_sales)

cor(data_sales)

ggpairs(data_sales)

options(repr.plot.width=4, repr.plot.height=4)

for(i in 1:5){
    for(j in (i+1):6){
      plot(data_sales[,i],data_sales[,j],xlab=paste0(names(data_sales)[i]," Values"),ylab=paste0(names(data_sales)[j]," Values"),
          main=paste0("Scatter plot of ",names(data_sales)[i]," & ", names(data_sales)[j]))  
        
    }
}

abs(cor(data_sales)[,1])

first_regression=lm(SALES~ AGE, data=data_sales)
summary(first_regression)

second_APT_regression=lm(SALES~ AGE+APT , data=data_sales)
summary(second_APT_regression)

anova(first_regression,second_APT_regression)

second_ANX_regression=lm(SALES~ AGE+ANX , data=data_sales)
summary(second_ANX_regression)

anova(first_regression,second_ANX_regression)

second_EXP_regression=lm(SALES~ AGE+EXP , data=data_sales)
summary(second_EXP_regression)

anova(first_regression, second_EXP_regression)

second_GPA_regression=lm(SALES~ AGE+GPA , data=data_sales)
summary(second_GPA_regression)

anova(first_regression, second_GPA_regression)

summary(second_APT_regression)

second_APT_regression_reduced=lm(SALES~ APT , data=data_sales)
summary(second_APT_regression_reduced)

anova(second_APT_regression,second_APT_regression_reduced)

second_regression=lm(SALES~ AGE+APT , data=data_sales)

third_ANX_regression=lm(SALES~ AGE+APT+ANX, data=data_sales)
summary(third_ANX_regression)

anova(second_regression, third_ANX_regression)

third_EXP_regression=lm(SALES~ AGE+APT+EXP, data=data_sales)
summary(third_EXP_regression)

anova(second_regression, third_EXP_regression)

third_GPA_regression=lm(SALES~ AGE+APT+GPA, data=data_sales)
summary(third_GPA_regression)

anova(second_regression, third_GPA_regression)

summary(second_regression)

second_regression_int_removed=lm(SALES ~ -1 + AGE + APT, data = data_sales)
summary(second_regression_int_removed)

anova(second_regression,second_regression_int_removed)

second_regression_AGE_removed=lm(SALES ~ APT, data = data_sales)
summary(second_regression_AGE_removed)

anova(second_regression,second_regression_AGE_removed)

second_regression_APT_removed=lm(SALES ~ AGE, data = data_sales)
summary(second_regression_APT_removed)

anova(second_regression,second_regression_APT_removed)

regression_for_c=lm(SALES~., data=data_sales)
summary(regression_for_c)

stepped_regression=step(regression_for_c, direction="backward")
summary(stepped_regression)

summary(second_regression)

summary(stepped_regression)

final_model=second_regression

summary(final_model)

final_model$coefficients

anova(final_model)

(summary(final_model)$sigma)**2.

hypothesis_test=lm(SALES~ AGE + APT + GPA, data=data_sales)
summary(hypothesis_test)

anova(final_model,hypothesis_test)


