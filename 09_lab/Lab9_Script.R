# Lab 9 Script: OLS regression
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn to apply a OLS regression model using a toy example
# 2. Learn to apply a OLS regression model using the book example
# 3. Learn to apply a OLS regression model using real data

#---- 1. Learn to apply a OLS regression model ----

#Steps:
 ## 1. Define the model specification based on theory and practical constraints (data, research design, etc.)
 ## 2. Explore the data creating relational plots (scatter plot, box-plot, histograms, etc.). Check the basic assumptions (continuous variables, number of NAs, correlations, etc.)
 ## 3. Estimate a basic OLS model, interpret results, check R-squared, F and T statistics.
 ## 4. Run some diagnostic tests to rule out issues (Heteroskedasticity, autocorrelation, etc)
 ## 5. Change model specification if needed and return to step 3.


# 1.1 Toy example (OLS model with only 1 independent variable)
library(data.table)
library(foreign)

# Food Expenditure = f(Income)

data_toy<-data.table(y_fe=c(2,12,11,14,21,20,27,27,29,27),
                     x_in=c(2,34,41,44,64,71,85,90,91,93))

#scatter plot:  there is a positive relation between X --> Y
library(ggplot2)

ggplot(data = data_toy,aes(x=x_in,y=y_fe))+
  geom_point()

# regression model

m_toy<-lm(formula = y_fe ~ x_in, data = data_toy)

summary(m_toy)

#results interpretation

# For each additional unit (a thousand dollars) of income, food expenditures will increase in 0.29 (thousand dollars), i.e., in 290 dollars

# Income has a strong explanatory power for food expenditure (significant, t and F tests, very high R-squared)

# Prediction - Fitted values

predict(m_toy)
m_toy$fitted.values

ggplot(data = data_toy,aes(x=x_in,y=y_fe))+
  geom_point()+
  geom_smooth(method = "lm")

predict(m_toy)
predict(m_toy,interval = "confidence") # confidence interval reflects the uncertainty around the mean prediction values
predict(m_toy,interval = "prediction")# prediction interval reflects the uncertainty around a single value

# Prediction - On new data

data_new<-data.table(x_in=c(5,25,50,75,100))
predict(object = m_toy,newdata = data_new) # point prediction

pred_mtoy<-predict(object = m_toy,newdata = data_new,interval = "prediction") # point prediction + confidence interval prediction

data_new<-cbind(data_new,pred_mtoy)

ggplot(data = data_toy,aes(x=x_in,y=y_fe))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_errorbar(data = data_new,aes(y=fit,ymin=lwr,ymax=upr),color="red",alpha=0.5)+
  geom_point(data=data_new,aes(y=fit),color="red")
  

# regression diagnostics
# visualizing errors
ggplot(data = data_toy,aes(x=x_in,y=y_fe))+
  geom_point()+
  geom_smooth(method = "lm",se = F)+
  geom_segment(aes(xend=x_in,yend=m_toy$fitted.values),color='red')


plot(m_toy)

# Plot 1: Linear relationship between X and Y: an horizontal red line indicates linear relationship
plot(m_toy,1)
# Plot 2: Normality of the residuals: points along the diagonal dotted line indicate normal residuals
plot(m_toy,2)
# Plot 3: Homoskedasticity assumtion: horizontal red line indicates variance of residuals does not change with predicted values (is homocedastic)
plot(m_toy,3)

# Plot 4: Check for influential observations: 
plot(m_toy,4)
  # Statisticians have developed a metric called Cook’s distance to determine the influence of a value. This metric defines influence as a combination of leverage and residual size.

# Plot 5: Check for outliers and high leverage points
plot(m_toy,5)
  # outliers: Observations whose standardized residuals are greater than 3 in absolute value are possible outliers (James et al. 2014).
  # leverage: A data point has high leverage, if it has extreme predictor x values. This can be detected by examining the leverage statistic or the hat-value. A value of this statistic above 2(k + 1)/n indicates an observation with high leverage (P. Bruce and Bruce 2017)

# test for autocorrelation
library(lmtest)
dwtest(m_toy)


#---- 2. Learn to apply a ANOVA test (F-test) ----
library("foreign")
uza <- data.table(read.spss("06_lab/datasets/UZA.sav",to.data.frame = TRUE))

plot(uza$vmt~uza$lm)

lm1<-lm(lnvmt~lnlm+lnpop000+lntpm+lnfuel+lninc000+lncompact,data=uza)
summary(lm1)

plot(lm1)

library(lmtest)
dwtest(lm1)

hist(lm1$residuals,freq=F)
lines(density(lm1$residuals,adjust=2))
plot(lm1$residuals~lm1$fitted.values)


#---- 3. Estimate a hedonic model ----
unzip(zipfile = "05_lab/datasets/HSD_sample.csv.zip",exdir = "09_lab/")
sales<-fread("09_lab/HSD_sample.csv")

names(sales)

f1<-formula(price ~ sqftb + age + rooms)
m1<-lm(f1,data=sales)
summary(m1)




