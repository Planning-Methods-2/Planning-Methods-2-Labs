# Lab 5 Script: Descriptive Statistics & Data Visualization
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 0. Learn about the difference between statistics (sample) and analytics (population)
# 1. Learn how to apply general descriptive statistic metrics
# 2. Learn how to use appropriate visualization methods

#---- Part 0. Population vs. Sample ----

# Law of Large Numbers
## More data on something will converge to the mean 

sample(x = 1:6,size = 1) # dice experiment


v100<-rnorm(n = 100,mean = 0,sd = 1)
v1000<-rnorm(n = 1000,mean = 0,sd = 1)
v10000<-rnorm(n = 10000,mean = 0,sd = 1)
v100000<-rnorm(n = 100000,mean = 0,sd = 1)
v1000000<-rnorm(n = 1000000,mean = 0,sd = 1)

plot(density(v100))
lines(density(v1000),col='red')
lines(density(v10000),col='green')
lines(density(v100000),col='blue')
lines(density(v1000000),col='brown')


b100<-rbinom(n = 100,size =10,prob = 0.5)
b1000<-rbinom(n = 1000,size =10,prob = 0.5)
b10000<-rbinom(n = 10000,size =10,prob = 0.5)
b100000<-rbinom(n = 100000,size =10,prob = 0.5)
b1000000<-rbinom(n = 100000,size =10000,prob = 0.5)

plot(density(b1000000))


# Let's imagine that this is a dataset containing ALL (the population) of air quality data 
library(data.table)

air_pop<-data.table(datasets::airquality)


View(air_pop)

str(air_pop)
air_pop[,summary(Temp)]
air_pop[,.(mean(Temp),sd(Temp))]


hist(air_pop$Temp,freq = F,breaks = 10)
lines(density(air_pop$Temp))
points(x=min(air_pop$Temp),y=0,col='red')
points(x=max(air_pop$Temp),y=0,col='blue')
abline(v=mean(air_pop$Temp),col='green')
abline(v=quantile(air_pop$Temp,probs = 0.5),col='darkblue')

# library(ggplot2)
# 
# ggplot(data = air_pop,aes(x=Temp,y = after_stat(density)))+
#   geom_histogram(bins=10)+
#   geom_density()

#Let's take a sample

air_sample_temp<-sample(air_pop$Temp,size = 50)

plot(density(air_pop$Temp))
lines(density(air_sample_temp),col='red')
