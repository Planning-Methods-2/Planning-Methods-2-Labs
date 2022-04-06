# Lab 10 Script: Logit regression
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn key concepts (probability, odds, log-odds)
# 2. Learn to apply a Logit regression model using a toy example
# 3. Learn to apply a Binomial Logit regression model using the book example
# 4. Learn to apply a Binomial Logit regression model using a real example

#---- 1. Learn key concepts (probability, odds, log-odds) ----

# dice example
p_dr_4 <- 1/6 # number of times an outcome will occur / total number of possible outcomes

odds_dr_4 <- p_dr_4 / (1 - p_dr_4) # ratio between the probability of success and the probability of failure

log(odds_dr_4) # log transformation of the odds 


# why do we use log-odds to model probabilities?

p<-seq(from = 0, to = 1,by = 0.01)
odds<- p / (1-p)
log_odds<-log(odds)

cbind(p,odds,log_odds)

plot(p,odds,type='l')
plot(odds,log_odds,type='l')

# Allow to represent probabilities that are bounded between 0 and 1 in a wider scale for easier modelling (normal distribution)
hist(p)
hist(odds)
hist(log_odds)

p[51]
odds[51]
log_odds[51]

# you can always retrieve the probability back from the log-odds
#   odds<- p / (1-p)   ==>  p <-  odds/ (1 + odds)  ==> p <- exp(log_odds) / (1 + exp(log_odds))

exp(log_odds[51])/ (1 + exp(log_odds[51]))


#---- 2. Learn to apply a Logit regression model using a toy example ----

library(data.table)
library(foreign)

hts <- data.table(read.spss("06_lab/datasets/HTS.household.10regions.sav",to.data.frame = T))

names(hts)

hist(hts$anyvmt)

# M0: simplest binomial logit model: logit(anyvmt) ~ 1

m0<-glm(formula = anyvmt ~ 1, data = hts,family = "binomial")
summary(m0)

a<-table(hts$anyvmt)
prop<-prop.table(x=a)

log(prop[2]/(1-prop[2])) # replicating the intercept coefficient

exp(m0$coefficients)/(1+exp(m0$coefficients)) # replicating the probability

# M1: binomial logit model with a single continuous ind variable: logit(anyvmt) ~ hhsize 

m1<-glm(formula = anyvmt ~ hhsize, data = hts,family = "binomial")
summary(m1)

exp(m1$coefficients[2]) # odd ratio for hhsize
  # this means, that for an extra person in the household, the odds of driving increase in 82%

# point estimate
a1<- as.numeric( m1$coefficients[1] + m1$coefficients[2]*1 )
a2<- as.numeric( m1$coefficients[1] + m1$coefficients[2]*2 )
a4<- as.numeric( m1$coefficients[1] + m1$coefficients[2]*4 )
a5<- as.numeric( m1$coefficients[1] + m1$coefficients[2]*5 )

a5-a4 # the coefficient for hhsize is the incremental difference in the log-odds


exp(a5)/exp(a4) #odds ratio (change of odds)

exp(m1$coefficients[2]) # same odds ratio

exp(m1$coefficients[2])/(1+exp(m1$coefficients[2])) # replicating the probability

# replicating the change in odds of driving by using the estimated probability
pr_a1<-exp(a1)/(1+exp(a1)) # probability of driving if hhsize=1
pr_a2<-exp(a2)/(1+exp(a2)) # probability of driving if hhsize=2

odds_a1<- pr_a1/(1-pr_a1)
odds_a2<- pr_a2/(1-pr_a2)

(odds_a2/odds_a1) # odds ratio (change in the odds of driving from an extra unit in household size)


#---- 3. Learn to apply a Binomial Logit regression model using the book example ----

# M2: full binomial logit model: logit(anyvmt) ~ hhsize + hhworker + lnhhincome + entropy + pct4way + stopden

m2<-glm(formula = anyvmt ~ hhsize + hhworker + lnhhincome + entropy + pct4way + stopden, data = hts,family = "binomial")
summary(m2)


#---- 4. Learn to apply a Binomial Logit regression model using a real example ----

library(tidycensus)

census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")# you must acquired your own key at http://api.census.gov/data/key_signup.html

bexar_medincome_15 <- get_acs(geography = "tract", variables = "B19013_001",
                              state = "TX", county = "Bexar", geometry = TRUE,year = 2015)
bexar_medincome_20 <- get_acs(geography = "tract", variables = "B19013_001",
                              state = "TX", county = "Bexar", geometry = FALSE,year = 2020)

bexar_homevalue_15 <- get_acs(geography = "tract", variables = "B25077_001",
                              state = "TX", county = "Bexar", geometry = TRUE,year = 2015)
bexar_homevalue_20 <- get_acs(geography = "tract", variables = "B25077_001",
                              state = "TX", county = "Bexar", geometry = FALSE,year = 2020)

names(bexar_homevalue_15)[names(bexar_homevalue_15)%in%c("estimate","moe")] <-c("estimate_mhv_15","moe_mhv_15")
names(bexar_homevalue_20)[names(bexar_homevalue_20)%in%c("estimate","moe")] <-c("estimate_mhv_20","moe_mhv_20")

names(bexar_medincome_15)[names(bexar_medincome_15)%in%c("estimate","moe")] <-c("estimate_mhi_15","moe_mhi_15")
names(bexar_medincome_20)[names(bexar_medincome_20)%in%c("estimate","moe")] <-c("estimate_mhi_20","moe_mhi_20")

#Merging data
bexar_mhv<-merge(bexar_homevalue_15,bexar_homevalue_20,by="GEOID",sort = F)
bexar_mhi<-merge(bexar_medincome_15,bexar_medincome_20,by="GEOID",sort = F)

#Calculating the percentage change
bexar_mhv$mhv_per_change<-round(((bexar_mhv$estimate_mhv_20/bexar_mhv$estimate_mhv_15)-1),2)
bexar_mhi$mhi_per_change<-round(((bexar_mhi$estimate_mhi_20/bexar_mhi$estimate_mhi_15)-1),2)

bexar_mhv<-as.data.table(bexar_mhv)
bexar_mhv<-bexar_mhv[,.(GEOID,estimate_mhv_15,estimate_mhv_20,mhv_per_change)]

bexar_socioeconomic<-merge(x = bexar_mhi,y = bexar_mhv,by="GEOID",sort=F)


# adding Building permits data

source("10_lab/sa_bp_cleanning.R")

# aligning two coordinate systems
library(sf)
st_crs(bexar_socioeconomic)

bp_sa_map<-st_as_sf(building_permits_sa,coords = c("X_COORD","Y_COORD"),crs=3674) #https://spatialreference.org/ref/?search=texas

bp_sa_map<-st_transform(x = bp_sa_map,crs = st_crs(bexar_socioeconomic))

library(ggplot2)
ggplot()+
  geom_sf(data = bexar_socioeconomic)+
  geom_sf(data=bp_sa_map,size=0.05)

bp_sa_map<-st_join(x = bp_sa_map,y = bexar_socioeconomic)

bp_sa <- as.data.table(bp_sa_map)

bp_sa <- bp_sa[,.(`PERMIT TYPE`,`PERMIT #`, GEOID)]
bp_sa <-bp_sa[,.N,by=.(GEOID)]

summary(bp_sa$N)

bp_sa[,disp_BP:=as.numeric(N>47)]

# merging back to bexar socioeconimic

bexar_socioeconomic<-merge(bexar_socioeconomic,bp_sa,by="GEOID")


# modeling

mod1<-glm(disp_BP ~ mhi_per_change + mhv_per_change,family = "binomial",data=bexar_socioeconomic)
summary(mod1)
