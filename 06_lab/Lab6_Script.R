# Lab 6 Script: Chi-Square and Correlation
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn to apply Chi-Square tests on categorical data
# 2. Learn to calculate Correlation coefficients

#---- 1. Learn to apply Chi-Square tests on categorical data ----

library(data.table)
library(foreign)

hts <- data.table(read.spss("06_lab/datasets/HTS.household.10regions.sav",to.data.frame = T))

hts.tab.obs<-table(hts$sf,hts$anywalk)
class(hts.tab.obs)

hts.cs<-chisq.test(hts$sf,hts$anywalk,correct = F) # chi-squared test
hts.cs

attributes(hts.cs)

hts.cs$statistic

# ---- Simulated hypothesis testing plot ----
curve(dchisq(x, df = 10), from = 0, to = 40)
abline(h=0,col='blue')
points(x=hts.cs$statistic,y=0,col='red')

##upper value from Chi-Squared Dist (1-alpha) with alpha=0.05
upper95 <- qchisq(.95, 10)

#create vector of x values
x_upper95 <- seq(upper95, 40)

#create vector of chi-square density values
p_upper95 <- dchisq(x_upper95, df = 10)

#fill in portion of the density plot for upper 95% value to end of plot
polygon(c(x_upper95, rev(x_upper95)), c(p_upper95, rep(0, length(p_upper95))), col = adjustcolor('red', alpha=0.3), border = NA)

# ---- symmetric measures for the chi-Squared test
install.packages('vcd')
library(vcd)
assocstats(hts.tab)

#---- 2. Learn to calculate Correlation coefficients ----

uza <- data.table(read.spss("06_lab/datasets/UZA.sav",to.data.frame = TRUE))

#pearson Correlation coefficient between log of road lane mile per 100 pop and log of daily vehicle miles traveled

cor(uza$lnlm,uza$lnvmt) # lanes -> vehicle usage?

cor.test(uza$lnlm,uza$lnvmt)  # H0: Correlation = 0; H1: Correlation !=0


# partial correlation
install.packages("ggm")
library(ggm)
names(uza)
uza_par<-uza[,c("lnlm","lnvmt","lnfuel")]
uza_pcor<-pcor(c(1,2,3),cov(uza_par)) #partial correlation coefficient
uza_pcor

dim(uza)
pcor.test(r = uza_pcor,q = 1,n = 157)

cor.test(uza$vmt,uza$pop000,method="spearman") #Spearman correlation coefficient
