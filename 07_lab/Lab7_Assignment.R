# Lab 7 Assignment: Chi-Square and Correlation
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`

library(data.table)
library(foreign)

hts <- data.table(read.spss("06_lab/datasets/HTS.household.10regions.sav",to.data.frame = T))


# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results

# step 1 -  Histogram looks ok normal
library(ggplot2)
ggplot(data=hts[!is.na(sf),],aes(x=hhincome))+
  geom_histogram()+
  facet_wrap(~sf)

# desc stats
hts[!is.na(sf),.(mean=mean(hhincome,na.rm=T),sd=sd(hhincome,na.rm=T),Obs=.N),by=.(sf)]

hts<-hts[!is.na(sf),]

#Step 2: dependent variable types
class(hts$hhincome) ; class(hts$sf)
# Step 3: H0: t-statistic = 0 (Mean_Seattle - Mean_Kansas = 0)

# Step 4: Conduct t-test
two_tailed_t_test<-hts[,t.test(formula = hhincome ~ sf)] # two-tailed
two_tailed_t_test

one_tailed_t_test<-hts[,t.test(formula = hhincome ~ sf,alernative = 'greater')] # one-tailed
one_tailed_t_test

# BONUS: Simulated hypothesis testing plot ----
curve(dt(x, df = 4353.4), from = -40, to = 40)
abline(h=0,col='blue')
points(x=two_tailed_t_test$statistic,y=0,col='red')

##upper value from Chi-Squared Dist (1-alpha) with alpha=0.05
upper975 <- qt(p = .975, df = 4353.4)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = 4353.4)
abline(v = lower025,y=0,col='red')

# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

hts_SA<-hts[region=="San Antonio, TX",]

hts_SA[,over_med_jobpop:=factor(as.numeric(jobpop>quantile(jobpop,0.5)))]
SA_two_tailed_t_test<-hts_SA[,t.test(formula = lnvmt ~ over_med_jobpop )] 
SA_two_tailed_t_test

# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.

ggplot(data=hts_SA, aes(x=income_cat, y= lnvmt))+
  geom_boxplot()

# deleting outliers 
hts_SA_bp<-boxplot(hts_SA$lnvmt~hts_SA$income_cat)

outliers <- hts_SA_bp$out

hts_SA_bp[lntpm%in%outliers,]
hts_SA2<-hts_SA[!lnvmt%in%outliers,]

boxplot(hts_SA2$lnvmt~hts_SA2$income_cat)

# Step 1: dependent variable is normal: 

hist(hts_SA2$lnvmt) # looks pretty normal to me!

# Step 1: variance homogeneity?
bartlett.test(lnvmt ~ income_cat, data=hts_SA2) # H0: variances are equal: p-value ==> accepts


# Step 4: one-way anova

fit<-aov(hts_SA2$lnvmt~hts_SA2$income_cat)
summary(fit)

#post-hoc test
TukeyHSD(fit)

plot(TukeyHSD(fit))

# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.

