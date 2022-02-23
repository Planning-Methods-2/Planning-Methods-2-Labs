# Lab 6 Assignment: Chi-Square and Correlation
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab6_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a Chi-Square and Correlation analysis in your data used to the final project. 


#---- Part 1.

# 1.1 load the same household data used in the Lab6_Script.R file, create the object `HTS`



# 2. Create an object called `HTS_tab` that has the frequency or contingency table for the variables `htype` and `income_cat`
# Refer to the BQRM book (p.13) if you don't know what the variables mean

HTS_tab <- table(hts$htype,hts$income_cat)


#3. run a Chi-Square test to show if the two variables above are associated or not. Provide an interpretation of the p-value and a sentence stating if the null hypothesis of no association should be rejected or not. What conclusions can you draw?

chisq.test(HTS_tab,correct = F)

#4. Calculate the Cramer's V coefficient for your Chi-Square test. What conclusions can you draw?

assocstats(HTS_tab)

#5. Calculate the pearson correlation coefficient between the variables `veh` and `hhincome`. What can you say about the relationship between these two variables?

cor(hts$veh,hts$hhincome,na.rm=T)

#6. Calculate the spearman correlation coefficient between the variables `hhsize` and `income_cat`. What can you say about the relationship between these two variables?
class(hts$income_cat)
table(hts$income_cat)

hts$income_cat2<-as.numeric(hts$income_cat)

cor(hts$hhsize,hts$income_cat2,method = "spearman",use = "pair")


