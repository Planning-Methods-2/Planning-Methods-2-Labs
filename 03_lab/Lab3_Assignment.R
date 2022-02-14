# Lab 3 Script: Data wrangling with data.table
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [20 points] Open the R file "Lab3_Script.R" comment each line of code with its purpose only for parts 1 and 2
# 2. [80 points] Open the R file "Lab3_Assignment.R" and answer the questions

#---- Q1. write the code to load the dataset "tract_covariates.csv" located under the "datasets" folder in your repository using the fread function in the data.table package. Create an object called `opportunities` ----
library(data.table)
opportunities <- fread("03_lab/tract_covariates.csv")

#---- Q2. Read and become familiar with the dataset metadata. Next write the code for the following:
# Link to metadata: https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-9.pdf 

# Create a new object called `houston_opportunities` using data.table syntax that only contains the rows for the Houston area (hint: use the `czname` variable). 

houston_opportunities<-opportunities[czname=="Houston",]

# Create a new variable called `poor_share_diff_90_10` that is the absolute difference between the poor_share1990 and poor_share2010

houston_opportunities[,poor_share_diff_90_10 := abs(poor_share2010-poor_share1990)]


# explore the relationship between the `rent_twobed2015` and the variable `poor_share_diff_90_10` that you just created by creating a scatter plot with ggplot. Can you say something about the causality direction?
library(ggplot2)
ggplot(data = houston_opportunities,aes(x=rent_twobed2015,y=poor_share_diff_90_10)) + 
  geom_point()

# The absolute difference between the share of poor individuals by census tracts between 1990 and 2010 in Houston seems to have no clear relation (or a weak positive relation) with the rent of two bedroom in 2015. 
houston_opportunities[,poor_share_raw_diff_90_10 := (poor_share2010-poor_share1990)]

ggplot(data = houston_opportunities,aes(x=rent_twobed2015,y=poor_share_raw_diff_90_10)) + 
  geom_point()+
  geom_hline(yintercept = 0,col='red')+
  geom_smooth()

# However, the raw difference (i.e. arithmetic difference) shows more clearly that for the <1000 segment of rent, there seems to be a positive trend between the growth of poverty and rent values in Houston. However, after the >1000 point of rent, the relation seems to invert. 

#---- Q4. aggregate your data at the county level by

# create a new object called `opportunities_county` that has a column with the mean values of the following variables `poor_share_diff_90_10` , `singleparent_share1990` and `singleparent_share2010`

opportunities[,poor_share_raw_diff_90_10 := (poor_share2010-poor_share1990)]


opportunities_county<-opportunities[,.(
  mean_poor_share_raw_diff_90_10=mean(poor_share_raw_diff_90_10,na.rm=T),
  mean_singleparent_share1990=mean(singleparent_share1990,na.rm=T),
  mean_singleparent_share2010=mean(singleparent_share2010,na.rm=T)
  ), by=.(czname)]

# calculate the difference between single parents share in the new object by creating a variable

opportunities_county[,singleparent_sharediff_90_10:=(mean_singleparent_share2010 - mean_singleparent_share1990)]

# create a scatter plot of the two differences values (poor_share and singleparent_share). Any theories?

ggplot(data = opportunities_county,aes(x=singleparent_sharediff_90_10, y= mean_poor_share_raw_diff_90_10 ))+
  geom_point()

# there seems to be a non-linear relationship between the growth of single parents and poverty in Houston census tracts.
