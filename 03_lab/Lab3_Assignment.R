# Lab 3 Script: Data wrangling with data.table
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [20 points] Open the R file "Lab3_Script.R" comment each line of code with its purpose only for parts 1 and 2
# 2. [80 points] Open the R file "Lab3_Assignment.R" and answer the questions

#---- Q1. write the code to load the dataset "tract_covariates.csv" located under the "datasets" folder in your repository using the fread function in the data.table package. Create an object called `opportunities` ----

houston_opportunities<-
#---- Q2. Read and become familiar with the dataset metadata. Next write the code for the following:
# Link to metadata: https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-9.pdf 

# Create a new object called `houston_opportunities` using data.table syntax that only contains the rows for the Houston area (hint: use the `czname` variable). 

# Create a new variable called `poor_share_diff_90_10` that is the absolute difference between the poor_share1990 and poor_share2010

houston_opportunities[,poor_share_diff_90_10 := abs(poor_share1990-poor_share1990)]


# explore the relationship between the `rent_twobed2015` and the variable `poor_share_diff_90_10` that you just created by creating a scatter plot with ggplot. Can you say something about the causality direction?


#---- Q4. aggregate your data at the county level by

# create a new object called `opportunities_county` that has a column with the mean values of the following variables `poor_share_diff_90_10` , `singleparent_share1990` and `singleparent_share2010`

# calculate the difference between single parents share in the new object by creating a variable

# create a scatter plot of the two differences values (poor_share and singleparent_share). Any theories?



