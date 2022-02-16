# Lab 4 Script: Spatial Data in R
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [20 points] Open the R file "Lab5_Script.R" comment each line of code with its purpose 
# 2. [80 points] Open the R file "Lab5_Assignment.R" and answer the questions


#---- Part 2. Chicago Housing sales exploratory analysis

# 1. load the same Chicago housing sales data that we used in "Lab5Script.R" as a data.table object called `sales_chicago`



# 2. Create an object called 'crisis_share' that shows the percentage of the sales in the total period that took place before and after the housing crisis (use year=>2008 as a crisis indicator)



#3. Create a bar plot using the crisis_share object to show the differences between pre and post crisis in the share of sales. Save the graph as `githubuser_plot1.pdf`
# hint: remember to use stat='identity' argument. Explain why?


#4. Using the `sales_chicago` object:
#     4.1 create a new ordered factor variable called `bedrooms2` using the `ordered()` function
#     4.2 create a box-plot with `bedrooms2` on the x-axis, price on the y-axis and crisis_year in color
#     4.3 what happened to the medium price of housing sales with less than 4 bedrooms compared with housing sales with more than 4 bedrooms before and after the crisis?
#     4.4 save your plot as `githubuser_plot2.pdf`




#5. BONUS [30 points]: Explore the data on your own and create a new plot. Make it meaningful. 