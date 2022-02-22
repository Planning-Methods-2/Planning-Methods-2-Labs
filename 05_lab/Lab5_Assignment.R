# Lab 4 Script: Spatial Data in R
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [20 points] Open the R file "Lab5_Script.R" comment each line of code with its purpose 
# 2. [80 points] Open the R file "Lab5_Assignment.R" and answer the questions


#---- Part 2. Chicago Housing sales exploratory analysis

# 1. load the same Chicago housing sales data that we used in "Lab5Script.R" as a data.table object called `sales_chicago`

unzip(zipfile = "datasets/HSD_sample.csv.zip",exdir = "datasets") # same as line 84 in "Lab5Script.R"

library(data.table)
sales_chicago<-fread(input = "datasets/HSD_sample.csv") # same as line 87 in "Lab5Script.R"
setkey(sales_chicago,year,month)


# 2. Create an object called 'crisis_share' that shows the percentage of the sales in the total period that took place before and after the housing crisis (use year=>2008 as a crisis indicator)

sales_chicago[,crisis:=as.numeric(year>=2008)] # creates an indicator (dummy) variable for the pre-post crisis period - same as line 116 in "Lab5Script.R"

crisis_share<-sales_chicago[,.N,by=.(crisis)] # counts the number of obs before and after the crisis year

crisis_share[,crisis_year:=factor(crisis,levels = c(0,1),labels = c("pre-crisis","post-crisis"))] # optional - same as line 117 in "Lab5Script.R" - allows for better plotting

crisis_share[,percent:=N/sum(N)]


#3. Create a bar plot using the crisis_share object to show the differences between pre and post crisis in the share of sales. Save the graph as `githubuser_plot1.pdf`
# hint: remember to use stat='identity' argument. Explain why?

library(ggplot2)

ggplot(data = crisis_share,aes(x=crisis_year,y=percent))+
  geom_bar(stat = "identity")
ggsave("estebanlp_plot1.pdf")


#4. Using the `sales_chicago` object:
#     4.1 create a new ordered factor variable called `bedrooms2` using the `ordered()` function

sales_chicago[,bedrooms2:=ordered(bedrooms)]
#or
sales_chicago[,bedrooms2:=factor(bedrooms,ordered = TRUE)]

#     4.2 create a box-plot with `bedrooms2` on the x-axis, price on the y-axis and crisis_year in color

sales_chicago[,crisis_year:=as.numeric(year>=2008)]
sales_chicago[,crisis_year:=factor(crisis_year,levels = c(0,1),labels = c("pre-crisis","post-crisis"))]

ggplot(data = sales_chicago)+
  geom_boxplot(aes(x=bedrooms2,y=price,colour=crisis_year))

#     4.3 what happened to the medium price of housing sales with less than 4 bedrooms compared with housing sales with more than 4 bedrooms before and after the crisis?

# The medium price of housing sales of larger homes (4>) is lower after the crisis, whereas housing sales of smaller homes (<=4) is higher after the crisis

#     4.4 save your plot as `githubuser_plot2.pdf`
ggsave(filename = "estebanlp_plot2.pdf")


#5. BONUS [30 points]: Explore the data on your own and create a new plot. Make it meaningful. 
# it is well-know that wealth and housing prices are higher in the north and lower in the south.
# latitude is the geographical coordinates that go from north to south
# as it can be seen, the price increase after the crisis was much more higher in the norther parts than in the south
# this suggest that wealthier areas of Chicago were having an easier time recovering housing prices than the south

ggplot(data = sales_chicago,aes(x=latitude,y=price))+
  geom_point()+
  geom_smooth()+
  facet_grid(~crisis_year)
