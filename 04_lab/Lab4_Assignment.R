# Lab 4 Script: Spatial Data in R
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [20 points] Open the R file "Lab4_Script.R" comment each line of code with its purpose parts 1.1 through 1.3
# 2. [80 points] Open the R file "Lab4_Assignment.R" and answer the questions



#---- Q1. Search for at least one secondary data source that will inform your research project. Do the following steps:

# Q1.1 Create a folder called `my_datasets` in your repository. You can do it manually or using the function `dir.create()`

dir.create(path = "04_lab/my_datasets")

# Q1.2 Download the data into the `my_datasets` folder. You can do it manually or using the function `download.file()` 

url_TIRZ_map<-"https://opendata-cosagis.opendata.arcgis.com/datasets/CoSAGIS::tax-increment-reinvestment-zone-tirz.zip?outSR=%7B%22latestWkid%22%3A2278%2C%22wkid%22%3A102740%7D"

download.file(url = url_TIRZ_map,destfile = "04_lab/my_datasets/TIRZ_map.zip")

# Q1.3 Load the data into R creating the object `my_data_1` (change the suffix as you add 2,3 or more data sets).


unzip(zipfile = "04_lab/my_datasets/TIRZ_map.zip",exdir = "04_lab/my_datasets/")

library(sf)
TIRZ_map<-read_sf("04_lab/my_datasets/Tax_Increment_Reinvestment_Zone_(TIRZ).shp")

plot(TIRZ_map)

#---- Q2. What is the unit of analysis of your data. Use the functions `str()`, `summary()` and `View()` to describe each of the objects in your dataset. Is the unit of analysis of your data appropriate to answer your research question?

#Tax Increment Reinvestment Zones are collections of census tracts defined by CoSA to be locations of tax increment reinvestment zones 

# Q3. What are the variables of interest (outcomes) that you would like to measure in your study? Plot an histogram of the most important variable of interest. Save the plot as `plot_1.pdf` in a new folder `my_results` in your repository. 
# Hint: use the `geom_histogram()` function. 
# Note: if you don't have a variable of interest, select a demographic variable that would be affecting your study and use that instead.

# no variables of interest at this point, but just for the sake of the excercise

dir.create("04_lab/my_results")

library(ggplot2)

ggplot(data = TIRZ_map, aes(x=Acres))+
  geom_histogram()

ggplot()+
  geom_histogram(data = TIRZ_map,aes(x=Acres))
ggsave(filename = "04_lab/my_results/plot_1.pdf")



# Q4. BONUS: 20 pts. Think of the best way to show at least two of the variables/concepts your study in plot and save it as `plot_2.pdf`. What can you say about your plot?

#This is my take on this. I was expecting something much simpler in your case. But please see this in case you are looking to do something simmilar.

#This is the average income as by the 2019 ACS by TIRZ.

library(tidycensus)


census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")# you must acquired your own key at http://api.census.gov/data/key_signup.html

bexar_medincome <- get_acs(geography = "tract", variables = "B19013_001",
                           state = "TX", county = "Bexar", geometry = TRUE,year = 2019)


TIRZ_map<-st_transform(x = TIRZ_map,crs = st_crs(bexar_medincome)) # puts the two shapefil in the same coordinate system

sf::sf_use_s2(FALSE) #turns off a spherical geometry function (i didn't know this, had to google it)

TIRZ_map2<-aggregate(x = bexar_medincome,by=TIRZ_map,FUN = mean,na.rm=T) # aggregates income data at the census tract level to the TIRZ level

ggplot()+
  geom_sf(data=bexar_medincome)+
  geom_sf(data=TIRZ_map2,aes(fill=estimate),colour=NA)
ggsave(filename = "04_lab/my_results/plot_2.pdf")
