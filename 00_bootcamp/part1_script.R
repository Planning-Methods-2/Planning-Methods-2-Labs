##########################################
## R Bootcamp - Part 1: Syntax, Objects, Functions, Urban Analytics Demo
## Author: Esteban Lopez Ochoa Ph.D.
## Course: URP 5393 - Urban Planning Methods II
## Program: Master in Science in Urban Planning
## Institution: University of Texas at San Antonio
##########################################


#---- 1. R Basics ----

# 1.0. R and Rstudio GUI

# 1.1. Types of Files (Scripts, Markdown, etc.)

# 1.2. Objects

# 1.3. Environment

# 1.4. Functions

# 1.5. Logical vectors & Indexing

# 1.6. Manipulating Objects

# 1.7. Packages or libraries


#---- 2. Urban Analytics Demo ----

#---- 2.1 Migration flows - county level ----
#install.packages("tidycensus")
library(tidycensus)
census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")

travis_inflow <- get_flows(
  geography = "county",
  state = "TX",
  county = "Travis",
  geometry = TRUE, 
  year = 2019) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%
  arrange(desc(estimate))


bexar_inflow <- get_flows(
  geography = "county",
  state = "TX",
  county = "Bexar",
  geometry = TRUE, 
  year = 2019) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%
  arrange(desc(estimate))


#install.packages('mapdeck')

library(mapdeck)
library(tidyverse)

token<-"pk.eyJ1IjoiZXN0ZWJhbmxwIiwiYSI6ImNreWFjOW84YTA0NWsyeXF1M3ZwbzM5NzQifQ.fVUKJ10zAVgIpMPeUrdvCg"

travis_inflow %>%
  slice_max(estimate, n = 30) %>%
  mutate(weight = estimate / 500) %>%
  mapdeck(token = token) %>%
  add_arc(origin = "centroid2",
          destination = "centroid1",
          stroke_width = "weight",
          update_view = FALSE) 


bexar_inflow %>%
  slice_max(estimate, n = 30) %>%
  mutate(weight = estimate / 500) %>%
  mapdeck(token = token) %>%
  add_arc(origin = "centroid2",
          destination = "centroid1",
          stroke_width = "weight",
          update_view = FALSE) 



#---- 2.2 Mobility
v19_acs <- load_variables(2019, "acs5", cache = TRUE)
View(v19_acs)

install.packages("remotes")
remotes::install_github("SafeGraphInc/SafeGraphR")

library(SafeGraphR)
aa<-read_patterns("2022-01-11-18-2021-12-patterns.csv",
                  select = c('placekey',
                             'location_name',
                             'street_address',
                             'city',
                             'region',
                             'postal_code',
                             'poi_cbg',
                             'raw_visit_counts',
                             'raw_visitor_counts',
                             'distance_from_home',
                             'normalized_visits_by_total_visits',
                             'normalized_visits_by_total_visitors'))
View(aa)

aa[,GEOID:=substr(poi_cbg,1,11)]
aa[duplicated(GEOID),.N]
aa<-aa[!duplicated(GEOID),]

bexar_medincome <- get_acs(geography = "tract", variables = "B19013_001",
                           state = "TX", county = "Bexar", geometry = TRUE)
head(bexar_medincome)

bexar_medincome<-merge(bexar_medincome,aa,by='GEOID',all.x=T)
View(bexar_medincome)

#basic visualization
library(ggplot2)

ggplot(data = bexar_medincome)+
  geom_point(aes(x = raw_visitor_counts, y= distance_from_home))

ggplot(data = bexar_medincome[bexar_medincome$distance_from_home<80000,])+
  geom_point(aes(x = raw_visitor_counts, y= distance_from_home))

ggplot(data = bexar_medincome[bexar_medincome$distance_from_home<80000,],
       aes(y = raw_visitor_counts, x= distance_from_home,size=estimate,colour=estimate))+
  geom_point()+
  geom_smooth(method = 'loess')


bexar_medincome%>%
  mapdeck(token=token)%>%
  add_polygon(fill_colour = 'estimate',legend = T)


#---- 2.2 SA Tomorrow Westside Plan ----

library(sf)
# download source: https://data.sanantonio.gov/dataset/satomorrowsubareaplans 

#Unziping and reading the data
unzip("SATomorrowSubAreaPlans-shp.zip",exdir = "SATomorrowSubAreaPlans-shp")
SAT<-read_sf('SATomorrowSubAreaPlans-shp/SATomorrowSubAreaPlans.shp')


#exploring the data
plot(SAT)
names(SAT)
table(SAT$Phase,SAT$PlanType)

mapdeck(SAT,token = token)%>%
  add_polygon(fill_colour = "PlanType")


bike_facilities_map_url<-"https://opendata-cosagis.opendata.arcgis.com/datasets/55234cc46b954f69a659711b240bdf59_0.zip?outSR=%7B%22latestWkid%22%3A2278%2C%22wkid%22%3A102740%7D"

download.file(url = bike_facilities_map_url,destfile = "Bikes.zip")

unzip(zipfile = "bike_facilities_map.zip")
