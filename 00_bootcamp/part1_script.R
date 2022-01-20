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



#---- 2.2 Travel time to work
v19_acs <- load_variables(2019, "acs5", cache = TRUE)
View(v19_acs)

bexar_TTW <- get_acs(
  year = 2019,
  geography = "tract",
  state = "TX",
  county = "Bexar",
  variables = c(ttw_0_5 = "B03002_004",
                ttw_5_9 = "B08303_003",
                ttw_10_14 = "B08303_004",
                ttw_15_19 = "B08303_005",
                ttw_20_24 = "B08303_006",
                ttw_25_29 = "B08303_007",
                ttw_30_34 = "B08303_008",
                ttw_35_39 = "B08303_009",
                ttw_40_44 = "B08303_010",
                ttw_45_59 = "B08303_011"),
                summary_var = "B08303_001",
  geometry = TRUE)%>%
  mutate(percent = 100 * (estimate / summary_est)) # Code extracted and adapted from Walker (2021) 

# Choroplet map with distribution of racial concentrations
#install.packages("tmap")
library(tmap)

tm_shape(bexar_TTW,
         projection = sf::st_crs(26915)) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "jenks",
          n = 5,
          palette = "Blues",
          title = "Travel Time to Work (TTW)\n Percentage of total trips by TTW") + 
  tm_layout(bg.color = "grey", 
            legend.outside = T,
            panel.label.bg.color = "white")

bexar_TTW<- get_acs(geography = "tract", variables = "B08013_001",
           state = "TX", county = "Bexar", geometry = TRUE)



