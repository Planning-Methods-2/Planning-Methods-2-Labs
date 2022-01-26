# Lab 2 Script: Loading data and the grammar of graphics (ggplot2)
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Load datasets into your R session -> open the `Lab2_script.R` to go over in class.
# 2. Learn about the different ways `R` can plot information
# 3. Learn about the usage of the `ggplot2` package


#---- Part 1: Loading data ----

# Data can be loaded in a variety of ways. As always is best to learn how to load using base functions that will likely remain in time so you can go back and retrace your steps. 
# This time we will load two data sets in three ways.


## ---- Part 1.1: Loading data from R pre-loaded packages ----

data() # shows all preloaded data available in R in the datasets package
help(package="datasets")

#Let's us the Violent Crime Rates by US State data 

help("USArrests")

# Step 1. Load the data in you session by creating an object

usa_arrests<-datasets::USArrests # this looks the object 'USAarrests' within '::' the package 'datasets'

class(usa_arrests)
names(usa_arrests)
dim(usa_arrests)
head(usa_arrests)


## ---- Part 1.2: Loading data from your computer directory ----
# We will use the Building Permits data from the city of San Antonio open data portal
# Source: https://data.sanantonio.gov/dataset/building-permits/resource/c21106f9-3ef5-4f3a-8604-f992b4db7512

building_permits_sa<-read.csv("02_lab/datasets/accelaissuedpermitsextract.csv",header = T)

names(building_permits_sa)
View(building_permits_sa)
class(building_permits_sa)
dim(building_permits_sa)
str(building_permits_sa)
summary(building_permits_sa)


## ---- Part 1.3: Loading data directly from the internet ----
# We will use the Building Permits data from the city of San Antonio open data portal
# Source: https://data.sanantonio.gov/dataset/building-permits/resource/c21106f9-3ef5-4f3a-8604-f992b4db7512

building_permits_sa2 <- read.csv("https://data.sanantonio.gov/dataset/05012dcb-ba1b-4ade-b5f3-7403bc7f52eb/resource/c21106f9-3ef5-4f3a-8604-f992b4db7512/download/accelaissuedpermitsextract.csv",header = T) 




## ---- Part 1.4: Loading data using a package + API ----
#install.packages("tidycensus")
#install.packages("tigris")
library(tidycensus)
library(tigris)


census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648") #this is mine, type ?census_api_key to get yours

age10 <- get_decennial(geography = "state", 
                       variables = "P013001", 
                       year = 2010)

head(age10)


bexar_medincome <- get_acs(geography = "tract", variables = "B19013_001",
                           state = "TX", county = "Bexar", geometry = TRUE)


View(bexar_medincome)



#---- Part 2: Visualizing the data ----
#install.packages('ggplot2')

library(ggplot2)



## ---- Part 2.1: Visualizing the 'usa_arrests' data ----

ggplot()

#scatter plot - relationship between two continuous variables
ggplot(data = usa_arrests,aes(x=Assault,y=Murder)) +
  geom_point()

#bar plot - compare levels across observations
usa_arrests$state<-rownames(usa_arrests)

ggplot(data = usa_arrests,aes(x=state,y=Murder))+
  geom_bar(stat = 'identity')

ggplot(data = usa_arrests,aes(x=reorder(state,Murder),y=Murder))+
  geom_bar(stat = 'identity')+
  coord_flip()

# adding color # would murder arrests be related to the percentage of urban population in the state?
ggplot(data = usa_arrests,aes(x=reorder(state,Murder),y=Murder,fill=UrbanPop))+
  geom_bar(stat = 'identity')+
  coord_flip()

# adding size
ggplot(data = usa_arrests,aes(x=Assault,y=Murder, size=UrbanPop)) +
  geom_point()


# plotting by south-east and everyone else 

usa_arrests$southeast<-as.numeric(usa_arrests$state%in%c("Florida","Georgia","Mississippi","Lousiana","South Carolina"))


ggplot(data = usa_arrests,aes(x=Assault,y=Murder, size=UrbanPop, color=southeast)) +
  geom_point()

usa_arrests$southeast<-factor(usa_arrests$southeast,levels = c(1,0),labels = c("South-east state",'other'))

ggplot(data = usa_arrests,aes(x=Assault,y=Murder, size=UrbanPop)) +
  geom_point()+
  facet_wrap(southeast~ .)


ggplot(data = usa_arrests,aes(x=Assault,y=Murder, size=UrbanPop)) +
  geom_point()+
  facet_grid(southeast ~ .)

## ---- Part 3: Visualizing the spatial data ----
# Administrative boundaries


library(leaflet)
library(tigris)

bexar_county <- counties(state = "TX",cb=T)
bexar_tracts<- tracts(state = "TX", county = "Bexar",cb=T)
bexar_blockgps <- block_groups(state = "TX", county = "Bexar",cb=T)
#bexar_blocks <- blocks(state = "TX", county = "Bexar") takes lots of time


# incremental visualization (static)

ggplot()+
  geom_sf(data = bexar_county)

ggplot()+
  geom_sf(data = bexar_county[bexar_county$NAME=="Bexar",])

ggplot()+
  geom_sf(data = bexar_county[bexar_county$NAME=="Bexar",])+
  geom_sf(data = bexar_tracts)

p1<-ggplot()+
  geom_sf(data = bexar_county[bexar_county$NAME=="Bexar",],color='blue',fill=NA)+
  geom_sf(data = bexar_tracts,color='black',fill=NA)+
  geom_sf(data = bexar_blockgps,color='red',fill=NA)

ggsave(filename = "02_lab/plots/01_static_map.pdf",plot = p1) #saves the plot as a pdf



# incremental visualization (interactive)
leaflet(bexar_county) %>%
  addTiles() %>%
  addPolygons()

names(table(bexar_county$NAME))

leaflet(bexar_county[bexar_county$NAME=="Bexar",]) %>%
  addTiles() %>%
  addPolygons()

leaflet(bexar_county[bexar_county$NAME=="Bexar",]) %>%
  addTiles() %>%
  addPolygons(group="county")%>%
  addPolygons(data=bexar_tracts,group="tracts") %>%
  addPolygons(data=bexar_blockgps,color = "#444444", weight = 1,group="block groups")

leaflet(bexar_county[bexar_county$NAME=="Bexar",]) %>%
  addTiles() %>%
  addPolygons(group="county")%>%
  addPolygons(data=bexar_tracts,group="tracts") %>%
  addPolygons(data=bexar_blockgps,color = "#444444", weight = 1,group="block groups") %>%
  addLayersControl(
    overlayGroups = c("county", "tracts","block groups"),
    options = layersControlOptions(collapsed = FALSE)
  )



