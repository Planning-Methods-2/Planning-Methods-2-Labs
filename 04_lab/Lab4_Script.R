# Lab 4 Script: Spatial data with R
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 0. Learn about shapefiles
# 1. Search and load different types of spatial data with R
# 1.1 Polygon data
# 1.2 Point data
# 1.3 Lines data
# 1.4 Raster data

#---- Part 1. Search and load different types of spatial data with R ----

##---- 1.1 Polygon data

library(tigris)
#help(package="tigris")

# counties
texas_counties <- counties(state = "TX",cb=T)
ggplot()+
  geom_sf(data = texas_counties,fill=NA)+
  geom_sf(data = texas_counties[texas_counties$NAME=="Bexar",],color='blue',fill=NA)

#tracts
bexar_tracts<- tracts(state = "TX", county = "Bexar",cb=T)
ggplot()+
  geom_sf(data = bexar_tracts,fill=NA)+
  geom_sf(data = texas_counties[texas_counties$NAME=="Bexar",],color='blue',fill=NA)
  
#example with census data
library(tidycensus)
census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")# you must acquired your own key at http://api.census.gov/data/key_signup.html

census_2020_vars<-load_variables(year = 2020,dataset = "pl")


#adapted from: https://walker-data.com/tidycensus/articles/spatial-data.html
racevars <- c(White = "P2_005N", 
              Black = "P2_006N", 
              Asian = "P2_008N", 
              Hispanic = "P2_002N")

bexar_race <- get_decennial(geography = "tract", variables = racevars, 
                        state = "TX", county = "Bexar County", geometry = TRUE,
                        summary_var = "P2_001N",year=2020) 
head(bexar_race)

bexar_race$pct <- 100 * (bexar_race$value / bexar_race$summary_value)

ggplot(data = bexar_race,aes(fill = pct)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  scale_fill_viridis_c()


##---- 1.2 Point data

tx_landmarks <- landmarks(state = "TX",type = 'point')
head(tx_landmarks)
ggplot()+
  geom_sf(data = texas_counties,fill=NA)+
  geom_sf(data = tx_landmarks,col="red")


# example: Building permits in San Antonio
#source:https://data.sanantonio.gov/dataset/building-permits/resource/764f3297-2f73-41a3-8de5-5e3976baaf03

library(data.table)
building_permits_sa <- fread("https://data.sanantonio.gov/dataset/05012dcb-ba1b-4ade-b5f3-7403bc7f52eb/resource/c21106f9-3ef5-4f3a-8604-f992b4db7512/download/accelaissuedpermitsextract.csv") 

head(building_permits_sa)

ggplot()+
  geom_point(data=building_permits_sa,aes(x=X_COORD,y=Y_COORD))

building_permits_sa[!is.na(X_COORD),]
building_permits_sa[!is.na(X_COORD),summary(X_COORD)]

building_permits_sa <- building_permits_sa[!is.na(X_COORD) & X_COORD>0 & Y_COORD>0,] # keeps 7813 obs out of 8886

ggplot()+
  geom_point(data=building_permits_sa,aes(x=X_COORD,y=Y_COORD))

ggplot()+
  geom_point(data=building_permits_sa,aes(x=X_COORD,y=Y_COORD))+
  geom_sf(data = bexar_tracts) # produces error, different CRS (coordinate reference system)

# aligning two coordinate systems
library(sf)
st_crs(bexar_tracts)

bp_sa_map<-st_as_sf(building_permits_sa,coords = c("X_COORD","Y_COORD"),crs=3674) #https://spatialreference.org/ref/?search=texas

bp_sa_map<-st_transform(x = bp_sa_map,crs = st_crs(bexar_tracts))

ggplot()+
  geom_sf(data = bexar_tracts)+
  geom_sf(data=bp_sa_map,size=0.05)
  

##---- 1.3 Lines data

bike_paths<-st_read(dsn = "04_lab/datasets/Bike_Facilities/Bike_Facilities.shp")

ggplot(bike_paths)+
  geom_sf()

ggplot(bike_paths)+
  geom_sf(data = bexar_tracts)+
  geom_sf(color='green')

##---- 1.4 Raster data

install.packages("ForestTools")
library(ForestTools)
library(raster)

data("kootenayCHM") # canopy height model (CHM) of 1.5 hectare swath of forest in the Kootenay Mountains, British Columbia

plot(kootenayCHM)
class(kootenayCHM)
nlayers(kootenayCHM) # unilayer raster

# example raster
install.packages("satellite")
library(satellite)

# download files from here if you want to play: https://www.dropbox.com/sh/4wdkpc3b3p6px3x/AADVlsSVs0BguMuBC2PZQ90aa?dl=0
raster_path<-"/Users/estebanlopezochoa/Dropbox/Documents/005 Teaching/007 Workshops/002 DataScience and RegionalScience/01 SatelliteImagery/LC08_L1TP_001076_20180228_20180308_01_T1/"

files <- list.files(raster_path,pattern=glob2rx("LC08*.TIF"), full.names = T)
sat<-satellite(files)

sat<-stack(sat) # stacking layers

# loading the panchromatic layer (15 mts resolution)

pan<-raster(files[10])


plotRGB(sat, r=4,g=3,b=2,stretch='lin') #True color composite
plotRGB(sat, r=5,g=4,b=3,stretch='lin') #NIR false color composite

#drawExtent()
e<-c(347303.5,361594.6,-2632700,-2594829)

sat.anf<-crop(sat,e)

plotRGB(sat.anf, r=5,g=4,b=3,stretch='lin') # Vegetation false color composite - healthy vegetation in red


vi <- function(img, i, k){
  bi <- img[[i]]
  bk <- img[[k]]
  vi <- (bi-bk)/(bk+bi)
  return(vi)
}

#For landstat8, NIR=5, red=4
ndvi<-vi(sat.anf,5,4)

plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from LandSat8')


