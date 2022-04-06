#urban area San Antonio to census tracts  
library(ggplot2)
library(tigris)
library(tidycensus)

urban<-urban_areas(cb = T)
sa_urban<-urban[urban$NAME10=="San Antonio, TX",]
plot(sa_urban)
bexar_tracts<-tracts(state = "TX",county = "Bexar",cb = T)
#comal_tracts<-tracts(state = "TX",county = "Comal",cb = T)
#guadalupe_tracts<-tracts(state = "TX",county = "Guadalupe",cb = T)
plot(sa_tracts)


ggplot()+
  geom_sf(data = bexar_tracts,col='red')+
  #geom_sf(data = comal_tracts,col='green')+
  #geom_sf(data = guadalupe_tracts,col='brown')+
  geom_sf(data=sa_urban,fill="NA",col='blue')


bexar_urban<-st_join(x = bexar_tracts,y = sa_urban[,"GEOID10"])
View(bexar_urban)

bexar_urban$urban_tract<-as.numeric(!is.na(bexar_urban$GEOID10))

table(bexar_urban$urban_tract)
