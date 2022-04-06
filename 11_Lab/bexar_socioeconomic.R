#bexar_socioeconomic
library(tidycensus)
library(data.table)

census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")# you must acquired your own key at http://api.census.gov/data/key_signup.html

bexar_medincome_20 <- get_acs(geography = "tract", variables = "B19013_001",
                              state = "TX", county = "Bexar", geometry = FALSE,year = 2020)

bexar_homevalue_20 <- get_acs(geography = "tract", variables = "B25077_001",
                              state = "TX", county = "Bexar", geometry = TRUE,year = 2020)


names(bexar_homevalue_20)[names(bexar_homevalue_20)%in%c("estimate","moe")] <-c("estimate_mhv_20","moe_mhv_20")
names(bexar_medincome_20)[names(bexar_medincome_20)%in%c("estimate","moe")] <-c("estimate_mhi_20","moe_mhi_20")

bexar_socioeconomic<-merge(x = bexar_homevalue_20,y = bexar_medincome_20,by="GEOID",sort=F)
