# Lab 3 Script: Data wrangling with data.table
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. The `data.table` syntax for data manipulation
# 2. Learn change, create, delete, sort, variables within a dataset.
# 3. Merge or combine two datasets
# 4. Aggregate a data set

# Part 0. The `data.frame` basis and why do we need `data.table`

d0<-read.csv("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv",header = T)

 # system.time({
 #   d0<-read.csv("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv",header = T)
 # })

d0$czname # access a variable within dataset
d0[,c("czname")] #same

d0_SA<-d0[d0$czname=="San Antonio",] # Filters only SA data

d0$ln_income<-log(d0$hhinc_mean2000) # creates a variable

by(d0[,c("hhinc_mean2000","mean_commutetime2000")],INDICES = d0$czname, FUN = function(x){sapply(x, mean,na.rm=T)}) # operations by a third variable

by(d0_SA[,c("hhinc_mean2000")],INDICES = d0_SA$czname, FUN = function(x){which(is.na(x))}) # operations by a third variable
d0_SA[c(374,377),]

#---- Part 1: The `data.table` syntax for data manipulation ----

install.packages("data.table")

library(data.table)

d1<-fread("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv")

# system.time({
#   d1<-fread("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv")
# })

class(d1)
d1$czname # access a variable within dataset
d1[,.(state,county,tract)] #same, but better

d1[hhinc_mean2000>50000 & poor_share2000<0.05,names(table(czname))]

View(d1[,.N,by=czname])




d1[czname=="San Antonio",] # Filters only SA data

d1[,ln_income:=log(hhinc_mean2000)] # creates a variable


mean_values<-d1[,mean(hhinc_mean2000,na.rm=TRUE), by=czname]

d1[,.(mean(hhinc_mean2000,na.rm=TRUE),mean(poor_share2000,na.rm=TRUE)), by=.(state)]

d1[,.(mean_income=mean(hhinc_mean2000),mean_com_time=mean(mean_commutetime2000)),by=czname] # operations by a third variable

d1[czname=="San Antonio",which(is.na(hhinc_mean2000))] # operations by a third variable
d1[czname=="San Antonio" & is.na(hhinc_mean2000)==T,]

d1[czname=="San Antonio",][c(374,377),] #checking same result



#----- Part 2. Learn change, create, delete, sort, variables within a dataset.-----

d1[,table(czname)] # variable exploration - counts
d1[,.N, by=.(czname)] #alternative


SA_OI<- d1[czname=="San Antonio",] # filtering + object creation (subsetting)

SA_OI<- SA_OI[is.na(hhinc_mean2000)==FALSE,] # creating an object without na's (careful version of na.omit() function)

SA_OI[, ones:=1] # variable creation

SA_OI[,]

SA_OI[hhinc_mean2000<=quantile(hhinc_mean2000,probs = 0.5), bellow_medianIncome:=1 ] # variable creation by condition
SA_OI[is.na(bellow_medianIncome),bellow_medianIncome:=0] # variable value replacement by condition

SA_OI[, med_hhinc_growth1990_2006:=((med_hhinc2016-med_hhinc1990)/med_hhinc1990)*100]# variable creation by a mathematical manipulation of other variables.


#---- Part 3. Merge or combine two datasets ----


library(tidycensus)

census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")# you must acquired your own key at http://api.census.gov/data/key_signup.html

bexar_medincome <- get_acs(geography = "tract", variables = "B19013_001",
                           state = "TX", county = "Bexar", geometry = TRUE,year = 2019)

bexar_medincome

plot(bexar_medincome)


#merge SA_OI data with the map

head(SA_OI) ; head(bexar_medincome)

SA_OI[,GEOID:=paste0(state,"0",county,tract)] #creating a GEOID variable to have a common variable between the two data sets

#check variable classes

class(bexar_medincome$GEOID)
class(SA_OI$GEOID)

table(bexar_medincome$GEOID %in%  SA_OI$GEOID) # checking overlap


# merge/join by GEOIDS
bexar_medincome2<-data.table::merge(bexar_medincome,SA_OI,by="GEOID",)


#plotting

plot(bexar_medincome2[,"med_hhinc_growth1990_2006"])# fast plotting

library(ggplot2);library(viridis) # prettier plotting
ggplot(bexar_medincome2)+
  geom_sf(aes(fill=med_hhinc_growth1990_2006))+
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")

library(leaflet) # dynamic plotting

pal <- colorQuantile("YlOrRd", domain = bexar_medincome2$med_hhinc_growth1990_2006,n = 5)

leaflet(bexar_medincome2)%>%
  addProviderTiles(provider = providers$CartoDB.Positron)%>%
  addPolygons(fillColor = ~pal(med_hhinc_growth1990_2006),label = ~med_hhinc_growth1990_2006,color=~pal(med_hhinc_growth1990_2006),fillOpacity = 0.5,weight = 0.1)%>%
  addLegend(pal = pal,values = ~med_hhinc_growth1990_2006,opacity = 1,title = "Income Growth 1990-2016",labels = c("a","b","c","d","e"))

#----- Part 4. Aggregate a data set-----

d1_cstate<-d1[,.(mean_poorshare_2010=mean(poor_share2010,na.rm=T), singleparent_share2000=mean(singleparent_share2000,na.rm=T)),by=.(state)]



