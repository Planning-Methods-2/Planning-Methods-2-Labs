##########################################
## R Bootcamp - Part 2: Data import, wrangling & Vizualization
## Author: Esteban Lopez Ochoa Ph.D.
## Course: URP 5393 - Urban Planning Methods II
## Program: Master in Science in Urban Planning
## Institution: University of Texas at San Antonio
##########################################


#---- 1. SA Bulding Permits Data ----
bb<-fread("https://data.sanantonio.gov/dataset/05012dcb-ba1b-4ade-b5f3-7403bc7f52eb/resource/fbb7202e-c6c1-475b-849e-c5c2cfb65833/download/accelasubmitpermitsextract.csv")
names(bb)

bb[,.N,by=.(`Permit Type`)]

bb[`Permit Type`=="Res Building Application",.N,by=.(`Date Issued`)]
bb[`Permit Type`=="Res Building Application",.N,by=.(`Date Submitted`)]
bb[,date:=as.Date(`Date Submitted`)]


ggplot(bb[`Permit Type`=="Res Building Application",.N,by=.(date)],aes(x=date,y=N))+geom_line()
bb[,mm:=month(date)]
bb[,yy:=year(date)]

new_housing<-bb[`Permit Type`=="Res Building Application",.N,by=.(yy,mm)]
new_housing[,date:=as.Date(paste0(yy,"-",mm,"-","01"))]

ggplot(new_housing,aes(x=date,y=N))+geom_line()

nh2<-bb[,.N,by=.(yy,mm,`Permit Type`)]
nh2[,date:=as.Date(paste0(yy,"-",mm,"-","01"))]


ggplot(nh2[yy==2021],aes(x=date,y=N))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Permit Type`,scales = "free_y")+
  labs(title = "CoSA Building Permits visualization", subtitle = "Number of applications submitted by date submitted", caption = "Source: https://data.sanantonio.gov/dataset/building-permits")



