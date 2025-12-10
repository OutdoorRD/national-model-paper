#########-----------------PREPARE REGRESSION DATASET------------################
## This file prepares the regression dataset for the national model and                          
## each of the 3 agencies. 

library(foreign)
library(lubridate)

# set the working directory to the root of the repo 
setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))

#load master list of units from GIS data
master=read.dbf("data/gis/national_model_polygons.dbf")

#Load the cell data
cell <- read.csv("./data/combined_airsage/national_model_Airsage_combined.csv")
cell$airsageud=cell$userdays

##load the social media data
social <- read.csv("./data/combined_smedia/monthly-socmed_20231207.csv")
social$year=year(social$d2p)
social$month=month(social$d2p)

##load the observed data
observed <- read.csv("./data/observed_data/combined_monthly_observed.csv")

#bring in weather, use this to make whole timeframe dataset for prediciton steps later
weather <- read.csv("./data/weather/national_model_monthly_weather.csv")
weather$date=as.Date(weather$month, format="%m/%d/%y")
weather$year=year(weather$date)
weather$month=month(weather$date)

#combine datasets
combined=merge(weather, social, by = c("year", "month", "siteid"), all.x=TRUE)
combined=merge(combined, cell, by = c("year", "month", "siteid"), all.x=TRUE)
combined=merge(combined, observed, by = c("year", "month", "siteid"), all.x=TRUE)

#add agency column using master list
combined <- merge(combined, master[,c("siteid","Category")], by = c("siteid"))
combined$agency=combined$Category.y

#combine both types of USFS catagories
levels(combined$agency) <- c(levels(combined$agency), "USFS")
combined$agency[combined$agency=="USFS ownership"] = "USFS"
combined$agency[combined$agency=="USFS Administrative"] = "USFS"

#keep only columns we need
combined <- combined[,c("year", "month", "siteid", "UNIT_NAME.x","airsageud", "visits","pud","tud","aud","eud","tmean_celsius","agency")]

#bring in other covariates
regions <- read.csv("./data/regions/national_model_polygons_subregions.csv")
population_surrounding <- read.csv("./data/population/surrounding_pop_30miles_all_agencies.csv")
population_in <- read.csv("./data/population/pop_within_boundary_all_agencies.csv")

#merge covariates
combined <- merge(combined, regions, by = c("siteid"), all.x=TRUE)
combined <- merge(combined, population_surrounding, by = c("siteid"), all.x=TRUE)
combined <- merge(combined, population_in, by = c("siteid"), all.x=TRUE)

#rename covariates
combined$Pop_in=combined$Pop
combined$region=combined$RPA_subreg
combined$temp=combined$tmean_celsius

#cut to just the columns we need
combined <- combined[,c("year", "month", "siteid", "UNIT_NAME.x","airsageud", "visits","pud","tud","aud","eud","region","Pop_30miles","Pop_in","temp","agency")]

#create log version of covariates to and deal with zero issue
combined$log_visits=log(combined$visits+1)
combined$log_airsageud=log(combined$airsageud+1)
combined$log_pud=log(combined$pud+1)
combined$log_tud=log(combined$tud+1)
combined$log_aud=log(combined$aud+1)
combined$log_eud=log(combined$eud+1)
combined$log_pop30=log(combined$Pop_30miles+1)
combined$log_popin=log(combined$Pop_in+1)

#create date variable
combined$date=as.Date(paste(combined$year, combined$month, "01", sep="-"))

#create FY for USFS 
combined$FY=ifelse(combined$month<10, combined$year, combined$year+1)

#save dataset with all months and years regardless of data completeness for prediction frame purposes
write.csv(combined, "./data/visitation_model_files/combined_national.csv")
