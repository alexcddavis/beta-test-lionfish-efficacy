#############
#############
### Data importing, cleaning, and manipulation of data for Davis et al 2021
#"Multiple drivers of invasive lionfish culling efficiency in marine protected 
#areas" 
##############
##########
#Author Alexandra CD Davis
#Installation date: 13 May 2021


#Set up===========================
### add here package to preserve relative paths
library(here)
library(tidyverse)

### Naming conventions ###

# 1. consistency
# ObjectOne -- Camel Case
# object_one -- snake case

# 2. Documentation
# Section documentation -- bullet points for the whole sections and what the
# packages are for
# line documentation -- line by line notes on what the code does

##### Creating a function

adding_sum = function(x,y) {
  
  ####### Docstring######
  #function takes in two numeric parameters and returns their sum
  
  ###### PARAMETERS:
  # x [num] - an object of type numeric
  # y [num] - an object of type numeric
  
  #### RETURNS:
  # z [num] - the sum of x & y
  
  z = x + y
  
  return (z)
}
  
adding_sum(5,22)
  
  
##### writing files#######
write_csv(CaptureEffort,here("./data/cleaned_data/CaptureEffortUseThis1.csv"))
  
  
  
  
  
######lionfish project load and tidy data code##

library(data.table)
library(tidyverse)
library(matrixStats)
library(triangle)
library(plyr)
library(nlme)
library(perturbR)
library(ggplot2)
library(lme4)
library(MuMIn)
library(clusterGeneration)
#require(MASS)
library(mgcv)
library(rstatix)
library(dplyr)
library(rlang)
library(car)
rm(list=ls(all=TRUE))
###Capture effort for lionfish, idedifiers are lionfish id and survey number
CaptureEffort = read_csv(here("./data/CaptureEffortUseThis1.csv"))

str (CaptureEffort)
head(CaptureEffort)
names(CaptureEffort)


###Survey metadata, has the site information associated with each survey number
#LionfishMeta = read_csv("Lionfish-Data-Entry_all-regions1.csv")
LionfishMeta = read_csv("All_Region_Meta_clean.csv")
str(LionfishMeta)
names(LionfishMeta)

names(LionfishMeta) = gsub(" ", "_", names(LionfishMeta))
unique(LionfishMeta$Site_ID)
###Vertical Relief dataset
VerticalR = read_csv("Habitat-data-entry_entered_updated1.csv")

unique(VerticalR$Site_ID)

# make new column so that I can sum the number of LF together
AverageVR <- ddply(VerticalR, c("Site_ID"),
                   summarize, Average = mean(Relief_cm))

setDT(VerticalR) #makes into data.table
setDT(LionfishMeta) #makes into data.table
setkey(LionfishMeta, Site_ID) #setting a join key
setkey(VerticalR, Site_ID) #setting a join key
LionfishMeta = LionfishMeta[AverageVR] #left-joinging based on key


##subset the meta data to only have the removal survey #s
temp =  LionfishMeta %>% 
  filter(SurveyNum %in% CaptureEffort$SurveyNum)


###COmbine the two dataframes
setDT(CaptureEffort) #makes into data.table
setDT(temp) #makes into data.table
setkey(CaptureEffort, SurveyNum) #setting a join key
setkey(temp, SurveyNum) #setting a join key
attemp1 = CaptureEffort[temp] #left-joinging based on key

CaptureLion =  attemp1
names(CaptureLion)



#replace NAs for zero in Hrs elapsed:
#CaptureLion$Hrs_elapsed[is.na(CaptureLion$Hrs_elapsed)] <- 0


#######################################################
####### calculate average time to collect lionfish per site:
########################################################

# make new column so that I can sum the number of LF together
CaptureLion$LF<- rep(1,nrow(CaptureLion)) 

#calculate number of lionfish and density for each Survey:
NumLFSurv <- ddply(CaptureLion, c("SurveyNum", "Site_ID", "trans_area"),
                   summarize, NumPerSurv = sum(LF), AvgTimeSurv =mean(Adj_min)) 


##CaptureLion$SurveyNum = as.factor(CaptureLion$SurveyNum)

# CaptureLion = CaptureLion %>%
#   mutate(NumPerSurv = as.vector(NumLFSurv)) %>%
#   select(-Site_ID, -Site_area_m2)


setDT(CaptureLion) #makes into data.table
setDT(NumLFSurv) #makes into data.table
setkey(CaptureLion, SurveyNum) #setting a join key
setkey(NumLFSurv, SurveyNum) #setting a join key
CaptureLion = CaptureLion[NumLFSurv] #left-joinging based on key


#calculate density of lionfish per Survey:
density_trans <- ddply(CaptureLion, c("SurveyNum", "NumPerSurv", "trans_area"),
                       summarize, TransDens = mean(NumPerSurv/trans_area))

setDT(density_trans) #makes into data.table
setkey(CaptureLion, SurveyNum) #setting a join key
setkey(density_trans, SurveyNum) #setting a join key
CaptureLion = CaptureLion[density_trans] #left-joinging based on key


#average the number of lionfish per site and the average time to get each one
AvgSiteLF<- ddply(CaptureLion, c("Site_ID", "Site_area_m2"),
                  summarize, AvgLFSite = mean(NumPerSurv), AvgTimeSite = mean(AvgTimeSurv))

setDT(AvgSiteLF) #makes into data.table
setkey(CaptureLion, Site_ID) #setting a join key
setkey(AvgSiteLF, Site_ID) #setting a join key
CaptureLion = CaptureLion[AvgSiteLF] #left-joinging based on key

density_site <- ddply(AvgSiteLF, c("Site_ID", "Site_area_m2"),
                      summarize, SiteDens1000 = I(1000*(AvgLFSite/Site_area_m2)),
                      SiteDens = (AvgLFSite/Site_area_m2))

setDT(density_site) #makes into data.table
setkey(CaptureLion, Site_ID) #setting a join key
setkey(density_site, Site_ID) #setting a join key
CaptureLion = CaptureLion[density_site] #left-joinging based on key



# setDT(CaptureLion) #makes into data.table
# setDT(AvgSiteLF) #makes into data.table
# setkey(CaptureLion, Site_ID) #setting a join key
# setkey(AvgSiteLF, Site_ID) #setting a join key
# CaptureLion = CaptureLion[AvgSiteLF] #left-joinging based on key


#Bring in the habitat compositiuon data frame
HabComp = read_csv("Amended Habitat Comp Master1.csv")
names(HabComp) = gsub(" ", "_", names(HabComp))
names(HabComp)


###### Delete the random header row DONT NEED TO DO THIS ANY MORE
# remove rows in r by row number
# test <- HabComp[-c(1),]
# HabComp = test

######################################################
####### calculate average % cover by site:
########################################################

#replace NAs for zero :
HabComp[is.na(HabComp)] <- 0

# ##change the factors to numeric, dont need to do this every time
# HabComp$TURF = as.numeric(as.character(HabComp$TURF))
# HabComp$C = as.numeric(as.character(HabComp$C))
# HabComp$G = as.numeric(as.character(HabComp$G))
# HabComp$S = as.numeric(as.character(HabComp$S))
# HabComp$DCA = as.numeric(as.character(HabComp$DCA))
# HabComp$DC = as.numeric(as.character(HabComp$DC))
# HabComp$MA = as.numeric(as.character(HabComp$MA))

#calculate the mean coral cover per site 
CompAvg <- ddply(HabComp, c("Site_ID"),
                 summarize, AvgCoral = mean(C)+ mean(DCA) + mean(DC),
                 AvgGorg = mean(G))


### add to the capture lion data frame so that all your variables are in one spot
# dfwith39 = dfwith39 %>% 
#    filter(Site_ID %in% dfwith30$Site_ID)
CompAvg = CompAvg %>% 
  arrange(Site_ID)
CaptureLion = CaptureLion %>% 
  arrange(Site_ID)

unique(CaptureLion$Site_ID)
unique(CompAvg$Site_ID)
# unique(CaptureLion$Site_ID)==unique(CompAvg$Site_ID)


##subset the meta data to only have the matching sites
CaptureLion = CaptureLion %>% 
  filter (Site_ID %in% CompAvg$Site_ID)
unique(CaptureLion$Site_ID) #should be like 19 reefs

temp1 =  CompAvg %>% 
  filter(Site_ID %in% CaptureLion$Site_ID)
CompAvg = temp1

setDT(CaptureLion) #makes into data.table
setDT(CompAvg) #makes into data.table
setkey(CaptureLion, Site_ID) #setting a join key
setkey(CompAvg, Site_ID) #setting a join key
CaptureLion = CaptureLion[CompAvg] #left-joinging based on key

new2<- subset(CaptureLion, Lionfish_size_TL >0)

lionfish_removals = new2

###remove the outliers from capture lion and replot
ab <- ggplot(lionfish_removals, aes( x=Adj_Prop))
ab + geom_histogram( bins = 100, color = "black", fill = "grey") +
  geom_vline(aes(xintercept = mean(Prop_remaining)), 
             linetype = "dashed", size = 0.6)

# lionfish_removals= lionfish_removals[!is.na(lionfish_removals$Lionfish_size_TL),]
# lionfish_removals= lionfish_removals[!is.na(lionfish_removals$Lionfish_visibility),]

#remove instances where they did not try and catch the fish
time_per_fish = subset(lionfish_removals, Adj_min !="0")

###subset data by region
NOFGBNMS= subset(lionfish_removals, Region != "FGBNMS")
FGBNMS= subset(lionfish_removals, Region == "FGBNMS") #354
FloridaOnly = subset(lionfish_removals, Region == "Florida") #428
USVIONly = subset(lionfish_removals, Region == "USVI") #85
Biscayne = subset(lionfish_removals, Location == "Biscayne") #181
FKNMS = subset(FloridaOnly, Location != "Biscayne") #247

###subset by time of day
midday_filter <- subset(lionfish_removals, TOD !="Crepuscular")
crepuscular_filter <- subset(lionfish_removals, TOD !="midday")