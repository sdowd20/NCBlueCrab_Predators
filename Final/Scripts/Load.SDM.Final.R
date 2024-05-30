
#Load packages and functions 
packages <- c("ggplot2", "tidyverse", "lubridate", "sf", "sp", "dplyr", "rnaturalearth", "readr", "readxl", "spatialEco", "rstatix", "viridis", "BBmisc", "corrplot", "mgcv", "GGally", "gjam", "report", "broom", "tidymodels", "car")
invisible(lapply(packages, library, character.only= TRUE))

library(lmtest)
library(countreg)
library(gridExtra)
library(ggplot2)
library(MASS)
library(countreg)
library(performance)
library(tidymodels)
library(tidyflow)
library(devtools)
library(rpart.plot)
library(vip)
library(baguette)
library(ranger)
library(dplyr)
library(beepr)

standard_theme <- theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.text.align= 0, legend.title= element_text(size = 12), legend.text = element_text(size= 10), axis.text=element_text(size=10), axis.title=element_text(size=12))

world <- ne_countries(scale = "medium", returnclass = "sf")

r2_general <-function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2)/sum((actual - mean(actual))^2))
} #function to calculate R2 in cross-validation

RMSE_func <- function(preds, actual){
  return(sqrt(mean((actual - preds)^2)))
} #function to calculate RMSE in cross-validation

pred_gam <- function(model, colnames){
  df <- as.data.frame(predict(model), df_CPUE_length_wide_both, type= "response")
  colnames(df) <- "prediction"
  df <- df %>% mutate(pred2= exp(prediction))
  df_model <- df_CPUE_length_wide_both %>% dplyr::select({{colnames}})
  df_model <- bind_cols(df_model, df)
  return(df_model)
}

graphs <- function(df, df2){
  plot1 <- df %>% ggplot(aes(x= value, y= pred2)) + geom_point() + geom_smooth(loess=TRUE) + facet_wrap(~predictor, scales= "free") + standard_theme + ylab("Predicted red drum CPUE")
  # plot2 <-df2 %>% ggplot(aes(x= reddrumP915, y= pred2)) + geom_point() + geom_smooth(loess=TRUE) + standard_theme
  print(plot1)
}

#Load in species response and predictor variable datasets. 
#These data are from the Pamlico Sound Independent Gill Net Survey (P915) 
#and the Estuarine Trawl Survey (P120) from the North Carolina Department of Marine Fisheries. 
#Sally Dowd processed these data with this workflow:
#1) Clean data and detect human recording errors 
#2) Form a 0.12° x 0.12° grid cell over the study area 
#3) Calculate the average catch-per-unit effort (CPUE) value 
#for an estuarine species and the average of environmental variables at a grid cell for a month (May or June) in a year 
#4) Use QGIS to calculate the NoFishRest predictor variable, or the % of a grid cell unrestricted to fishing activities.  

##CPUE: P915 and P120 averaged data 

df_CPUE_length <- read.csv("~/Google Drive/My Drive/Research/Ch1Data/CPUE/CPUE_grid_avg_lengthedt.03.04.24.csv")
df_CPUE_length <- df_CPUE_length[,-1]
df_CPUE_length <- df_CPUE_length %>% mutate_at("Survey", as.factor)
df_CPUE_length$Speciescommonname <- gsub(" ", "", df_CPUE_length$Speciescommonname)
colnames(df_CPUE_length) <- gsub(pattern = "_", replacement = "", colnames(df_CPUE_length))

##P915 non-averaged data (Model 3 in Part 2 of study)
df_CPUE_ind_length <- read.csv("~/Google Drive/My Drive/Research/Ch1Data/CPUE/CPUE_grid_avg_lengthedtP915.03.27.24.csv")
df_CPUE_ind_length <- df_CPUE_ind_length[,-1]
df_CPUE_ind_length <- df_CPUE_ind_length %>% mutate_at("Survey", as.factor)
df_CPUE_ind_length$Speciescommonname <- gsub(" ", "", df_CPUE_ind_length$Speciescommonname)
colnames(df_CPUE_ind_length) <- gsub(pattern = "_", replacement = "", colnames(df_CPUE_ind_length))

#Form datasets! 
##Pivot-wider dataset: P915 and P120 
df_CPUE_length$SpeciesSurvey <- paste(df_CPUE_length$Speciescommonname, df_CPUE_length$Survey, sep= "")
df_CPUE_length$meanCPUE <- as.numeric(df_CPUE_length$meanCPUE)
df_CPUE_length_wide_both <- df_CPUE_length %>% filter(Survey %in% "P120"|Survey %in% "P915") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "meanCPUE") %>% drop_na()

##Pivot-wider dataset: P915 
df_CPUE_length_wide_P915 <- df_CPUE_length %>% filter(Survey %in% "P915") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "meanCPUE") %>% drop_na() %>% filter(!gridID %in% c(398, 707, 679)) #drop gridID outliers (QGIS analysis)
  
##Pivot-wider dataset: P915 non-averaged
df_CPUE_ind_length$SpeciesSurvey <- paste(df_CPUE_ind_length$Speciescommonname, df_CPUE_ind_length$Survey, sep= "") 
df_CPUE_ind_length <- df_CPUE_ind_length %>% dplyr::select(-Speciescommonname, -latlon, -Season, -Latitude, -Longitude, -Season, -Sciname, -gridID, -Survey, -InletDistkm, -SAVkm, -Sedsizecommon, -FishingAllnum)
df_CPUE_length_wide_indP915 <- df_CPUE_ind_length %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "CPUE") %>% drop_na()
#this drop NA drops 121 Control1s- some Control1s got two rows where some species had NAs, this is because a different value was recorded for an environmental variable, just get rid of these
#before the drop NA, dataset gained 113 rows, even though unique # of Control1s is the same, dropped these Control1s
#8542 rows after drop_na over 8655, same as unique Control1

###Add on forage index to CPUE data
####Total forage 
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% mutate(reddrumP915forageP915 = rowSums(dplyr::select(., smallatlanticmenhadenP915, smallatlanticcroakerP915, pinfishP915, smallspotP915, smallsouthernflounderP915)), 
                                                                reddrumP915forageP120 = rowSums(dplyr::select(., smallatlanticcroakerP120, atlanticmenhadenP120, pinfishP120, smallspotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), 
                                                                southernkingfishP915forageP915 = rowSums(dplyr::select(., smallatlanticmenhadenP915, smallatlanticcroakerP915, smallspotP915)),
                                                                southernkingfishP915forageP120 = rowSums(dplyr::select(., atlanticmenhadenP120, smallatlanticcroakerP120, smallspotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)),
                                                                blackdrumP915forageP120 = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))

# ####Family forage
# df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% mutate(clupeidaeP915 = rowSums(dplyr::select(., smallatlanticmenhadenP915)), 
#                                                                 clupeidaeP120 = rowSums(dplyr::select(., atlanticmenhadenP120)), 
#                                                                 sciaenidaeP915 = rowSums(dplyr::select(., smallatlanticcroakerP915, smallspotP915)), 
#                                                                 sciaenidaeP120 = rowSums(dplyr::select(., smallatlanticcroakerP120, spotP120)),
#                                                                 sparidaeP915 = rowSums(dplyr::select(., pinfishP915)),
#                                                                 sparidaeP120 = rowSums(dplyr::select(., pinfishP120)),
#                                                                 penaiedP120 = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)),
#                                                                 paralichthyidaeP915 = rowSums(dplyr::select(., smallsouthernflounderP915)),
#                                                                 paralichthyidaeP120 = rowSums(dplyr::select(., southernflounderP120)))

##Select species of focus, refer to Final dataset & model formula 
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% dplyr::select(Month:avgsdo, smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915, smallatlanticcroakerP120, smallbluecrabP120, brownshrimpP120, whiteshrimpP120, pinkshrimpP120, pinfishP120, southernflounderP120, smallspotP120, atlanticmenhadenP120, reddrumP915forageP915:blackdrumP915forageP120)
df_CPUE_length_wide_P915 <- df_CPUE_length_wide_P915 %>% dplyr::select(Month:avgsdo, smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915)
df_CPUE_length_wide_indP915 <- df_CPUE_length_wide_indP915 %>% dplyr::select(Control1:NoFishRest, smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915)

##### INDIVIDUAL FORAGE #####
# reddrumP915forage: smallatlanticmenhadenP915, smallatlanticcroakerP915, pinfishP915, smallspotP915, smallatlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120
# southernkingfishP915forage: smallatlanticmenhadenP915, smallatlanticcroakerP915, smallspotP915, atlanticmenhadenP120, smallatlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120
# blackdrumP915forage: whiteshrimpP120, pinkshrimpP120, brownshrimpP120

##### FAMILY FORAGE #####
# ClupeidaeP915: smallatlanticmenhadenP915
# ClupeidaeP120: atlanticmenhadenP120 
# SciaenidaeP915: smallatlanticcroakerP915, smallspotP915
# SciaenidaeP120: smallatlanticcroakerP120, spotP120 
# SparidaeP915: pinfishP915
# SparidaeP120: pinfishP120
# PaneidP120: whiteshrimpP120, pinkshrimpP120, brownshrimpP120
# ParalichthyidaeP915: smallsouthernflounderP915
# ParalichthyidaeP120: southernflounderP120 
# PortunidaeP120: smallbluecrabP120

##Get rid of correlated variables
# check <- df_CPUE_length_wide_both
# cor <- cor(check[, c(2:11)]) #biotic 
# corrplot(cor) #removeInletDistkm (correlated w/ avgssal) and FishingAllnum (correlated w/ gridID)
# cor1 <- cor(check[, c(12:41)]) #abiotic, just indicates to not use individual species with total forage, makes sense
# corrplot(cor1) 

df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% dplyr::select(-InletDistkm, -FishingAllnum, -SAVkm)

##Make year a factor  
df_CPUE_length_wide_both$Yearfactor <- as.factor(df_CPUE_length_wide_both$Year)
df_CPUE_length_wide_P915$Yearfactor <- as.factor(df_CPUE_length_wide_P915$Year)
df_CPUE_length_wide_indP915$Yearfactor <- as.factor(df_CPUE_length_wide_indP915$Year)
# ##Filter out rare species in total forage or prey family forage (individual species don't matter)
# test <- df_CPUE_length_wide_both %>% dplyr::select(which(sapply(., function(col) sum(col>0) >= 50)))
# t <- setdiff(colnames(df_CPUE_length_wide_both), colnames(test)) #paralichthyidae_P915

# ###Remove paralichthyidaeP915
# df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% dplyr::select(-paralichthyidaeP915)

##Get rid of 2022
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% filter(Year < 2022)
df_CPUE_length_wide_P915 <- df_CPUE_length_wide_P915 %>% filter(between(Year, 2001, 2019), Month %in% c(5,6))
#df_CPUE_length_wide_indP915 is already filtered for < 2022 

##Add log of prey items
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% mutate(logsmallbluecrabP120= log(smallbluecrabP120+1), logreddrumP915forageP915= log(reddrumP915forageP915+1), logreddrumP915forageP120= log(reddrumP915forageP120+1), logsouthernkingfishP915forageP915= log(southernkingfishP915forageP915+1), logsouthernkingfishP915forageP120= log(southernkingfishP915forageP120+1), logblackdrumP915forageP120= log(blackdrumP915forageP120+1))
