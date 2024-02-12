
#Load packages and functions 
packages <- c("ggplot2", "tidyverse", "lubridate", "sf", "sp", "dplyr", "rnaturalearth", "readr", "readxl", "spatialEco", "rstatix", "viridis", "BBmisc", "corrplot", "mgcv", "GGally", "gjam", "report")
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

standard_theme <- theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.text.align= 0, legend.title= element_text(size = 12), legend.text = element_text(size= 10), axis.text=element_text(size=10), axis.title=element_text(size=12))

world <- ne_countries(scale = "medium", returnclass = "sf")

#Load in datasets
##CPUE
df_CPUE_length <- read.csv("~/Desktop/Ch1Data/CPUE/CPUE_grid_avg_lengthedt.02.12.24.csv")
df_CPUE_length <- df_CPUE_length[,-1]
df_CPUE_length <- df_CPUE_length %>% mutate_at("Survey", as.factor)
df_CPUE_length$Speciescommonname <- gsub(" ", "", df_CPUE_length$Speciescommonname)
colnames(df_CPUE_length) <- gsub(pattern = "_", replacement = "", colnames(df_CPUE_length))

#Pivot-wider datasets: P915
##CPUE
df_CPUE_length_wide_P915 <- df_CPUE_length %>% filter(Survey %in% "P915") %>% ungroup() %>% pivot_wider(names_from = "Speciescommonname", values_from = "meanCPUE") %>% drop_na()

#Pivot-wider dataset: P915 and P120 
##CPUE
df_CPUE_length$SpeciesSurvey <- paste(df_CPUE_length$Speciescommonname, df_CPUE_length$Survey, sep= "")
df_CPUE_length$meanCPUE <- as.numeric(df_CPUE_length$meanCPUE)
df_CPUE_length_wide_both <- df_CPUE_length %>% filter(Survey %in% "P120"|Survey %in% "P915") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "meanCPUE") %>% drop_na()

#Add on forage index to CPUE data
##Total forage 
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., smallatlanticmenhadenP915, smallatlanticcroakerP915, pinfishP915, smallspotP915, smallatlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), southernkingfishP915forage = rowSums(dplyr::select(., smallatlanticmenhadenP915, smallatlanticcroakerP915, smallspotP915, atlanticmenhadenP120, smallatlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))

##Family forage
df_CPUE_length_wide_both <- df_CPUE_length_wide_both %>% mutate(clupeidaeP915 = rowSums(dplyr::select(., smallatlanticmenhadenP915)), 
                                                                clupeidaeP120 = rowSums(dplyr::select(., atlanticmenhadenP120)), 
                                                                sciaenidae_P915 = rowSums(dplyr::select(., smallatlanticcroakerP915, smallspotP915)), 
                                                                sciaenidae_P120 = rowSums(dplyr::select(., smallatlanticcroakerP120, spotP120)),
                                                                sparidae_P915 = rowSums(dplyr::select(., pinfishP915)),
                                                                sparidae_P120 = rowSums(dplyr::select(., pinfishP120)),
                                                                penaied_P120 = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)),
                                                                paralichthyidae_P915 = rowSums(dplyr::select(., smallsouthernflounderP915)),
                                                                paralichthyidae_P120 = rowSums(dplyr::select(., southernflounderP120)))


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
