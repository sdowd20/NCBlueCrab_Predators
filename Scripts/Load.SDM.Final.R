
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
df_CPUE_length <- df_CPUE_length %>% mutate_at(c("Sedsize_common", "ShorelineType", "Survey"), as.factor)
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

#STOPPED HERE
#Add on forage index to count data 
##P915 
df_count_BC_wide_P915 <- df_count_BC_wide_P915 %>% mutate(reddrumforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, pinfish, spot)), southernkingfishforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, spot))) #. allows you to reference dataframe 
df_count_BC_wide_P915 <- df_count_BC_wide_P915 %>% dplyr::select(Month:avgsdo, atlanticcroaker, blackdrum, bluecrab, bonnetheadshark, bullshark, cownoseray, atlanticmenhaden, gizzardshad, reddrum, southernflounder, southernkingfish, spot, stripedbass, stripedmullet, summerflounder, pinfish, sheepshead, spottedseatrout, reddrumforage, southernkingfishforage) %>% filter(!Month %in% 9)

##P915 and P120
df_count_BC_wide_both_gJam <- df_count_BC_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, pinfishP915, spotP915, atlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), southernkingfishP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, spotP915, atlanticmenhadenP120, atlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))
df_count_BC_wide_both_gJam <- df_count_BC_wide_both_gJam %>% dplyr::select(Month:avgsdo, atlanticcroakerP915, blackdrumP915, bluecrabP915, bonnetheadsharkP915, bullsharkP915, cownoserayP915, atlanticmenhadenP915, gizzardshadP915, reddrumP915, southernflounderP915, southernkingfishP915, spotP915, stripedbassP915, stripedmulletP915, summerflounderP915, pinfishP915, sheepsheadP915, spottedseatroutP915, atlanticcroakerP120, atlanticmenhadenP120, bayanchovyP120, smallbluecrabP120, largebluecrabP120, brownshrimpP120, pinfishP120, pinkshrimpP120, southernflounderP120, spotP120, spottedseatroutP120, stripedanchovyP120, weakfishP120, whiteshrimpP120, bayanchovyP120, silverperchP120, reddrumP915forage:blackdrumP915forage)
df_count_BC_wide_both <- df_count_BC_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, pinfishP915, spotP915, atlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), southernkingfishP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, spotP915, atlanticmenhadenP120, atlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))
df_count_BC_wide_both <- df_count_BC_wide_both %>% dplyr::select(Month:avgsdo, atlanticcroakerP915, blackdrumP915, bluecrabP915, bonnetheadsharkP915, bullsharkP915, cownoserayP915, atlanticmenhadenP915, gizzardshadP915, reddrumP915, southernflounderP915, southernkingfishP915, spotP915, stripedbassP915, stripedmulletP915, summerflounderP915, pinfishP915, sheepsheadP915, spottedseatroutP915, atlanticcroakerP120, atlanticmenhadenP120, bayanchovyP120, smallbluecrabP120, largebluecrabP120, brownshrimpP120, pinfishP120, pinkshrimpP120, southernflounderP120, spotP120, spottedseatroutP120, stripedanchovyP120, weakfishP120, whiteshrimpP120, bayanchovyP120, silverperchP120, reddrumP915forage:blackdrumP915forage)
#Red drum: atlantic menhaden, atlantic croaker, blue crab, white shrimp, brown shrimp, pink shrimp, pinfish, southern flounder, spot, weakfish, removed southern flounder b/c could be large
#Black drum: blue crab, white, brown and pink shrimp
#Southern kingfish: Atlantic brief squid, atlantic menhaden, atlantic croaker, blue crab, white, brown and pink shrimp, spot
#Bonnethead shark: blue crab, white, brown and pink shrimp 
#To note: removed blue crab from analysis from forage calculation! 

#Add on forage index to CPUE data: 02/05/24 
##P915 
df_CPUE_BC_wide_P915 <- df_CPUE_BC_wide_P915 %>% mutate(reddrumforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, pinfish, spot)), southernkingfishforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, spot))) #. allows you to reference dataframe 
df_CPUE_BC_wide_P915 <- df_CPUE_BC_wide_P915 %>% dplyr::select(Month:avgsdo, atlanticcroaker, blackdrum, bluecrab, bonnetheadshark, bullshark, cownoseray, atlanticmenhaden, gizzardshad, reddrum, southernflounder, southernkingfish, spot, stripedbass, stripedmullet, summerflounder, pinfish, sheepshead, spottedseatrout, reddrumforage, southernkingfishforage) %>% filter(!Month %in% 9)

##P915 and P120
df_CPUE_BC_wide_both_gJam <- df_CPUE_BC_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, pinfishP915, spotP915, atlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), southernkingfishP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, spotP915, atlanticmenhadenP120, atlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))
df_CPUE_BC_wide_both_gJam <- df_CPUE_BC_wide_both_gJam %>% dplyr::select(Month:avgsdo, atlanticcroakerP915, blackdrumP915, bluecrabP915, bonnetheadsharkP915, bullsharkP915, cownoserayP915, atlanticmenhadenP915, gizzardshadP915, reddrumP915, southernflounderP915, southernkingfishP915, spotP915, stripedbassP915, stripedmulletP915, summerflounderP915, pinfishP915, sheepsheadP915, spottedseatroutP915, atlanticcroakerP120, atlanticmenhadenP120, bayanchovyP120, smallbluecrabP120, largebluecrabP120, brownshrimpP120, pinfishP120, pinkshrimpP120, southernflounderP120, spotP120, spottedseatroutP120, stripedanchovyP120, weakfishP120, whiteshrimpP120, bayanchovyP120, silverperchP120, reddrumP915forage:blackdrumP915forage)
df_CPUE_BC_wide_both <- df_CPUE_BC_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, pinfishP915, spotP915, atlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120)), southernkingfishP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, spotP915, atlanticmenhadenP120, atlanticcroakerP120, spotP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))
df_CPUE_BC_wide_both <- df_CPUE_BC_wide_both %>% dplyr::select(Month:avgsdo, atlanticcroakerP915, blackdrumP915, bluecrabP915, bonnetheadsharkP915, bullsharkP915, cownoserayP915, atlanticmenhadenP915, gizzardshadP915, reddrumP915, southernflounderP915, southernkingfishP915, spotP915, stripedbassP915, stripedmulletP915, summerflounderP915, pinfishP915, sheepsheadP915, spottedseatroutP915, atlanticcroakerP120, atlanticmenhadenP120, bayanchovyP120, smallbluecrabP120, largebluecrabP120, brownshrimpP120, pinfishP120, pinkshrimpP120, southernflounderP120, spotP120, spottedseatroutP120, stripedanchovyP120, weakfishP120, whiteshrimpP120, bayanchovyP120, silverperchP120, reddrumP915forage:blackdrumP915forage)

#Add on family forage index to CPUE data: 02/07/24
##P915 and P120
df_CPUE_BC_wide_both <- df_CPUE_BC_wide_both %>% mutate(clupeidae_forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticmenhadenP120)), 
                                                        sciaenidae_forage = rowSums(dplyr::select(., atlanticcroakerP915, spotP915, atlanticcroakerP120, spotP120)), 
                                                        sparidae_forage = rowSums(dplyr::select(., pinfishP915, pinfishP120)),
                                                        penaied_forage = rowSums(dplyr::select(., whiteshrimpP120, pinkshrimpP120, brownshrimpP120)),
                                                        paralichthyidae_forage = rowSums(dplyr::select(., southernflounderP120)))

#Clupeidae: atlanticmenhadenP915, atlanticmenhadenP120 
#Sciaenidae: atlanticcroakerP915, spotP915, atlanticcroakerP120, spotP120
#Sparidae: pinfishP915, pinfishP120
#Penaied:whiteshrimpP120, pinkshrimpP120, brownshrimpP120,
#Paralichthyidae: southernflounderP120