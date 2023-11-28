
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

packages <- c("ggplot2", "tidyverse", "lubridate", "sf", "sp", "dplyr", "rnaturalearth", "readr", "readxl", "spatialEco", "rstatix", "viridis", "BBmisc", "corrplot", "mgcv", "GGally", "gjam")
invisible(lapply(packages, library, character.only= TRUE))

standard_theme <- theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.text.align= 0, legend.title= element_text(size = 12), legend.text = element_text(size= 10), axis.text=element_text(size=10), axis.title=element_text(size=12))

world <- ne_countries(scale = "medium", returnclass = "sf")

#Load in datasets, 10 km 
##CPUE
df_CPUE <- read.csv("~/Desktop/Ch1Data/CPUE/CPUE_grid_avg_edt.11.21.23.csv")
df_CPUE <- df_CPUE[,-1]
df_CPUE <- df_CPUE %>% mutate_at(c("Sedsize_common", "ShorelineType", "Survey"), as.factor)
df_CPUE$Speciescommonname <- gsub(" ", "", df_CPUE$Speciescommonname)
colnames(df_CPUE) <- gsub(pattern = "_", replacement = "", colnames(df_CPUE))

##Count dataset 
df_count <- read_csv("~/Desktop/Ch1Data/CPUE/CPUE_grid_count_avg_edt.11.21.23.csv")
df_count <- df_count %>% dplyr::select(-c(...1, CPUE, CPUE_stdzd, mean_CPUE, mean_CPUE_stdzd)) #need to remove or R will get confused 
df_count$Speciescommonname <- gsub(" ", "", df_count$Speciescommonname)
colnames(df_count) <- gsub(pattern = "_", replacement = "", colnames(df_count))

##Binary dataset
df_binary <- read_csv("~/Desktop/Ch1Data/CPUE/CPUE_grid_binary_edt.11.22.23.csv")
df_binary <- df_binary %>% dplyr::select(-c(...1, CPUE, CPUE_stdzd, mean_CPUE, mean_CPUE_stdzd, avg_count))
df_binary$Speciescommonname <- gsub(" ", "", df_binary$Speciescommonname)
colnames(df_binary) <- gsub(pattern = "_", replacement = "", colnames(df_binary))

#Pivot-wider datasets: P915
##CPUE
df_CPUE_wide_P915 <- df_CPUE %>% filter(Survey %in% "P915") %>% ungroup() %>% pivot_wider(names_from = "Speciescommonname", values_from = "meanCPUEstdzd") %>% drop_na()

##Count
df_count_wide_P915 <- df_count %>% filter(Survey %in% "P915") %>% ungroup() %>% pivot_wider(names_from = "Speciescommonname", values_from = "avgcount") %>% drop_na()

##Binary 
df_binary_wide_P915 <- df_binary %>% filter(Survey %in% "P915") %>% ungroup() %>% pivot_wider(names_from = "Speciescommonname", values_from = "binary") %>% drop_na()

#Pivot-wider datasets: P915 and P120 
##CPUE
df_CPUE$SpeciesSurvey <- paste(df_CPUE$Speciescommonname, df_CPUE$Survey, sep= "")
df_CPUE$meanCPUEstdzd <- as.numeric(df_CPUE$meanCPUEstdzd)
df_CPUE_wide_both <- df_CPUE %>% filter(Survey %in% "P120"|Survey %in% "P915") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "meanCPUEstdzd") %>% drop_na()

##Count
df_count$SpeciesSurvey <- paste(df_count$Speciescommonname, df_count$Survey, sep= "")
df_count_wide_both <- df_count %>% filter(Survey %in% "P915"|Survey %in% "P120") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "avgcount") %>% drop_na()

##Binary 
df_binary$SpeciesSurvey <- paste(df_binary$Speciescommonname, df_binary$Survey, sep= "")
df_binary_wide_both <- df_binary %>% filter(Survey %in% "P915"|Survey %in% "P120") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "binary") %>% drop_na()

#Add on forage index to count data 
##P915 
df_count_wide_P915 <- df_count_wide_P915 %>% mutate(reddrumforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, pinfish, spot)), southernkingfishforage = rowSums(dplyr::select(., atlanticmenhaden, atlanticcroaker, spot))) #. allows you to reference dataframe 
df_count_wide_P915 <- df_count_wide_P915 %>% dplyr::select(Month:avgsdo, atlanticcroaker, blackdrum, bluecrab, bonnetheadshark, bullshark, cownoseray, atlanticmenhaden, gizzardshad, reddrum, southernflounder, southernkingfish, spot, stripedbass, stripedmullet, summerflounder, pinfish, sheepshead, spottedseatrout, reddrumforage, southernkingfishforage) %>% filter(!Month %in% 9)

##P915 and P120 
df_count_wide_both <- df_count_wide_both %>% mutate(reddrumP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, pinfishP915, spotP915, atlanticcroakerP120, atlanticmenhadenP120, pinfishP120, spotP120, bluecrabP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120, southernflounderP120, weakfishP120)), southernkingfishP915forage = rowSums(dplyr::select(., atlanticmenhadenP915, atlanticcroakerP915, spotP915, atlanticmenhadenP120, atlanticcroakerP120, spotP120, bluecrabP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), blackdrumP915forage = rowSums(dplyr::select(., bluecrabP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)), bonnetheadsharkP915forage = rowSums(dplyr::select(., bluecrabP120, whiteshrimpP120, pinkshrimpP120, brownshrimpP120)))
df_count_wide_both <- df_count_wide_both %>% dplyr::select(Month:avgsdo, atlanticcroakerP915, blackdrumP915, bluecrabP915, bonnetheadsharkP915, bullsharkP915, cownoserayP915, atlanticmenhadenP915, gizzardshadP915, reddrumP915, southernflounderP915, southernkingfishP915, spotP915, stripedbassP915, stripedmulletP915, summerflounderP915, pinfishP915, sheepsheadP915, spottedseatroutP915, atlanticcroakerP120, atlanticmenhadenP120, bayanchovyP120, bluecrabP120, brownshrimpP120, pinfishP120, pinkshrimpP120, southernflounderP120, spotP120, spottedseatroutP120, stripedanchovyP120, weakfishP120, whiteshrimpP120, bayanchovyP120, silverperchP120, reddrumP915forage:bonnetheadsharkP915forage)
#Red drum: atlantic menhaden, atlantic croaker, blue crab, white shrimp, brown shrimp, pink shrimp, pinfish, southern flounder, spot, weakfish, removed southern flounder b/c could be large
#Black drum: blue crab, white, brown and pink shrimp
#Southern kingfish: Atlantic brief squid, atlantic menhaden, atlantic croaker, blue crab, white, brown and pink shrimp, spot
#Bonnethead shark: atlantic brief squid, blue crab, white, brown and pink shrimp 
