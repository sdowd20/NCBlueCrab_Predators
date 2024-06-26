
#Data, code, and functions to load to create manuscript figures and tables. 

######### Packages, ggplot theme, and grids #########

packages <- c("ggplot2", "tidyverse", "lubridate", "sf", "sp", "dplyr", "rnaturalearth", "readr", "readxl", "spatialEco", "rstatix", "viridis", "BBmisc", "corrplot")
invisible(lapply(packages, library, character.only= TRUE))

standard_theme <- theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.text.align= 0, legend.title= element_text(size = 12), legend.text = element_text(size= 10), axis.text=element_text(size=10), axis.title=element_text(size=12))
world <- ne_countries(scale = "medium", returnclass = "sf")

##Load grids
globe_bb <- matrix(c(-78.900147,33.802938,
                     -78.900147,36.672128,
                     -75.263672,36.672128,
                     -75.263672, 33.802938,
                     -78.900147,33.802938), byrow = TRUE, ncol = 2) %>% list() %>% st_polygon() %>% st_sfc(., crs = 4326)
globe_grid_30 <- st_make_grid(x= globe_bb, n = c(30,30), crs = 4326, what = 'polygons') %>% st_sf('geometry' = ., data.frame('ID' = 1:length(.))) #grid is 0.13° by 0.13° (14.4 km)

globe_grid_60 <- st_make_grid(x= globe_bb, n = c(60,60), crs = 4326, what = 'polygons') %>% st_sf('geometry' = ., data.frame('ID' = 1:length(.))) #grid is 0.06° by 0.06° (5.5 km b/c half of 0.13 which was 10)

globe_grid_15 <- st_make_grid(x= globe_bb, n = c(15,15), crs = 4326, what = 'polygons') %>% st_sf('geometry' = ., data.frame('ID' = 1:length(.))) 

######### FIGURE 1 #########

#Load data 
p915_CPUE <- read_csv("/users/sallydowd/Google Drive/My Drive/Research/Ch1Data/P915/p915_CPUE_new.csv")
setwd("~/Documents/GitHub/NCBlueCrab_Predators")
p120_CPUE <- read_csv("Data/P120/Finalized/p120_CPUE.csv")

#Filter datasets
p915_CPUEedt <- p915_CPUE %>% dplyr::select(-...1, -Obs, -Weather, -Weather, -Sedsize, -Btmcomp, -doy, -Wbdytype, -Wbd, -Sample, -Ph, -Quad, -Secchi) %>% dplyr::rename("Location" = "Area", "Strata/station"= "Strata") %>% filter(between(Year, 2001, 2019)) 
p120_CPUEedt <- p120_CPUE %>% filter(Core== 1|Core==2) %>% dplyr::select(-...1, -Program, -Nbrrec3, -Time, -Duration, -doy, -Core, -Sedsize, -Btmcomp, -Secchi) %>% dplyr::rename("Strata/station"= "Station") %>% filter(between(Year, 2001, 2019)) %>% dplyr::select(-Photoperiod)
p915_CPUEedt <- p915_CPUEedt %>% drop_na(Latitude, Longitude)
p915_CPUEedt$Survey <- "P915"
p120_CPUEedt <- p120_CPUEedt  %>% drop_na(Latitude, Longitude)
p120_CPUEedt$Survey <- "P120"
CPUE_all <- rbind(p915_CPUEedt, p120_CPUEedt)
CPUE_all$lat_lon <- paste(CPUE_all$Latitude, CPUE_all$Longitude, sep= "_")

#0.06 x 0.06 size
gridded_0.06 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/gridded_10.02.23.5km.csv")
#Join CPUE data to gridded dataset

#Join CPUE dataset to lat/lons and grid IDs
CPUE_all_grid <- CPUE_all %>% left_join(gridded_0.06, by= "lat_lon") %>% dplyr::select(-X) %>% drop_na(gridID)
summary(is.na(CPUE_all_grid))

#Small outlier filtering through CPUE and count dataset formation 
CPUE_grid_count <- CPUE_all_grid %>% group_by(gridID, Month, Survey, Year, Speciescommonname) %>% mutate(avg_count= mean(Colnum))

CPUE_grid_count_avg <- CPUE_grid_count %>% ungroup() %>% mutate(Stemp= ifelse(CPUE_grid_count$Stemp <= 11.8, NA, CPUE_grid_count$Stemp), Ssal= ifelse(CPUE_grid_count$Ssal==0.0, NA, CPUE_grid_count$Ssal), Sdo= ifelse(CPUE_grid_count$Sdo==0.0, NA, CPUE_grid_count$Sdo), Depth= ifelse(CPUE_grid_count$Depth==0.0, NA, CPUE_grid_count$Depth)) %>% group_by(Year, gridID, Month) %>% mutate(avg_depth= mean(Depth, na.rm= TRUE), avg_ssal= mean(Ssal, na.rm= TRUE), avg_bsal= mean(Bsal, na.rm= TRUE), avg_stemp= mean(Stemp, na.rm= TRUE), avg_btemp= mean(Btemp, na.rm= TRUE), avg_sdo= mean(Sdo, na.rm= TRUE), avg_bdo= mean(Bdo, na.rm= TRUE), avg_latitude= mean(Latitude, na.rm= TRUE), avg_longitude= mean(Longitude), na.rm= TRUE) %>% dplyr::select(-avg_bsal, -avg_btemp, -avg_bdo, -Stemp, -Btemp, -Sdo, -Bdo, -Ssal, -Bsal, -Season, -Sedsize_new, -Btmcomp_new, -Depth, -na.rm, -avg_latitude, -avg_longitude)

summary(is.na(CPUE_grid_count_avg))
CPUE_grid_count_avg <- CPUE_grid_count_avg %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% drop_na() #drops 4610 points 

CPUE_grid_count_avg_edt <- CPUE_grid_count_avg %>% dplyr::select(-Latitude, -Longitude, -lat_lon, -Day, -Location, -Colnum, -Date, -Ym_date, -Sciname, -"Strata/station", -Month, -Control1)
CPUE_grid_count_avg_edt <- CPUE_grid_count_avg_edt %>% group_by(Year, gridID, Speciescommonname, Survey, Month) %>% distinct(avg_count, .keep_all= TRUE)

df_count <- CPUE_grid_count_avg_edt 
df_count$Speciescommonname <- gsub(" ", "", df_count$Speciescommonname)
colnames(df_count) <- gsub(pattern = "_", replacement = "", colnames(df_count))
df_count$SpeciesSurvey <- paste(df_count$Speciescommonname, df_count$Survey, sep= "")
df_count_wide_both06 <- df_count %>% filter(Survey == "P915"|Survey == "P120") %>% ungroup() %>% dplyr::select(-c(Speciescommonname, Survey)) %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "avgcount") %>% drop_na()

#join P915 CPUE and P120 CPUE with gridID dataset
gridID_both06 <- unique(df_count_wide_both06$gridID) 
gridded06 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/gridded_10.02.23.5km.csv")

p915_CPUEedt$lat_lon <- paste(p915_CPUEedt$Latitude, p915_CPUEedt$Longitude, sep= "_")
p915_CPUEedt_fig06 <- p915_CPUEedt %>% left_join(gridded06, by= "lat_lon") %>% filter(gridID %in% gridID_both06)

p120_CPUEedt$lat_lon <- paste(p120_CPUEedt$Latitude, p120_CPUEedt$Longitude, sep= "_")
p120_CPUEedt_fig06 <- p120_CPUEedt %>% left_join(gridded06, by= "lat_lon") %>% filter(gridID %in% gridID_both06)

#filter gridID dataset for only gridIDs with data
globe_grid_60_edt <- globe_grid_60 %>% dplyr::rename("gridID"= "ID") %>% filter(gridID %in% gridID_both06)
# 0.24 x 0.24 size
gridded_0.24 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/gridded_24km.01.30.24.csv")

#Join CPUE dataset to lat/lons and grid IDs
CPUE_all_grid <- CPUE_all %>% left_join(gridded_0.24, by= "lat_lon") %>% dplyr::select(-X) %>% drop_na(gridID)
summary(is.na(CPUE_all_grid))

#Small outlier filtering through CPUE and count dataset formation 
CPUE_grid_count <- CPUE_all_grid %>% group_by(gridID, Month, Survey, Year, Speciescommonname) %>% mutate(avg_count= mean(Colnum))

CPUE_grid_count_avg <- CPUE_grid_count %>% ungroup() %>% mutate(Stemp= ifelse(CPUE_grid_count$Stemp <= 11.8, NA, CPUE_grid_count$Stemp), Ssal= ifelse(CPUE_grid_count$Ssal==0.0, NA, CPUE_grid_count$Ssal), Sdo= ifelse(CPUE_grid_count$Sdo==0.0, NA, CPUE_grid_count$Sdo), Depth= ifelse(CPUE_grid_count$Depth==0.0, NA, CPUE_grid_count$Depth)) %>% group_by(Year, gridID, Month) %>% mutate(avg_depth= mean(Depth, na.rm= TRUE), avg_ssal= mean(Ssal, na.rm= TRUE), avg_bsal= mean(Bsal, na.rm= TRUE), avg_stemp= mean(Stemp, na.rm= TRUE), avg_btemp= mean(Btemp, na.rm= TRUE), avg_sdo= mean(Sdo, na.rm= TRUE), avg_bdo= mean(Bdo, na.rm= TRUE), avg_latitude= mean(Latitude, na.rm= TRUE), avg_longitude= mean(Longitude), na.rm= TRUE) %>% dplyr::select(-avg_bsal, -avg_btemp, -avg_bdo, -Stemp, -Btemp, -Sdo, -Bdo, -Ssal, -Bsal, -Season, -Sedsize_new, -Btmcomp_new, -Depth, -na.rm, -avg_latitude, -avg_longitude)

summary(is.na(CPUE_grid_count_avg))
CPUE_grid_count_avg <- CPUE_grid_count_avg %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% drop_na() #drops 4610 points 

CPUE_grid_count_avg_edt <- CPUE_grid_count_avg %>% dplyr::select(-Latitude, -Longitude, -lat_lon, -Day, -Location, -Colnum, -Date, -Ym_date, -Sciname, -"Strata/station", -Month, -Control1)
CPUE_grid_count_avg_edt <- CPUE_grid_count_avg_edt %>% group_by(Year, gridID, Speciescommonname, Survey, Month) %>% distinct(avg_count, .keep_all= TRUE)

df_count <- CPUE_grid_count_avg_edt 
df_count$Speciescommonname <- gsub(" ", "", df_count$Speciescommonname)
colnames(df_count) <- gsub(pattern = "_", replacement = "", colnames(df_count))
df_count$SpeciesSurvey <- paste(df_count$Speciescommonname, df_count$Survey, sep= "")
df_count_wide_both24 <- df_count %>% filter(Survey == "P915"|Survey == "P120") %>% ungroup() %>% dplyr::select(-c(Speciescommonname, Survey)) %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "avgcount") %>% drop_na()

#join P915 CPUE and P120 CPUE with gridID dataset
gridID_both24 <- unique(df_count_wide_both24$gridID) 
gridded24 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/gridded_24km.01.30.24.csv")

p915_CPUEedt$lat_lon <- paste(p915_CPUEedt$Latitude, p915_CPUEedt$Longitude, sep= "_")
p915_CPUEedt_fig24 <- p915_CPUEedt %>% left_join(gridded24, by= "lat_lon") %>% filter(gridID %in% gridID_both24)

p120_CPUEedt$lat_lon <- paste(p120_CPUEedt$Latitude, p120_CPUEedt$Longitude, sep= "_")
p120_CPUEedt_fig24 <- p120_CPUEedt %>% left_join(gridded24, by= "lat_lon") %>% filter(gridID %in% gridID_both24)

#filter gridID dataset for only gridIDs with data
globe_grid_15_edt <- globe_grid_15 %>% dplyr::rename("gridID"= "ID") %>% filter(gridID %in% gridID_both24)

# 0.12 x 0.12 size
#Load data
df_count <- read_csv("~/Google Drive/My Drive/Research/Ch1Data/CPUE/CPUE_grid_count_avg_edt.11.21.23.csv")

df_count <- df_count %>% dplyr::select(-c(...1, CPUE, CPUE_stdzd, mean_CPUE, mean_CPUE_stdzd)) #need to remove or R will get confused 
df_count$Speciescommonname <- gsub(" ", "", df_count$Speciescommonname)
colnames(df_count) <- gsub(pattern = "_", replacement = "", colnames(df_count))
df_count$SpeciesSurvey <- paste(df_count$Speciescommonname, df_count$Survey, sep= "")
df_count_wide_both <- df_count %>% filter(Survey %in% "P915"|Survey %in% "P120") %>% dplyr::select(-Speciescommonname, -Survey) %>% ungroup() %>% pivot_wider(names_from = "SpeciesSurvey", values_from = "avgcount") %>% drop_na()

#join P915 CPUE and P120 CPUE with gridID dataset
gridID_both <- unique(df_count_wide_both$gridID) 
gridded12 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/gridded_11.20.23.csv")

p915_CPUEedt$lat_lon <- paste(p915_CPUEedt$Latitude, p915_CPUEedt$Longitude, sep= "_")
p915_CPUEedt_fig12 <- p915_CPUEedt %>% left_join(gridded12, by= "lat_lon") %>% filter(gridID %in% gridID_both)

p120_CPUEedt$lat_lon <- paste(p120_CPUEedt$Latitude, p120_CPUEedt$Longitude, sep= "_")
p120_CPUEedt_fig12 <- p120_CPUEedt %>% left_join(gridded12, by= "lat_lon") %>% filter(gridID %in% gridID_both)

#filter gridID dataset for only gridIDs with data
globe_grid_30_edt <- globe_grid_30 %>% dplyr::rename("gridID"= "ID") %>% filter(gridID %in% gridID_both)


######### FIGURE A.5 #########
#Length datasets
p915_len_f <- read.csv("/users/sallydowd/Google Drive/My Drive/Research/Ch1Data/P915/p915_length_final.csv")
p915_len_f <- p915_len_f %>% dplyr::select(-X) %>% filter(between(Year, 2001, 2022), Month %in% c(5,6))
P120_bioledt <- read_csv("/users/sallydowd/Google Drive/My Drive/Research/Ch1Data/P120/p120_biol_new.csv")
P120_bioledt <- P120_bioledt %>% filter(Core==1|Core==2) %>% filter(between(Year, 2001, 2022), Month %in% c(5,6))

#P915
species <- p915_len_f %>% filter(Speciescommonname %in% c("atlantic croaker", "atlantic menhaden", "spot", "southern flounder", "pinfish", "red drum", "black drum", "southern kingfish")) %>% dplyr::select(Sample, Speciescommonname, Colnum, Length, Control2, Linenum)

##Count # of individuals of a length for a mesh size, sum_count as # of individuals total 
species_edt <- species %>% group_by(Sample, Colnum, Length, Control2, Speciescommonname) %>% summarize(count= n()) %>% ungroup() %>% group_by(Sample, Colnum, Control2, Speciescommonname) %>% mutate(sum_count= sum(count)) 

##Counted # of observations at each length for a sample, computed a frequency, multiplied frequency by total Colnum
species2_freq <- species_edt %>% group_by(Sample, Colnum, Control2, Speciescommonname) %>% mutate(Freq= count/sum_count) %>% ungroup() %>% mutate(Number= Freq*Colnum) #each row as length so don't group by length
p915_length_toplot <- species2_freq %>% group_by(Sample, Length, Speciescommonname) %>% summarize(Num_sum = sum(Number)) %>% rename("Colnum"= "Num_sum")
p915_length_toplot$Survey <- "P915"

#P120 
species_p120 <- P120_bioledt %>% filter(Speciescommonname %in% c("atlantic croaker", "atlantic menhaden", "spot", "southern flounder", "pinfish", "white shrimp", "pink shrimp", "brown shrimp", "blue crab")) %>% dplyr::select(Control1, Date, Location, Colnum, Samnum, Subnum, Linenum, Frequenc, Length, Speciescommonname) %>% drop_na(Colnum, Length)

P120_other_species_edt <- species_p120 %>% group_by(Control1, Colnum, Length, Speciescommonname) %>% uncount(weights=Frequenc, .remove= FALSE) %>% mutate(Frequenc_new = 1) #duplicates rows according to Frequency

P120_os_prop <- P120_other_species_edt %>% group_by(Control1, Length, Colnum, Speciescommonname) %>% mutate(Count= n()) %>% distinct(Count, .keep_all= TRUE) %>% ungroup() %>% group_by(Control1, Colnum, Speciescommonname) %>% mutate(Total_sampled= sum(Count)) %>% ungroup() %>% mutate(Proportion= Count/Total_sampled) %>% dplyr::select(Control1, Date, Speciescommonname, Colnum, Length, Count, Total_sampled, Proportion)

P120_os_prop_edt <- P120_os_prop %>% mutate(Number= Colnum*Proportion) %>% group_by(Control1, Length, Speciescommonname) %>% mutate(Number_new = sum(Number)) %>% distinct(Number_new) %>% rename("Colnum"= "Number_new")
P120_os_prop_edt$Survey <- "P120"

######### FIGURE B.1 #########
library(ggpubr)
library(grid)
plot <- function(smooth_df, smooth2, predictor, df, predictor2, predictor3, predator, xlabel){
  smooth_df %>%
    filter(smooth == smooth2) %>% ggplot() + geom_rug(aes(x= {{predictor}}), data= df) + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = {{predictor}}), alpha = 0.2) + geom_point(aes(x = {{predictor}}, y = {{predictor2}}), data = df, cex = 1.5, colour = "steelblue3")+ geom_line(aes(x = {{predictor}}, y = est), lwd = 1.2) + labs(y = "Partial effect", title = predictor3) + standard_theme + xlab(xlabel) + labs(title=NULL) + theme(axis.title.y=element_blank())
}

df <- df_CPUE_length_wide_both
