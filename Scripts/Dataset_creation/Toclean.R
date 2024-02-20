Toclean.R
#Standard flow: remove special characters and replace with NA, make sure there is a month, day, year and date column, str_to_title, no all caps for species, bind with common name dataset, make sure common names are the same as species
#Load packages
library(readxl)
library(stringr)
library(readr)
library(dplyr)
library(lubridate)
library(geosphere)
library(ggplot2)
library(rnaturalearth)
library(ggmap)
library(sf)
library(tidyr)
library(geosphere)

world <- ne_countries(scale = "medium", returnclass = "sf")

#######SPEICES NAMES########
setwd("~/Documents/GitHub/NCBlueCrab_Predators")
P120_speciesnms <- read_csv("Data/Spp_names/P120_speciesnms.csv")
colnames(P120_speciesnms) <- str_to_title(colnames(P120_speciesnms))
P120_speciesnms$Speciescommonname <- str_to_lower(P120_speciesnms$Speciescommonname)
P120_speciesnms$Speciesscientificname <- str_to_lower(P120_speciesnms$Speciesscientificname)
P120_speciesnms_edt <- P120_speciesnms %>% select(Speciescommonname, Sciname)

#######P915########
#P915 OLD
##Creating old CPUE file (p195clean.csv)
#setwd("/Users/sallydowd/Desktop/pamlicodatasets/p915/Data/Individual")
filenames <- list.files("~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Raw/Old_CPUE", pattern= '*.xlsx')  
all <- lapply(filenames, readxl::read_excel)
merged <- do.call(rbind, all)

colnames(merged) <- str_to_title(colnames(merged))
merged$Species <- tolower(merged$Species)

##Edit old CPUE file to include species scientific name
p915 <- read.csv("Data/P915/Raw/p915clean.csv")
p915 <- p915 %>% dplyr::rename("Speciescommonname"= "Species")
p915$Date <- as.Date(paste(p915$Month, p915$Day, p915$Year, sep= "-"), "%m-%d-%Y")
p915_spp <- p915 %>% left_join(spp_list, by= "Speciescommonname") #No NAs for species name, same row #
write.csv(p915_spp, "Data/P915/Finalized/p915clean_new.csv")

t <- p915 %>% filter(Year > 2008) %>% drop_na(Sedsize, Btmcomp)

#P915 NEW
setwd("/Users/sallydowd/Desktop/CPUE")
filenames <- list.files("/Users/sallydowd/Desktop/Ch1Data/CPUE", pattern= '*.xlsx')  
filenames <- filenames[-c(1,16)]
all <- lapply(filenames, readxl::read_excel, col_types= "text") #read in as text to avoid parsing warnings, sedsize and btmcomp was getting messed up
merged <- do.call(rbind, all)
t <- merged %>% filter(Year > 2008, SedSize %in% "***ERROR***") #most with error for Sedsize are before 2008, 21618 in all and 3,285 after 2008
nrow(merged %>% filter(Year > 2008, SedSize %in% NA)) #errors are just NA
t2 <- merged %>% filter(Year > 2008,BtmComp %in% "***ERROR***") #most with error for Btmcomp are before 2008, 25857 in all and 2,322 after 2008
nrow(merged %>% filter(Year > 2008, BtmComp %in% NA)) #errors are just NA
angelshark <- read_excel("/Users/sallydowd/Desktop/CPUE/Angelshark_CPUEupdates.XLS.xlsx", sheet= 2, col_types= "text")
oystertoadfish <- read_excel("/Users/sallydowd/Desktop/CPUE/oystertoadfish_CPUEupdates.XLS.xlsx", sheet= 2, col_types= "text")
merged <- rbind(merged, angelshark, oystertoadfish)
colnames(merged) <- str_to_title(colnames(merged))
merged$Season <- ifelse(merged$Month==4 | merged$Month==5 | merged$Month==6, "Spring", ifelse(merged$Month==9 |merged$Month==10 | merged$Month==11 | merged$Month==12, "Fall", ifelse(merged$Month==7 |merged$Month==8, "Summer", "Winter")))
merged$Date <- as.Date(paste(merged$Month, merged$Day, merged$Year, sep= "-"), "%m-%d-%Y")
merged$Ym_date <- format(merged$Date, "%Y-%m")

#Make Sedsize and Btmcomp consistent: 
unique(merged$Sedsize)
unique(merged$Btmcomp)
merged <- merged %>% mutate(Sedsize_new = ifelse(merged$Sedsize %in% "Mud", 9, ifelse(merged$Sedsize %in% "Sandy mud", 7, ifelse(merged$Sedsize %in% "Sand", 8, ifelse(merged$Sedsize %in% "Muddy sand"|merged$Sedsize %in% "I"| merged$Sedsize %in% "Coarse sand: Coarse silt", 6, 
ifelse(merged$Sedsize %in% "Soft mud", 2, ifelse(merged$Sedsize %in% "Hard sand", 1, ifelse(merged$Sedsize %in% "Clay", 4, ifelse(merged$Sedsize %in% "Hard mud", 3, ifelse(merged$Sedsize %in% "Silt", 5, ifelse(merged$Sedsize %in% "Cemented hard bottom or rock", 0, ifelse(!merged$Sedsize %in% c(NA, "***ERROR***"), merged$Sedsize, NA)))))))))))) #keep the rest of the values the same 
nrow(merged %>% filter(Year> 2008, Sedsize_new %in% NA)) #7300 NA's, this worked 
#Rock, shell, algae isn't on there so classified as rock, shell
#no grass meant no structure (O) prior to 2008 
merged<- merged %>% mutate(Btmcomp_new = ifelse(merged$Btmcomp %in% "Grass", "B", ifelse(merged$Btmcomp %in% "No Grass", "O", ifelse(merged$Btmcomp %in% "Bryozoan", "Q", ifelse(merged$Btmcomp %in% "Grass, Algae", "H", ifelse(merged$Btmcomp %in% "Tunicate", "P",
ifelse(merged$Btmcomp %in% "Algae", "C", ifelse(merged$Btmcomp %in% "Grass, Algae, Detritus", "L", ifelse(merged$Btmcomp %in% "Shell, Algae", "J", ifelse(merged$Btmcomp %in% "Shell", "A", ifelse(merged$Btmcomp %in% "Shell, Grass", "G", ifelse(merged$Btmcomp %in% "Shell, Grass, Algae", "I",
ifelse(merged$Btmcomp %in% "Grass, Detritus", "K", ifelse(merged$Btmcomp %in% "Detritus", "D", ifelse(merged$Btmcomp %in% "Shell, Detritus", "M", ifelse(merged$Btmcomp %in% "Rock, Shell, Algae"| merged$Btmcomp %in% "Rock, Shell", "S", ifelse(merged$Btmcomp %in% "Other", "Z", ifelse(merged$Btmcomp %in% "Rock", "R",
ifelse(merged$Btmcomp %in% "Rock, Algae", "U",ifelse(merged$Btmcomp %in% "Rock, Shell, Grass, Algae", "W", ifelse(merged$Btmcomp %in% "Rock, Grass", "T", ifelse(merged$Btmcomp %in% "Cinder", "N", ifelse(merged$Btmcomp %in% "Shell, Algae", "H", ifelse(!merged$Btmcomp %in% c(NA, "Tow relocated due to Grass (1993)", "***ERROR***"), merged$Btmcomp, NA))))))))))))))))))))))))  
nrow(merged %>% filter(Year> 2008, Btmcomp_new %in% NA)) #only 5171 NAs, this is correct! 
#Coarse sand: Coarse silt had 11 rows and was removed b/c didn't match a classification
#Tow relocated to not grass (1993) had 11 rows and was marked as NA

columns <- c(6, 10:17, 20:22, 25:26)
for (col in columns) {
  merged[[col]][is.na(merged[[col]]) | merged[[col]] == "."] <- NA
} #some values only have a dot or maybe it is an NA that R generated, replace these with NA instead, I checked with Depth and Ssal and it worked! 

P915_CPUE <- merged %>% dplyr::rename("Sciname"= "Species")
P915_CPUE <- P915_CPUE %>% left_join(sppcommonnmsc, by= "Sciname") #No NAs for species name, same row #

#Add other predictor variables
P915_CPUE$doy <- yday(P915_CPUE$Date)
P915_CPUE <- P915_CPUE %>% mutate_at(vars(c(10:17, 25, 26)), as.numeric)
P915_CPUE$Photoperiod <- daylength(lat= P915_CPUE$Latitude, doy= P915_CPUE$doy)
P915_CPUE$Wbdytype <- ifelse(P915_CPUE$Area %in% "PUNGO" | P915_CPUE$Area %in% "NEUSE" | P915_CPUE$Area %in% "NEWR"| P915_CPUE$Area %in% "CAPEF" | P915_CPUE$Area %in% "CAPEF", "River", "Sound")
P915_CPUE$Wbd <- ifelse(P915_CPUE$Area %in% "DARE1" | P915_CPUE$Area %in% "DARE2" | P915_CPUE$Area %in% "DARE3"| P915_CPUE$Area %in% "DARE4" | P915_CPUE$Area %in% "HYDE1"| P915_CPUE$Area %in% "HYDE2"| P915_CPUE$Area %in% "HYDE3"| P915_CPUE$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(P915_CPUE$Area %in% "MHDC1"| P915_CPUE$Area %in% "MHDC2"| P915_CPUE$Area %in% "MHDC3", "MHDC", P915_CPUE$Area))
getwd()
write.csv(P915_CPUE, "/Users/sallydowd/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Finalized/p915_CPUE.csv")
##P915 NEW: Biological data 

#P915 CPUE NEW #2: FINAL!!
setwd("/Users/sallydowd/Desktop/Ch1Data/P915/CPUE_final")
filenames <- list.files("/Users/sallydowd/Desktop/Ch1Data/P915/CPUE_final", pattern= '*.csv') #update on 11/21/23: deleted duplicate files of hickory shad and sheepshead
all <- lapply(filenames, readr::read_csv)
merged <- do.call(rbind, all)
filenames2 <- list.files("/Users/sallydowd/Desktop/Ch1Data/P915/CPUE_final", pattern= '*.xlsx')
all2 <- lapply(filenames2, readxl::read_excel)
merged2 <- do.call(rbind, all2)
merged <- rbind(merged, merged2)
colnames(merged) <- str_to_title(colnames(merged))
merged$Species <- tolower(merged$Species)
merged$Season <- ifelse(merged$Month==4 | merged$Month==5 | merged$Month==6, "Spring", ifelse(merged$Month==9 |merged$Month==10 | merged$Month==11 | merged$Month==12, "Fall", ifelse(merged$Month==7 |merged$Month==8, "Summer", "Winter")))
merged$Date <- as.Date(paste(merged$Month, merged$Day, merged$Year, sep= "-"), "%m-%d-%Y")
merged$Ym_date <- format(merged$Date, "%Y-%m")

##Remove dots 
columns <- c(11:18, 22)
for (col in columns) {
  merged[[col]][is.na(merged[[col]]) | merged[[col]] == "."] <- NA
} #some values only have a dot or maybe it is an NA that R generated, replace these with NA instead, I checked with Depth and Ssal and it worked! 

merged <- merged %>% mutate_at(vars(11:18), as.numeric)

##Make Sedsize and Btmcomp consistent: No issues here except coarse sand:silt 
merged <- merged %>% mutate(Sedsize = ifelse(merged$Sedsize %in% "coarse sand:coarse silt", "muddy sand", merged$Sedsize))
merged <- merged %>% mutate(Sedsize_new = ifelse(merged$Sedsize %in% "soft mud"|merged$Sedsize %in% "sandy mud"|merged$Sedsize %in% "mud"|merged$Sedsize %in% "hard mud"|merged$Sedsize %in% "clay"|merged$Sedsize %in% "silt", "Mud", ifelse(merged$Sedsize %in% "muddy sand"|merged$Sedsize %in% "sand"|merged$Sedsize %in% "hard sand", "Sand", ifelse(merged$Sedsize %in% "cemented hard bottom or rock", "Hard bottom", NA))))
unique(merged$Btmcomp) 
merged <- merged %>% mutate(Btmcomp_new = ifelse(merged$Btmcomp %in% "shell"|merged$Btmcomp %in% "rock, shell"|merged$Btmcomp %in% "Rock"|merged$Btmcomp %in% "Bryozoan", "Shell", ifelse(merged$Btmcomp %in% "grass, algae, detritus"|merged$Btmcomp %in% "algae"|merged$Btmcomp %in% "detritus"|merged$Btmcomp %in% "grass, algae"|merged$Btmcomp %in% "grass"|merged$Btmcomp %in% "grass, detritus", "Vegetated", ifelse(merged$Btmcomp %in% "shell, algae"|merged$Btmcomp %in% "shell, detritus"|merged$Btmcomp %in% "shell, grass"|merged$Btmcomp %in% "shell, grass, algae"|merged$Btmcomp %in% "rock, shell, algae"|merged$Btmcomp %in% "rock, shell, grass, algae"|merged$Btmcomp %in% "rock, algae"|merged$Btmcomp %in% "rock, grass", "Vegetated-Shell", ifelse(merged$Btmcomp %in% "no grass"|merged$Btmcomp %in% "Tunicate", "Unstructured", ifelse(merged$Btmcomp %in% "Other"|merged$Btmcomp %in% "cinder", "Unnatural", ifelse(merged$Btmcomp %in% "Tow relocated due to Grass(1993)", NA, merged$Btmcomp)))))))
summary(is.na(merged$Btmcomp_new)) #it worked, more NAs from the tow 

#Scientific name, just do species names!
p915_sppnames <- read.csv("/users/sallydowd/Documents/GitHub/NCBlueCrab_Predators/Data/Spp_names/p915_sppnames.csv")
P915_CPUE <- merged %>% dplyr::rename("Speciescommonname"= "Species") %>% left_join(p915_sppnames, by= "Speciescommonname")

##Add in other predictor variables
library(geosphere)
P915_CPUE$doy <- yday(P915_CPUE$Date)
#P915_CPUE$Photoperiod <- daylength(lat= P915_CPUE$Latitude, doy= P915_CPUE$doy)
P915_CPUE$Wbdytype <- ifelse(P915_CPUE$Area %in% "PUNGO" | P915_CPUE$Area %in% "NEUSE" | P915_CPUE$Area %in% "NEWR"| P915_CPUE$Area %in% "CAPEF" | P915_CPUE$Area %in% "CAPEF", "River", "Sound")
P915_CPUE$Wbd <- ifelse(P915_CPUE$Area %in% "DARE1" | P915_CPUE$Area %in% "DARE2" | P915_CPUE$Area %in% "DARE3"| P915_CPUE$Area %in% "DARE4" | P915_CPUE$Area %in% "HYDE1"| P915_CPUE$Area %in% "HYDE2"| P915_CPUE$Area %in% "HYDE3"| P915_CPUE$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(P915_CPUE$Area %in% "MHDC1"| P915_CPUE$Area %in% "MHDC2"| P915_CPUE$Area %in% "MHDC3", "MHDC", P915_CPUE$Area))
P915_CPUE <- P915_CPUE %>% rename("Secchi"= "Depthend")
write.csv(P915_CPUE, "/Users/sallydowd/Desktop/Ch1Data/P915/p915_CPUE_new.csv") #saved again on 02/05/24 b/c accidently saved P135 as this

#p915_biol1 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new1.xlsx")
#write.csv(p915_biol1, "Data/P915/Raw/p915_biol1new.csv")

p915_biol1 <- read_csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Raw/p915_biol1new.csv")
colnames(p915_biol1) <- str_to_title(colnames(p915_biol1))
p915_biol1$Location <- str_to_title(p915_biol1$Location)
p915_biol1 <- p915_biol1 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol1_apply <- as.data.frame(sapply(p915_biol1[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol1_apply[p915_biol1_apply== "ERROR"] <- NA
p915_biol1[ , colnames(p915_biol1) %in% colnames(p915_biol1_apply)] <- p915_biol1_apply #replace updated columns in original dataset
p915_biol1$Season <- ifelse(p915_biol1$Month==4 | p915_biol1$Month==5 | p915_biol1$Month==6, "Spring", ifelse(p915_biol1$Month==9 |p915_biol1$Month==10 | p915_biol1$Month==11 | p915_biol1$Month==12, "Fall", ifelse(p915_biol1$Month==7 |p915_biol1$Month==8, "Summer", "Winter")))
p915_biol1$Ym_date <- format(p915_biol1$Date, "%Y-%m")
##Add other predictor variables
p915_biol1$doy <- yday(p915_biol1$Date)
p915_biol1$Photoperiod <- daylength(lat= p915_biol1$Latitude, doy= p915_biol1$doy)
p915_biol1$Wbdytype <- ifelse(p915_biol1$Area %in% "PUNGO" | p915_biol1$Area %in% "NEUSE" | p915_biol1$Area %in% "NEWR"| p915_biol1$Area %in% "CAPEF" | p915_biol1$Area %in% "CAPEF", "River", "Sound")
p915_biol1$Wbd <- ifelse(p915_biol1$Area %in% "DARE1" |p915_biol1$Area %in% "DARE2" | p915_biol1$Area %in% "DARE3"| p915_biol1$Area %in% "DARE4" | p915_biol1$Area %in% "HYDE1"| p915_biol1$Area %in% "HYDE2"| p915_biol1$Area %in% "HYDE3"| p915_biol1$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(p915_biol1$Area %in% "MHDC1"| p915_biol1$Area %in% "MHDC2"| p915_biol1$Area %in% "MHDC3", "MHDC", p915_biol1$Area))

p915_biol2 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Raw/p915_biol2new.csv")
colnames(p915_biol2) <- str_to_title(colnames(p915_biol2))
p915_biol2$Location <- str_to_title(p915_biol2$Location)
p915_biol2 <- p915_biol2 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol2_apply <- as.data.frame(sapply(p915_biol2[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol2_apply[p915_biol2_apply== "ERROR"] <- NA
p915_biol2[ , colnames(p915_biol2) %in% colnames(p915_biol2_apply)] <- p915_biol2_apply #replace updated columns in original dataset
p915_biol2$Season <- ifelse(p915_biol2$Month==4 | p915_biol2$Month==5 | p915_biol2$Month==6, "Spring", ifelse(p915_biol2$Month==9 |p915_biol2$Month==10 | p915_biol2$Month==11 | p915_biol2$Month==12, "Fall", ifelse(p915_biol2$Month==7 |p915_biol2$Month==8, "Summer", "Winter")))
p915_biol2$Date <- as.Date(p915_biol2$Date, "%Y-%m-%d")
p915_biol2$Ym_date <- format(p915_biol2$Date, "%Y-%m")

##Add other predictor variables : stopped here! 
p915_biol2$doy <- yday(p915_biol2$Date)
p915_biol2$Photoperiod <- daylength(lat= p915_biol2$Latitude, doy= p915_biol2$doy)
p915_biol2$Wbdytype <- ifelse(p915_biol2$Area %in% "PUNGO" | p915_biol2$Area %in% "NEUSE" | p915_biol2$Area %in% "NEWR"| p915_biol2$Area %in% "CAPEF"| p915_biol2$Area %in% "PAMLI", "River", "Sound")
p915_biol2$Wbd <- ifelse(p915_biol2$Area %in% "DARE1" |p915_biol2$Area %in% "DARE2" | p915_biol2$Area %in% "DARE3"| p915_biol2$Area %in% "DARE4" | p915_biol2$Area %in% "HYDE1"| p915_biol2$Area %in% "HYDE2"| p915_biol2$Area %in% "HYDE3"| p915_biol2$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(p915_biol2$Area %in% "MHDC1"| p915_biol2$Area %in% "MHDC2"| p915_biol2$Area %in% "MHDC3"| p915_biol2$Area %in% "MHDC4", "MHDC", p915_biol2$Area))
#To note: MISS has 4 data points of a creek (Queen's Creek) but that's labeled as sound 

###Adding species commonnames
p915_biol1 <- p915_biol1 %>% dplyr::rename("Sciname"= "Species")
p915_biol2 <- p915_biol2 %>% dplyr::rename("Sciname"= "Species")
p915_biol1_new <- p915_biol1 %>% left_join(spp_list, by= "Sciname")
p915_biol2_new <- p915_biol2 %>% left_join(spp_list, by= "Sciname") #no NAs for Sciname for 1st and 2nd df, a couple of NAs for Speciescommonname in 1st and 2nd b/c of angel shark

write.csv(p915_biol1_new, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Finalized/p915_biol1new_clean.csv")
write.csv(p915_biol2_new, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Finalized/p915_biol2new_clean.csv")

#P915 LENGTH NEW
mylist <- lapply(1:17, function(i) read_excel("~/Desktop/Ch1Data/P915/P915LengthData_20231003.xlsx", sheet = i))
combo <- bind_rows(mylist)
colnames(combo) <- str_to_title(colnames(combo))
combo$Season <- ifelse(combo$Month==4 | combo$Month==5 | combo$Month==6, "Spring", ifelse(combo$Month==9 |combo$Month==10 | combo$Month==11 | combo$Month==12, "Fall", ifelse(combo$Month==7 |combo$Month==8, "Summer", "Winter")))
combo$Date <- as.Date(paste(combo$Month, combo$Day, combo$Year, sep= "-"), "%m-%d-%Y")
combo$Ym_date <- format(combo$Date, "%Y-%m")

#Scientific name, just do species names!
p915_sppnames <- read.csv("/users/sallydowd/Documents/GitHub/NCBlueCrab_Predators/Data/Spp_names/p915_sppnames.csv")
p915_length <- combo %>% dplyr::rename("Sciname"= "Species") %>% left_join(p915_sppnames, by= "Sciname") #no NAs for species (works!)

p915_length$doy <- yday(p915_length$Date)
p915_length$Photoperiod <- daylength(lat= p915_length$Latitude, doy= p915_length$doy)
p915_length$Wbdytype <- ifelse(p915_length$Area %in% "PUNGO" | p915_length$Area %in% "NEUSE" | p915_length$Area %in% "NEWR"| p915_length$Area %in% "CAPEF" | p915_length$Area %in% "CAPEF", "River", "Sound")
p915_length$Wbd <- ifelse(p915_length$Area %in% "DARE1" | p915_length$Area %in% "DARE2" | p915_length$Area %in% "DARE3"| p915_length$Area %in% "DARE4" | p915_length$Area %in% "HYDE1"| p915_length$Area %in% "HYDE2"| p915_length$Area %in% "HYDE3"| p915_length$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(p915_length$Area %in% "MHDC1"| p915_length$Area %in% "MHDC2"| p915_length$Area %in% "MHDC3", "MHDC", p915_length$Area))
write.csv(p915_length, "/Users/sallydowd/Desktop/Ch1Data/P915/p915_length_new.csv")

#P915 LENGTH FINAL
length_p915 <- read_csv("~/Desktop/Ch1Data/P915/P915_Lengths_UPDATED_20231030.csv")
colnames(length_p915) <- str_to_title(colnames(length_p915))
length_p915$Season <- ifelse(length_p915$Month==4 | length_p915$Month==5 | length_p915$Month==6, "Spring", ifelse(length_p915$Month==9 |length_p915$Month==10 | length_p915$Month==11 | length_p915$Month==12, "Fall", ifelse(length_p915$Month==7 |length_p915$Month==8, "Summer", "Winter")))
length_p915$Date <- as.Date(paste(length_p915$Month, length_p915$Day, length_p915$Year, sep= "-"), "%m-%d-%Y")
length_p915$Ym_date <- format(length_p915$Date, "%Y-%m")

#Scientific name, just do species names!
p915_sppnames <- read.csv("/users/sallydowd/Documents/GitHub/NCBlueCrab_Predators/Data/Spp_names/p915_sppnames.csv")
length_p915$Species <- str_to_lower(length_p915$Species)
p <- length_p915 %>% dplyr::rename("Speciescommonname"= "Species") %>% left_join(p915_sppnames, by= "Speciescommonname") #no NAs for species (works!)
unique(p$Sciname) #no NAs, this worked

p$doy <- yday(p$Date)
p$Photoperiod <- daylength(lat= p$Latitude, doy= p$doy)
p$Wbdytype <- ifelse(p$Area %in% "PUNGO" | p$Area %in% "NEUSE" | p$Area %in% "NEWR"| p$Area %in% "CAPEF" | p$Area %in% "CAPEF", "River", "Sound")
p$Wbd <- ifelse(p$Area %in% "DARE1" | p$Area %in% "DARE2" | p$Area %in% "DARE3"| p$Area %in% "DARE4" | p$Area %in% "HYDE1"| p$Area %in% "HYDE2"| p$Area %in% "HYDE3"| p$Area %in% "HYDE4", "PAMLICO SOUND", ifelse(p$Area %in% "MHDC1"| p$Area %in% "MHDC2"| p$Area %in% "MHDC3", "MHDC", p$Area))

write.csv(p, "/Users/sallydowd/Desktop/Ch1Data/P915/p915_length_final.csv")

#######P120########
#P120 OLD
p120 <- read_csv("Data/P120/Raw/p120_clean_2021.csv")
colnames(p120) <- str_to_title(colnames(p120))
p120 <- p120 %>% dplyr::rename("Sciname" = "Species")
P120_speciesnms_edt2 <- P120_speciesnms %>% distinct(Sciname, .keep_all= TRUE)
p120_edt <- p120 %>% dplyr::left_join(P120_speciesnms_edt2, by= "Sciname")
p120_edt$Speciescommonname[p120_edt$Speciescommonname %in% "northern brown shrimp"] <- "brown shrimp" #check which ones didn't match spp_list in P120 to change

write.csv(p120_edt, "Data/P120/Finalized/p120_clean_2021new.csv")

##Catch data (no biological)
p120_speciesn <- p120_speciesn %>% rename("Species" = "SciName") %>% select(Species, SPECIESCOMMONNAME)
p120_edt <- p120 %>% left_join(p120_speciesn, by= "Species")
colnames(p120_edt) <- str_to_title(colnames(p120_edt))
p120_edt[[39]] <- tolower(p120_edt[[39]])

##P120 NEW: Biological data: through present  
setwd("~/Desktop/Ch1Data/P120")
df=read.delim("P120_1019.txt",sep="$",header=TRUE,dec=".")
df2=read.delim("P120_7279.txt",sep="$",header=TRUE,dec=".")
df3=read.delim("P120_8089.txt",sep="$",header=TRUE,dec=".")
df4=read.delim("P120_9099.txt",sep="$",header=TRUE,dec=".")
df5=read.delim("P120_0009.txt",sep="$",header=TRUE,dec=".")
fulld=rbind(df2,df3,df4,df5, df)
head(fulld)

updated_df <- read_csv("P120_1022_UPDATED.CSV")
fulld2 <- rbind(fulld, updated_df)
colnames(fulld2) <- str_to_title(colnames(fulld2))
fulld2=fulld2%>%mutate(Date=make_date(Year,Month,Day))
fulld2$Control1=as.factor(fulld2$Control1)
SpeciesNotIncluded <- read_excel("SpeciesNotIncluded.xls")
species_names <- read_csv("species_names.csv") #with the matching P120 species format and a bunch of species I added at the bottom
species_namesedt <- species_names %>% select(SPECIESSCIENTIFICNAME, SPECIESCOMMONNAME, Species) %>% distinct(Species, .keep_all = TRUE) #remove matching rows, butterfly ray had two of same, 389--> 385 observations
colnames(species_namesedt) <- str_to_title(colnames(species_namesedt))
species_namesedt$Speciesscientificname <- str_to_lower(species_namesedt$Speciesscientificname)
species_namesedt$Speciescommonname <- str_to_lower(species_namesedt$Speciescommonname)
fulld2_edt <- fulld2 %>% left_join(species_namesedt, by = "Species") %>% rename("Sciname"= "Species") #NAs in common name from plants, jellys, non-sessile things and airbreathers (Lela edited these out)
#fulld=fulld%>%filter(CORE==1|CORE==2)%>%as.data.frame() #core stations
fulld2_edt <- fulld2_edt %>% rename("Latitude"= "Lat_dd", "Longitude"= "Long_dd")

fulld2_edt$Year <- as.numeric(fulld2_edt$Year)
fulld2_edt <- fulld2_edt %>% arrange(Year)
fulld2_edt <- fulld2_edt %>% filter(Year > 1988)
fulld2_edt$Season <- ifelse(fulld2_edt$Month==4 | fulld2_edt$Month==5 | fulld2_edt$Month==6, "Spring", ifelse(fulld2_edt$Month==9 |fulld2_edt$Month==10 | fulld2_edt$Month==11 | fulld2_edt$Month==12, "Fall", ifelse(fulld2_edt$Month==7 |fulld2_edt$Month==8, "Summer", "Winter")))
fulld2_edt$Ym_date <- format(fulld2_edt$Date, "%Y-%m")
fulld2_edt$Location <- as.numeric(fulld2_edt$Location) #remove leading 0s, make sure this is ok, 184 locations w/ out this and 101 locations with this, I checked the data and the data is difference between locations (e.g: 0527010000 vs. 527010000). The station, grid and quad are the same though! #removes leading zeros, 101 locations is the same in old processed data (p120_clean_2021) 

##Add in other predictor variables 
fulld2_edt$doy <- yday(fulld2_edt$Date)
fulld2_edt$Photoperiod <- daylength(lat= fulld2_edt$Latitude, doy= fulld2_edt$doy)

#Make Sedsize and Btmcomp consistent: 
unique(fulld2_edt$Btmcomp) #Z isn't in P120 but is other in P915
#removes only 53 entries after 2008 from two sampling days
unique(fulld2_edt$Sedsize) #0 to 9 is normal, 76% of all data is this, stations in Wilmington are the ones maybe using secondary codes
#367 that are "" (NA) and 285 that are NA after 2007 #367 after 2007 
fulld2_edt <- fulld2_edt %>% mutate(Sedsize_new= ifelse(fulld2_edt$Sedsize %in% "S"|fulld2_edt$Sedsize %in% "X"|fulld2_edt$Sedsize %in% "Y", 7, ifelse(fulld2_edt$Sedsize %in% "T"|fulld2_edt$Sedsize %in% "P", 3, ifelse(fulld2_edt$Sedsize %in% "U", 4, ifelse(fulld2_edt$Sedsize %in% "O"|fulld2_edt$Sedsize %in% "I"|fulld2_edt$Sedsize %in% "N", 6, ifelse(fulld2_edt$Sedsize %in% "B", 1, fulld2_edt$Sedsize)))))) #Same amount of NAs as before 
#08/17/23: reclassify sediment size 
fulld2_edt <- fulld2_edt %>% mutate(Sedsize_new= ifelse(fulld2_edt$Sedsize_new == 1|fulld2_edt$Sedsize_new == 6|fulld2_edt$Sedsize_new == 8, "Sand", ifelse(fulld2_edt$Sedsize_new== 2|fulld2_edt$Sedsize_new== 3|fulld2_edt$Sedsize_new== 4| fulld2_edt$Sedsize_new== 5| fulld2_edt$Sedsize_new== 7| fulld2_edt$Sedsize_new== 9, "Mud", ifelse(fulld2_edt$Sedsize_new== 0, "Hard bottom", fulld2_edt$Sedsize_new))))
fulld2_edt <- fulld2_edt %>% mutate(Btmcomp_new = ifelse(fulld2_edt$Btmcomp %in% "A"|fulld2_edt$Btmcomp %in% "Q", "Shell", ifelse(fulld2_edt$Btmcomp %in% "L"|fulld2_edt$Btmcomp %in% "H"|fulld2_edt$Btmcomp %in% "C"|fulld2_edt$Btmcomp %in% "B"|fulld2_edt$Btmcomp %in% "K"|fulld2_edt$Btmcomp %in% "D", "Vegetated", ifelse(fulld2_edt$Btmcomp %in% "I"|fulld2_edt$Btmcomp %in% "G"|fulld2_edt$Btmcomp %in% "M", "Vegetated-Shell", ifelse(fulld2_edt$Btmcomp %in% "P"|fulld2_edt$Btmcomp %in% "O"|fulld2_edt$Btmcomp %in% "E", "Unstructured", ifelse(fulld2_edt$Btmcomp %in% "Z", "Unnatural", ifelse(fulld2_edt$Btmcomp %in% "X", NA, fulld2_edt$Btmcomp)))))))
#write.csv(species_namesedt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P120/Finalized/p120_speciesnms_new.csv")
write.csv(fulld2_edt, "~/Desktop/Ch1Data/P120/p120_biol_new.csv")

#######P195########
#P195 OLD
p195 <- read_csv("Data/P195/Raw/p195clean.csv")
colnames(p195) <- str_to_title(colnames(p195))
p195[[12]] <- tolower(p195[[12]])
p195[[13]] <- tolower(p195[[13]])
p195$Speciescommonname[p195$Speciescommonname %in% "northern brown shrimp"] <- "brown shrimp" #check which ones didn't match spp_list in P120 to change
P120_speciesnms_ssn <- P120_speciesnms %>% select(Speciesscientificname, Sciname)
P120_speciesnms_ssn[316, 1] <- "morone saxatilis" 
P120_speciesnms_ssn[316, 2] <- "M. saxatilis" #add in striped bass b/c wasn't in P120 
p195_edt <-p195 %>% left_join(P120_speciesnms_ssn, by= "Speciesscientificname") #All NAs now for Sciname are for species common names not in dataset (we don't need)
write.csv(p195_edt, "Data/P195/Finalized/p195_clean_new.csv")

#P195 NEW: abundance 
setwd("~/Documents/GitHub/NCBlueCrab_Predators/Data")
P195_biomass <- read_csv("P195/Raw/P195.abubio.2023-07-07.csv")
P195_event <- read_csv("P195/Raw/P195.event.2023-07-07.csv")
length(unique(P195_event$EVENTNAME)) 
length(unique(P195_biomass$EVENTNAME)) #same numbers of events, tow #

P195_event_edt <- P195_event %>% select(DATE, EVENTNAME, DEPTHSTART, DEPTHEND, LOCATION, LIGHTPHASE, PRESSURE, SEDSIZEDESC, BTMCOMPDESC) %>% group_by(DATE, DEPTHEND, DEPTHSTART, LOCATION) %>% distinct(EVENTNAME, .keep_all= TRUE)
P195_bind <- P195_biomass %>% left_join(P195_event_edt, by= c("EVENTNAME", "DATE", "LOCATION")) %>% filter(!LOCATION %in% NA)
#added the 18 variables not present in biomass df about event, removed 8 rows that had NA for Location (and almost every cell)

colnames(P195_bind)
merged_apply <- as.data.frame(sapply(P195_bind[,c(1:2, 4:13, 23, 25, 26:28, 29:30, 31, 43, 47, 49:50)], function(x) gsub('[^[:alnum:] ]', "", x))) #remove all special characters
P195_bind[ , colnames(P195_bind) %in% colnames(merged_apply)] <- merged_apply #replace updated columns in original dataset
colnames(P195_bind) <- str_to_title(colnames(P195_bind))
P195_bind$Date <- as.Date(P195_bind$Date, "%m-%d-%Y")
P195_bind$Ym_date <- format(P195_bind$Date, "%Y-%m")
P195_bind$Year <- year(P195_bind$Date)
P195_bind$Month <- month(P195_bind$Date)
P195_bind$Day <- day(P195_bind$Date)
P195_bind <- P195_bind %>% filter(Month == 6| Month== 9, Year > 2000, !Location %in% "ALBEMARLE SOUND")
P195_bind$Season <- ifelse(P195_bind$Month==4 | P195_bind$Month==5 | P195_bind$Month==6, "Spring", ifelse(P195_bind$Month==9 |P195_bind$Month==10 | P195_bind$Month==11 | P195_bind$Month==12, "Fall", ifelse(P195_bind$Month==7 |P195_bind$Month==8, "Summer", "Winter")))
P195_bind$Ym_date <- format(P195_bind$Date, "%Y-%m")

##Add in other predictor variables
P195_bind$doy <- yday(P195_bind$Date)
P195_bind$Photoperiod <- daylength(lat= P195_bind$Latitudestart, doy= P195_bind$doy)
P195_bind$Wbdytype <- ifelse(P195_bind$Location %in% "NEUSE RIVER" | P195_bind$Location %in% "PAMLICO RIVER" | P195_bind$Location %in% "PUNGO RIVER", "River", "Sound")
P195_bind$Wbd <- ifelse(P195_bind$Location %in% "PAMLICO SOUND WEST OF BLUFF SHOAL" | P195_bind$Location %in% "PAMLICO SOUND EAST OF BLUFF SHOAL", "PAMLICO SOUND", P195_bind$Location)

#Adding in Sciname for species
P195_bind$Speciescommonname <- str_to_lower(P195_bind$Speciescommonname)
P195_bind$Speciesscientificname <- str_to_lower(P195_bind$Speciesscientificname)
species_P195 <- read_csv("Spp_names/species.P195.csv")
colnames(species_P195) <- str_to_title(colnames(species_P195))
species_P195$Speciescommonname <- str_to_lower(species_P195$Speciescommonname)
species_P195$Speciesscientificname <- str_to_lower(species_P195$Speciesscientificname)
species_P195_edt <- as.data.frame(species_P195[, c(1,9)])

P195_bind_edt <- P195_bind %>% left_join(species_P195_edt, by= "Speciesscientificname") %>% rename("Sciname"= "Species")
P195_bind_edt$Speciescommonname[P195_bind_edt$Speciescommonname== "northern brown shrimp"] <- "brown shrimp"

#Standardize SedSize and Btmcomp
P195_bind_edt <- P195_bind_edt %>% rename("Sedsize"= "Sedsizedesc", "Btmcomp"= "Btmcompdesc")
P195_bind_edt$Sedsize <- str_to_title(P195_bind_edt$Sedsize)
P195_bind_edt$Btmcomp <- str_to_title(P195_bind_edt$Btmcomp)
unique(P195_bind_edt$Sedsize)

P195_bind_edt <- P195_bind_edt %>% mutate(Sedsize_new = ifelse(P195_bind_edt$Sedsize %in% "Mud", 9, ifelse(P195_bind_edt$Sedsize %in% "Sandy Mud", 7, ifelse(P195_bind_edt$Sedsize %in% "Sand", 8, ifelse(P195_bind_edt$Sedsize %in% "Muddy Sand", 6, ifelse(P195_bind_edt$Sedsize %in% "Soft Mud", 2, 
ifelse(P195_bind_edt$Sedsize %in% "Hard Sand", 1, ifelse(P195_bind_edt$Sedsize %in% "Clay", 4, ifelse(P195_bind_edt$Sedsize %in% "Hard Mud", 3, ifelse(P195_bind_edt$Sedsize %in% "Silt", 5,  NA)))))))))) #keep the rest of the values the same 
nrow(P195_bind_edt %>% filter(Year> 2008, Sedsize_new %in% NA)) #221 NAs before and after Sedsize 
#08/17/23: reclassify sediment size 
P195_bind_edt <- P195_bind_edt %>% mutate(Sedsize_new= ifelse(P195_bind_edt$Sedsize_new == 1|P195_bind_edt$Sedsize_new == 6|P195_bind_edt$Sedsize_new == 8, "Sand", ifelse(P195_bind_edt$Sedsize_new== 2|P195_bind_edt$Sedsize_new== 3|P195_bind_edt$Sedsize_new== 4|P195_bind_edt$Sedsize_new== 5| P195_bind_edt$Sedsize_new== 7| P195_bind_edt$Sedsize_new== 9, "Mud", ifelse(P195_bind_edt$Sedsize_new== 0, "Hard bottom", P195_bind_edt$Sedsize_new))))

P195_bind_edt<- P195_bind_edt %>% mutate(Btmcomp_new = ifelse(P195_bind_edt$Btmcomp %in% "Grass", "B", ifelse(P195_bind_edt$Btmcomp %in% "No Grass", "O", ifelse(P195_bind_edt$Btmcomp %in% "Byozoan", "Q", ifelse(P195_bind_edt$Btmcomp %in% "Grass Algae", "H", ifelse(P195_bind_edt$Btmcomp %in% "Tunicate", "P",
ifelse(P195_bind_edt$Btmcomp %in% "Algae", "C", ifelse(P195_bind_edt$Btmcomp %in% "Shellgrass Algae", "I", ifelse(P195_bind_edt$Btmcomp %in% "Shell Detritus", "M", ifelse(P195_bind_edt$Btmcomp %in% "Shell", "A", ifelse(P195_bind_edt$Btmcomp %in% "Shell Grass", "G", ifelse(P195_bind_edt$Btmcomp %in% "Detritus", "C",
ifelse(P195_bind_edt$Btmcomp %in% "Rock Shell", "S", NA)))))))))))))
P195_bind_edt <- P195_bind_edt %>% mutate(Btmcomp_new = ifelse(P195_bind_edt$Btmcomp %in% "A"|P195_bind_edt$Btmcomp %in% "Q"|P195_bind_edt$Btmcomp %in% "S", "Shell", ifelse(P195_bind_edt$Btmcomp %in% "L"|P195_bind_edt$Btmcomp %in% "H"|P195_bind_edt$Btmcomp %in% "C"|P195_bind_edt$Btmcomp %in% "B"|P195_bind_edt$Btmcomp %in% "K"|P195_bind_edt$Btmcomp %in% "D", "Vegetated", ifelse(P195_bind_edt$Btmcomp %in% "I"|P195_bind_edt$Btmcomp %in% "G"|P195_bind_edt$Btmcomp %in% "M", "Vegetated-Shell", ifelse(P195_bind_edt$Btmcomp %in% "P"|P195_bind_edt$Btmcomp %in% "O"|P195_bind_edt$Btmcomp %in% "E", "Unstructured", ifelse(P195_bind_edt$Btmcomp %in% "Z", "Unnatural", ifelse(P195_bind_edt$Btmcomp %in% "X", NA, P195_bind_edt$Btmcomp)))))))

#Add in Secchi depth 
secchi_depth <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/P195/Raw/P195_secchi_Dowd_20230815.csv")
secchi_depth$Date <- as.Date(secchi_depth$Date, format= "%m/%d/%Y")
secchi_depth <- secchi_depth %>% select(Date, Grid, Secchi.Depth..cm.) %>% rename("Secchi_depthcm"= "Secchi.Depth..cm.", "Stationcode"= "Grid")
P195_bind_edt <- P195_bind_edt %>% left_join(secchi_depth, by= c("Date", "Stationcode"))
write.csv(P195_bind_edt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P195/Finalized/p195_abund.csv")

#P195: Length frequency
setwd("~/Documents/GitHub/NCBlueCrab_Predators")
p195_lengthfreq <- read_csv("Data/P195/Raw/p195.lengthfreq.2023-07-25.csv")
#length(unique(P195_event$EVENTNAME)) 
length(unique(p195_lengthfreq$EVENTNAME)) #diff number of eventnames, makes sense because this is just when individuals were measured 

merged_apply <- as.data.frame(sapply(p195_lengthfreq[,c(1:2, 4:14, 18:24, 31, 33)], function(x) gsub('[^[:alnum:] ]', "", x))) #remove all special characters
p195_lengthfreq[ , colnames(p195_lengthfreq) %in% colnames(merged_apply)] <- merged_apply #replace updated columns in original dataset
colnames(p195_lengthfreq) <- str_to_title(colnames(p195_lengthfreq))
p195_lengthfreq$Date <- as.Date(p195_lengthfreq$Date, "%m/%d/%y")
p195_lengthfreq$Ym_date <- format(p195_lengthfreq$Date, "%Y-%m")
p195_lengthfreq$Year <- year(p195_lengthfreq$Date)
p195_lengthfreq$Month <- month(p195_lengthfreq$Date)
p195_lengthfreq$Day <- day(p195_lengthfreq$Date)
p195_lengthfreq$Season <- ifelse(p195_lengthfreq$Month==4 | p195_lengthfreq$Month==5 | p195_lengthfreq$Month==6, "Spring", ifelse(p195_lengthfreq$Month==9 |p195_lengthfreq$Month==10 | p195_lengthfreq$Month==11 | p195_lengthfreq$Month==12, "Fall", ifelse(p195_lengthfreq$Month==7 |p195_lengthfreq$Month==8, "Summer", "Winter")))
p195_lengthfreq <- p195_lengthfreq %>% filter(Month == 6| Month==9)

#Adding in Sciname for species
p195_lengthfreq$Speciescommonname <- str_to_lower(p195_lengthfreq$Speciescommonname)
p195_lengthfreq$Speciesscientificname <- str_to_lower(p195_lengthfreq$Speciesscientificname)
species_P195 <- read_csv("DATA/species.P195.csv")
colnames(species_P195) <- str_to_title(colnames(species_P195))
species_P195$Speciescommonname <- str_to_lower(species_P195$Speciescommonname)
species_P195$Speciesscientificname <- str_to_lower(species_P195$Speciesscientificname)
species_P195_edt <- as.data.frame(species_P195[, c(1,9)])

p195_lengthfreq_edt <- p195_lengthfreq %>% left_join(species_P195_edt, by= "Speciesscientificname") %>% rename("Sciname"= "Species")
p195_lengthfreq_edt$Speciescommonname[p195_lengthfreq_edt$Speciescommonname== "northern brown shrimp"] <- "brown shrimp"
summary(is.na(p195_lengthfreq_edt)) #no NAs in Sciname, makes sense 

write.csv(p195_lengthfreq_edt, "Data/P195/Finalized/p195_lengthfreq.csv")

#####P135#####

###Habitat data## 
setwd("/Users/sallydowd/Desktop/Ch1Data/P135")
habitat <- read_excel("HabitatDataP135_1990-May2023.xlsx", col_types= "text") #read this in as text or columns mess up
colnames(habitat) <- str_to_title(colnames(habitat))
habitat <- habitat %>% mutate(Year = case_when(as.numeric(Year) >= 90 & as.numeric(Year) <= 99 ~ paste0("19", Year),
    as.numeric(Year) >= 0 & as.numeric(Year) <= 9 ~ paste0("200", Year),
    as.numeric(Year) >= 10 & as.numeric(Year) <= 23 ~ paste0("20", Year))) #add #s to year
habitat$Season <- ifelse(habitat$Month==4 | habitat$Month==5 | habitat$Month==6, "Spring", ifelse(habitat$Month==9 |habitat$Month==10 | habitat$Month==11 | habitat$Month==12, "Fall", ifelse(habitat$Month==7 |habitat$Month==8, "Summer", "Winter")))
habitat$Date <- as.Date(paste(habitat$Month, habitat$Day, habitat$Year, sep= "-"), "%m-%d-%Y")
habitat$Ym_date <- format(habitat$Date, "%Y-%m")

#identify column names with dot
columns_with_dot <- colnames(habitat)[sapply(habitat, function(x) any(grepl("\\.", x)))]
print(columns_with_dot) #Depth (16), Atemp (17), Btemp (19), Depth is plausible (e.g: 15.5 means there's a dot)

##Remove implausible dots 
columns <- c(17, 19)
for (col in columns) {
  habitat[[col]][is.na(habitat[[col]]) | habitat[[col]] == "."] <- NA
} #some values only have a dot or maybe it is an NA that R generated, replace these with NA instead

habitat <- habitat %>% mutate_at(vars(c(3:5, 16:21, 23:24)), as.numeric)

#Re-classify sediment size as sand or mud 
habitat <- habitat %>% mutate(Sedsize_new= ifelse(habitat$Sedsize == 1|habitat$Sedsize == 6|habitat$Sedsize == 8, "Sand", ifelse(habitat$Sedsize== 2|habitat$Sedsize== 3|habitat$Sedsize== 4|habitat$Sedsize== 5| habitat$Sedsize== 7| habitat$Sedsize== 9, "Mud", ifelse(habitat$Sedsize== 0, "Hard bottom", habitat$Sedsize))))

#Lat/lon
habitat$QuadGrid <- paste(habitat$Quad, habitat$Grid, sep= "-")
P135_lat_lons <- read_excel("~/Documents/GitHub/NCBlueCrab_Predators/Data/P135/P135_lat_lons.xlsx")
lat_lons <- P135_lat_lons %>% rename("Quad"= "Zone") %>% mutate(QuadGrid = paste(.$Quad, .$Grid, sep= "-")) %>% dplyr::select(-Quad, -Grid)
habitat <- habitat %>% left_join(lat_lons, by= "QuadGrid")

# write_xlsx(habitat, "/Users/sallydowd/Desktop/Ch1Data/P135/p135_habitat_edt.xlsx") #02/06/23

##Biological data## 
setwd("/Users/sallydowd/Desktop/Ch1Data/P135")
biol <- read_excel("~/Desktop/Ch1Data/P135/BiologicalDataP135forN100_Final.xlsx", col_types= "text")
colnames(biol) <- str_to_title(colnames(biol))
biol <- biol %>% mutate(Year = case_when(as.numeric(Year) >= 90 & as.numeric(Year) <= 99 ~ paste0("19", Year),
                                               as.numeric(Year) >= 0 & as.numeric(Year) <= 9 ~ paste0("200", Year),
                                               as.numeric(Year) >= 10 & as.numeric(Year) <= 23 ~ paste0("20", Year))) #add #s to year
biol$Season <- ifelse(biol$Month==4 | biol$Month==5 | biol$Month==6, "Spring", ifelse(biol$Month==9 |biol$Month==10 | biol$Month==11 | biol$Month==12, "Fall", ifelse(biol$Month==7 |biol$Month==8, "Summer", "Winter")))
biol$Date <- as.Date(paste(biol$Month, biol$Day, biol$Year, sep= "-"), "%m-%d-%Y")
biol$Ym_date <- format(biol$Date, "%Y-%m")

#identify column names with dot
columns_with_dot <- colnames(biol)[sapply(biol, function(x) any(grepl("\\.", x)))]
print(columns_with_dot) #Weight, this is plausible 

#deal with species names 
P120_speciesnms <- read_csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/Spp_names/P120_speciesnms.csv")
colnames(P120_speciesnms) <- str_to_title(colnames(P120_speciesnms))
P120_speciesnms$Speciescommonname <- str_to_lower(P120_speciesnms$Speciescommonname)
P120_speciesnms_edt <- P120_speciesnms %>% dplyr::select(Speciescode, Speciescommonname) %>% mutate_at('Speciescode', as.character)
P120_speciesnms_edt$Speciescode <- trimws(P120_speciesnms_edt$Speciescode)

#identify which ones don't have species codes from P120, manually edit P120 dataset in excel and save as new, species code in bdbcode in Datasets google drive
biol <- biol %>% rename("Speciescode"= "Species")
length(unique(biol$Speciescode))
biol_test <- biol %>% left_join(P120_speciesnms_edt, by= "Speciescode")
ugh <- biol_test %>% filter(Speciescommonname %in% NA)
length(unique(ugh$Speciescode))

#now actually join spp names 
ugh2 <- read.csv("~/Documents/GitHub/NCBlueCrab_Predators/Data/Spp_names/P120_P135_sppnames.csv")
colnames(ugh2) <- str_to_title(colnames(ugh2))
ugh2$Speciescommonname <- str_to_lower(ugh2$Speciescommonname)
P120_P135_sppnames <- ugh2 %>% mutate_at('Speciescode', as.character)
biol_edt <- biol %>% left_join(P120_P135_sppnames, by= "Speciescode")
# write_xlsx(biol_edt, "/Users/sallydowd/Desktop/Ch1Data/P135/biol_edt.xlsx") #02/06/23

#####P100#####
setwd("/Users/sallydowd/Desktop/Ch1Data/P100/Data")
filenames <- list.files("/Users/sallydowd/Desktop/Ch1Data/P100/Data")
all <- lapply(filenames, readr::read_csv)
merged <- do.call(rbind, all)

colnames(merged) <- str_to_title(colnames(merged))
merged$Season <- ifelse(merged$Month==4 | merged$Month==5 | merged$Month==6, "Spring", ifelse(merged$Month==9 |merged$Month==10 | merged$Month==11 | merged$Month==12, "Fall", ifelse(merged$Month==7 |merged$Month==8, "Summer", "Winter")))
merged$Date <- as.Date(paste(merged$Month, merged$Day, merged$Year, sep= "-"), "%m-%d-%Y")
merged$Ym_date <- format(merged$Date, "%Y-%m")

#identify column names with dot
columns_with_dot <- colnames(merged)[sapply(merged, function(x) any(grepl("\\.", x)))]
print(columns_with_dot) #all check out 

#deal with species names 
merged$Species_common <- str_to_lower(merged$Species_common)
unique(merged$Species_common)
switch_order <- function(text) {
  words <- unlist(strsplit(text, ", "))
  switched_text <- paste(rev(words), collapse = " ")
  return(switched_text)
}
merged$Species_common <- sapply(merged$Species_common, switch_order)
unique(merged$Species_common)

merged <- merged %>% filter(!Species_common %in% "8835022102") %>% rename("Speciescommonname"= Species_common)
#removes 1 row 

#Fix location issues - not working!! 
P100_locations <- read_excel("~/Documents/GitHub/NCBlueCrab_Predators/Data/P100/P100_locations.xlsx")
colnames(P100_locations) <- str_to_title(colnames(P100_locations))
P100_locations <- P100_locations %>% dplyr::select(Station__, Location, Lat_dd, Long_dd, Loc_sta) %>% rename(Latitude= Lat_dd, Longitude= Long_dd, Station= Station__)
P100_locations$Location <- as.numeric(P100_locations$Location)
merged$Loc_sta <- paste(merged$Location, merged$Station, sep= "")
# merged$LocStat <- paste(merged$Location, merged$Station, sep="")
merged_edt <- merged %>% left_join(P100_locations, by= "Loc_sta") %>% filter(Gear1 %in% 535)

unique(P100_locations$LocStat)
P100_locations %>% filter(Location %in% 208000500)
d <- merged_edt %>% filter(Latitude %in% NA) %>% dplyr::select(Latitude, Location, Station) 
unique(d$LocStat)
# write_xlsx(merged_edt, "/Users/sallydowd/Desktop/Ch1Data/P100/p100_clean.xlsx") #02/06/23

