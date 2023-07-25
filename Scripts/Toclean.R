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

inlet_coords <- st_read("~/Desktop/Inlet.points.kml")
coords <- st_coordinates(inlet_coords)
inlet_coords$Latitude <- coords[, "Y"]
inlet_coords$Longitude <- coords[, "X"]
inlet_coords <- inlet_coords %>% select(-Description)

ggplot(data= world) + geom_sf() + geom_point(data= inlet_coords, aes(x= Longitude, y= Latitude, color= Name)) + coord_sf(xlim=c(-78, -75), ylim=c(33,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + labs(x= "Longitude", y= "Latitude") + theme(legend.position="none")

#######SPEICES NAMES########
setwd("~/Documents/GitHub/NCBlueCrab_Predators")
P120_speciesnms <- read_csv("Data/P120/P120_speciesnms.csv")
colnames(P120_speciesnms) <- str_to_title(colnames(P120_speciesnms))
P120_speciesnms$Speciescommonname <- str_to_lower(P120_speciesnms$Speciescommonname)
P120_speciesnms$Speciesscientificname <- str_to_lower(P120_speciesnms$Speciesscientificname)
P120_speciesnms_edt <- P120_speciesnms %>% select(Speciescommonname, Sciname)

p915_biol1 <- read.csv("Data/P915/Finalized/p915_biol1new_clean.csv")
sci_spp <- as.data.frame(unique(p915_biol1$Sciname))
colnames(sci_spp)[1] <- "Sciname"
sppcommonnmsc <- sci_spp %>% left_join(P120_speciesnms_edt, by= "Sciname")
sppcommonnmsc[8,2] <- "bonnethead hammerhead"
sppcommonnmsc[10,2] <- "brown shrimp"
sppcommonnmsc[13,2] <- "eastern oyster"
sppcommonnmsc[14,2] <- "sandbar shark"
sppcommonnmsc[17,2] <- "blue catfish"
sppcommonnmsc[18,2] <- "angel shark"
sppcommonnmsc[20,2] <- "scallop hammerhead"
sppcommonnmsc[3,2] <- "striped bass" #northern kingfish doesn't exist in dataset, has same abbreviated name 

write.csv(sppcommonnmsc, "Data/sppcommonnmsc.csv")

spp_list <- read.csv("Data/sppcommonnmsc.csv")
spp_list <- as.data.frame(spp_list[,-1])

#######P915########
#P915 OLD
##Creating old CPUE file (p195clean.csv)
setwd("/Users/sallydowd/Desktop/pamlicodatasets/p915/Data/Individual")
filenames <- list.files("/Users/sallydowd/Desktop/pamlicodatasets/p915/Data/Individual", pattern= '*.xlsx')  
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

#P915 NEW
setwd("/Users/sallydowd/Desktop/CPUE")
filenames <- list.files("/Users/sallydowd/Desktop/CPUE", pattern= '*.xlsx')  
filenames <- filenames[-c(1,16)]
all <- lapply(filenames, readxl::read_excel)
merged <- do.call(rbind, all)
angelshark <- read_excel("/Users/sallydowd/Desktop/CPUE/Angelshark_CPUEupdates.XLS.xlsx", sheet= 2)
oystertoadfish <- read_excel("/Users/sallydowd/Desktop/CPUE/oystertoadfish_CPUEupdates.XLS.xlsx", sheet= 2)
merged <- rbind(merged, angelshark, oystertoadfish)
colnames(merged) <- str_to_title(colnames(merged))
merged$Season <- ifelse(merged$Month==4 | merged$Month==5 | merged$Month==6, "Spring", ifelse(merged$Month==9 |merged$Month==10 | merged$Month==11 | merged$Month==12, "Fall", ifelse(merged$Month==7 |merged$Month==8, "Summer", "Winter")))
merged$Date <- as.Date(paste(merged$Month, merged$Day, merged$Year, sep= "-"), "%m-%d-%Y")
merged$Ym_date <- format(merged$Date, "%Y-%m")
merged_apply <- as.data.frame(sapply(merged[,c(18:19)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
merged_apply[merged_apply== "ERROR"] <- NA
merged[ , colnames(merged) %in% colnames(merged_apply)] <- merged_apply #replace updated columns in original dataset

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
#distance to inlet:

P915_CPUE_edt <- P915_CPUE %>% drop_na(Latitude, Longitude) 
P915_CPUE_sf <- st_as_sf(P915_CPUE_edt, coords = c("Longitude", "Latitude"), crs= 4326) 

coords_inlet_nearest <- st_nearest_feature(P915_CPUE_sf, inlet_coords)
dist_inlets = st_distance(P915_CPUE_sf, inlet_coords[coords_inlet_nearest,], by_element=TRUE)
dist_inlets <- as.numeric(dist_inlets)

ggplot(data= world) + geom_sf() + geom_point(data= P915_CPUE_edt, aes(x= Longitude, y= Latitude, color= inlets_dist)) + geom_point(data= inlet_coords, aes(x= Longitude, y= Latitude)) + coord_sf(xlim=c(-78, -75), ylim=c(33,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + labs(x= "Longitude", y= "Latitude") + scale_color_viridis()

library(viridis)
geom_segment(
  data = distance_df,
  aes(x = your_data[distance_df$from, "longitude_column"],
      y = your_data[distance_df$from, "latitude_column"],
      xend = your_data[distance_df$to, "longitude_column"],
      yend = your_data[distance_df$to, "latitude_column"]),
  color = "blue"  # You can customize the line color here
) +

library(tidyr)

coords_shore_nearest <- st_nearest_feature(lat_longs_sf, shape) #find nearest coastline
dist_shore = st_distance(lat_longs_sf, shape[coords_shore_nearest,], by_element = TRUE) #find shortest distance to nearest coastline 
dist_shore <- as.numeric(dist_shore)
lat_longs_filled <- lat_longs_filled %>% mutate(shore_dist_m = dist_shore)
ggmap(myMap) + geom_point(lat_longs_filled, mapping = aes(x = lon, y = lat, color = shore_dist_m)) + xlab("Longitude") + ylab("Latitude") + labs(color = "Distance to shore (m)") + theme_Publication() +  theme(legend.position = "right", legend.direction = "vertical")







dist_inlets <- as.numeric(dist_inlets)
P915_CPUE_edt <- P915_CPUE_edt %>% mutate(inlet_dist_m = dist_inlets) 
#P915_CPUE_edt$port_dist_m <- as.numeric(as.character(P915_CPUE_edt$port_dist_m))

ggmap(myMap) + geom_point(P915_CPUE_edt, mapping = aes(x = Longitude, y = Latitude, fill = inlets_dist)) + geom_point(inlet_coords, mapping= aes(x= Longitude, y= Latitude), col= "orange") + standard_theme + xlab("Longitude") + ylab("Latitude") + labs(color = "Distance to inlet (m)") + theme(legend.position = "right", legend.direction = "vertical")  


ggplot(data = world) + geom_sf() + geom_point(P915_CPUE_edt, mapping = aes(x = Longitude, y = Latitude, fill = inlet_dist_m)) + geom_point(inlet_coords, mapping= aes(x= Longitude, y= Latitude), col= "orange") + standard_theme + xlab("Longitude") + ylab("Latitude") + labs(color = "Distance to inlet (m)") + theme(legend.position = "right", legend.direction = "vertical")  + coord_sf(xlim=c(-78, -75), ylim=c(34,37), expand = TRUE) + theme(panel.background = element_rect(fill = "white", colour = "black")) + labs(x= "Longitude", y= "Latitude")

ggmap(myMap) +
  geom_point(data = P915_CPUE_edt, mapping = aes(x = Longitude, y = Latitude, fill = inlet_dist_m)) +
  geom_point(inlet_coords, mapping = aes(x = Longitude, y = Latitude), col = "orange") +
  standard_theme +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(color = "Distance to inlet (m)") +
  theme(legend.position = "right", legend.direction = "vertical") +
  scale_fill_gradient(low = "blue", high = "red")

str(P915_CPUE_edt$inlet_dist_m)

unique(P915_CPUE_edt$inlet_dist_m)



write.csv(P915_CPUE_edt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P915/Finalized/p915_CPUE.csv")

  
  
  
  #Calculate distance to nearest port for lat_longs_filled dataset
  ##Load and manipulate datasets 
  ports_US <- ports_global %>% filter(Country.Code %in% "United States", World.Water.Body %in% "North Atlantic Ocean") #select US East coast 
lat_longs_sf <- st_as_sf(lat_longs_filled, coords = c("lon", "lat"), crs= 4326) #make into spatial dataset, standard CRS
ports_US_sf <- st_as_sf(ports_US, coords = c("Longitude", "Latitude"), crs= 4326)

##Find nearest distance and add it as a column to lat_longs_sf 
coords_ports_nearest <- st_nearest_feature(lat_longs_sf, ports_US_sf) #features in x of class sfc compared to y of class sfc, output is row number of nearest feature in b to each feature in a 
dist_ports = st_distance(lat_longs_sf, ports_US_sf[coords_ports_nearest,], by_element=TRUE) #compute distance b/w geometry pairs,
dist_ports <- as.numeric(dist_ports)
lat_longs_filled <- lat_longs_filled %>% mutate(port_dist_m = dist_ports) 
ggmap(myMap) + geom_point(lat_longs_filled, mapping = aes(x = lon, y = lat, color = port_dist_m)) + geom_point(ports_US, mapping= aes(x= Longitude, y= Latitude), col= "orange") + theme_Publication() + xlab("Longitude") + ylab("Latitude") + labs(color = "Distance to port (m)") + theme(legend.position = "right", legend.direction = "vertical")  
  
  
  
  
  
  
  



  
  
##P915 NEW: Biological data 

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
setwd("~/Desktop")
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

##Add in other predictor variables 
fulld2_edt$doy <- yday(fulld2_edt$Date)
fulld2_edt$Photoperiod <- daylength(lat= fulld2_edt$Latitude, doy= fulld2_edt$doy)

write.csv(species_namesedt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P120/Finalized/p120_speciesnms_new.csv")
write.csv(fulld2_edt, "~/Desktop/p120_biol_new.csv")

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

merged_apply <- as.data.frame(sapply(P195_bind[,c(1:2, 4:17, 23, 25, 26:28, 29:30, 31, 43, 47)], function(x) gsub('[^[:alnum:] ]', "", x))) #remove all special characters
P195_bind[ , colnames(P195_bind) %in% colnames(merged_apply)] <- merged_apply #replace updated columns in original dataset
colnames(P195_bind) <- str_to_title(colnames(P195_bind))
P195_bind$Date <- as.Date(P195_bind$Date, "%m-%d-%Y")
P195_bind$Ym_date <- format(P195_bind$Date, "%Y-%m")
P195_bind$Year <- year(P195_bind$Date)
P195_bind$Month <- month(P195_bind$Date)
P195_bind$Day <- day(P195_bind$Date)

##Add in other predictor variables
P195_bind$doy <- yday(P195_bind$Date)
P195_bind$Photoperiod <- daylength(lat= P195_bind$Latitudestart, doy= P195_bind$doy)
P195_bind$Wbdytype <- ifelse(P195_bind$Location %in% "NEUSE RIVER" | P195_bind$Location %in% "PAMLICO RIVER" | P195_bind$Location %in% "PUNGO RIVER", "River", "Sound")
P195_bind$Wbd <- ifelse(P195_bind$Location %in% "PAMLICO SOUND WEST OF BLUFF SHOAL" | P195_bind$Location %in% "PAMLICO SOUND EAST OF BLUFF SHOAL", "PAMLICO SOUND", P195_bind$Location)

#Adding in Sciname for species
P195_bind$Speciescommonname <- str_to_lower(P195_bind$Speciescommonname)
P195_bind$Speciesscientificname <- str_to_lower(P195_bind$Speciesscientificname)
species_P195 <- read_csv("species.P195.csv")
colnames(species_P195) <- str_to_title(colnames(species_P195))
species_P195$Speciescommonname <- str_to_lower(species_P195$Speciescommonname)
species_P195$Speciesscientificname <- str_to_lower(species_P195$Speciesscientificname)
species_P195_edt <- as.data.frame(species_P195[, c(1,9)])

P195_bind_edt <- P195_bind %>% left_join(species_P195_edt, by= "Speciesscientificname") %>% rename("Sciname"= "Species")
P195_bind_edt$Speciescommonname[P195_bind_edt$Speciescommonname== "northern brown shrimp"] <- "brown shrimp"

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

