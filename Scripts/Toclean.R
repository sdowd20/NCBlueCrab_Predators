Toclean.R
#Standard flow: remove special characters and replace with NA, make sure there is a month, day, year and date column, str_to_title, no all caps for species, bind with common name dataset, make sure common names are the same as species
#Load packages
library(readxl)
library(stringr)
library(readr)
library(dplyr)
library(lubridate)

#Species names
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
sppcommonnmsc[20,2] <- "scallop hammerhead"
sppcommonnmsc[3,2] <- "striped bass" #northern kingfish doesn't exist in dataset, has same abbreviated name 
sppcommonnmsc <- sppcommonnmsc %>% filter(!Speciescommonname %in% "sand devil") #take out angel shark right now b/c don't have CPUE
write.csv(sppcommonnmsc, "Data/sppcommonnmsc.csv")
sppcommonnmsc[20,1] <- "S. dumeril"
sppcommonnmsc[20,2] <- "angel shark"

spp_list <- read.csv("Data/sppcommonnmsc.csv")
spp_list <- as.data.frame(spp_list[,-1])

#P915

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

##Create new CPUE file 
setwd("/Users/sallydowd/Desktop/CPUE")
filenames <- list.files("/Users/sallydowd/Desktop/CPUE", pattern= '*.xlsx')  
all <- lapply(filenames, readxl::read_excel)
merged <- do.call(rbind, all)
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
write.csv(P915_CPUE, "Data/P915/Finalized/p915_CPUE.csv")


##New biological data files 
###General

#p915_biol1 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new1.xlsx")
#write.csv(p915_biol1, "Data/P915/Raw/p915_biol1new.csv")

p915_biol1 <- read_csv("Data/P915/Raw/p915_biol1new.csv")
colnames(p915_biol1) <- str_to_title(colnames(p915_biol1))
p915_biol1$Location <- str_to_title(p915_biol1$Location)
p915_biol1 <- p915_biol1 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol1_apply <- as.data.frame(sapply(p915_biol1[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol1_apply[p915_biol1_apply== "ERROR"] <- NA
p915_biol1[ , colnames(p915_biol1) %in% colnames(p915_biol1_apply)] <- p915_biol1_apply #replace updated columns in original dataset
p915_biol1$Season <- ifelse(p915_biol1$Month==4 | p915_biol1$Month==5 | p915_biol1$Month==6, "Spring", ifelse(p915_biol1$Month==9 |p915_biol1$Month==10 | p915_biol1$Month==11 | p915_biol1$Month==12, "Fall", ifelse(p915_biol1$Month==7 |p915_biol1$Month==8, "Summer", "Winter")))
p915_biol1$Ym_date <- format(p915_biol1$Date, "%Y-%m")

p915_biol2 <- read.csv("Data/P915/Raw/p915_biol2new.csv")
colnames(p915_biol2) <- str_to_title(colnames(p915_biol2))
p915_biol2$Location <- str_to_title(p915_biol2$Location)
p915_biol2 <- p915_biol2 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol2_apply <- as.data.frame(sapply(p915_biol2[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol2_apply[p915_biol2_apply== "ERROR"] <- NA
p915_biol2[ , colnames(p915_biol2) %in% colnames(p915_biol2_apply)] <- p915_biol2_apply #replace updated columns in original dataset
p915_biol2$Season <- ifelse(p915_biol2$Month==4 | p915_biol2$Month==5 | p915_biol2$Month==6, "Spring", ifelse(p915_biol2$Month==9 |p915_biol2$Month==10 | p915_biol2$Month==11 | p915_biol2$Month==12, "Fall", ifelse(p915_biol2$Month==7 |p915_biol2$Month==8, "Summer", "Winter")))
p915_biol2$Date <- as.Date(p915_biol2$Date, "%Y-%m-%d")
p915_biol2$Ym_date <- format(p915_biol2$Date, "%Y-%m")

unique(p915_biol2$Species)
unique(p915$Speciescommonname)
###Adding species commonnames
p915_biol1 <- p915_biol1 %>% dplyr::rename("Sciname"= "Species")
p915_biol2 <- p915_biol2 %>% dplyr::rename("Sciname"= "Species")
p915_biol1_new <- p915_biol1 %>% left_join(spp_list, by= "Sciname")
p915_biol2_new <- p915_biol2 %>% left_join(spp_list, by= "Sciname") #no NAs for Sciname for 1st and 2nd df, a couple of NAs for Speciescommonname in 1st and 2nd b/c of angel shark

write.csv(p915_biol1_new, "Data/P915/Finalized/p915_biol1new_clean.csv")
write.csv(p915_biol2_new, "Data/P915/Finalized/p915_biol2new_clean.csv")

#P120 old
p120 <- read_csv("Data/P120/Raw/p120_clean_2021.csv")
colnames(p120) <- str_to_title(colnames(p120))
p120 <- p120 %>% dplyr::rename("Sciname" = "Species")
P120_speciesnms_edt2 <- P120_speciesnms %>% distinct(Sciname, .keep_all= TRUE)
p120_edt <- p120 %>% dplyr::left_join(P120_speciesnms_edt2, by= "Sciname")
p120_edt$Speciescommonname[p120_edt$Speciescommonname %in% "northern brown shrimp"] <- "brown shrimp" #check which ones didn't match spp_list in P120 to change

write.csv(p120_edt, "Data/P120/Finalized/p120_clean_2021new.csv")

#P195 old 
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

##Edit old CPUE file to include species scientific name
p915 <- read.csv("Data/P915/Raw/p915clean.csv")
p915 <- p915 %>% dplyr::rename("Speciescommonname"= "Species")
p915$Date <- as.Date(paste(p915$Month, p915$Day, p915$Year, sep= "-"), "%m-%d-%Y")
p915_spp <- p915 %>% left_join(spp_list, by= "Speciescommonname") #Same amount of NAs as P915 normally for Speciescommonname
write.csv(p915_spp, "Data/P915/Finalized/p915clean_new.csv")

#P120
##Catch data (no biological)
p120_speciesn <- p120_speciesn %>% rename("Species" = "SciName") %>% select(Species, SPECIESCOMMONNAME)
p120_edt <- p120 %>% left_join(p120_speciesn, by= "Species")
colnames(p120_edt) <- str_to_title(colnames(p120_edt))
p120_edt[[39]] <- tolower(p120_edt[[39]])


##Biological data: old, through 2019 
##Haven't finished this 
setwd("~/Desktop")
df=read.delim("P120_1019.txt",sep="$",header=TRUE,dec=".")
df2=read.delim("P120_7279.txt",sep="$",header=TRUE,dec=".")
df3=read.delim("P120_8089.txt",sep="$",header=TRUE,dec=".")
df4=read.delim("P120_9099.txt",sep="$",header=TRUE,dec=".")
df5=read.delim("P120_0009.txt",sep="$",header=TRUE,dec=".")
fulld=rbind(df,df2,df3,df4,df5)
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

write.csv(species_namesedt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P120/Finalized/p120_speciesnms_new.csv")

write.csv(fulld2_edt, "~/Documents/GitHub/NCBlueCrab_Predators/Data/P120/Finalized/p120_biol_new.csv")
