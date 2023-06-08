Toclean.R
#Standard flow: remove special characters and replace with NA, make sure there is a month, day, year and date column, str_to_title, no all caps for species, bind with common name dataset
#Load packages
library(readxl)
library(stringr)

#P915:

##Creating old CPUE file (p195clean.csv)
setwd("/Users/sallydowd/Desktop/pamlicodatasets/p915/Data/Individual")
filenames <- list.files("/Users/sallydowd/Desktop/pamlicodatasets/p915/Data/Individual", pattern= '*.xlsx')  
all <- lapply(filenames, readxl::read_excel)
merged <- do.call(rbind, all)

colnames(merged) <- str_to_title(colnames(merged))
merged$Species <- tolower(merged$Species)

##New biological data files 
p915_biol1 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new1.xlsx")
colnames(p915_biol1) <- str_to_title(colnames(p915_biol1))
p915_biol1$Location <- str_to_title(p915_biol1$Location)
p915_biol1 <- p915_biol1 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol1_apply <- as.data.frame(sapply(p915_biol1[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol1_apply[p915_biol1_apply== "ERROR"] <- NA
p915_biol1[ , colnames(p915_biol1) %in% colnames(p915_biol1_apply)] <- p915_biol1_apply #replace updated columns in original dataset
p915_biol1$Season <- ifelse(p915_biol1$Month==4 | p915_biol1$Month==5 | p915_biol1$Month==6, "Spring", ifelse(p915_biol1$Month==9 |p915_biol1$Month==10 | p915_biol1$Month==11 | p915_biol1$Month==12, "Fall", ifelse(p915_biol1$Month==7 |p915_biol1$Month==8, "Summer", "Winter")))
p915_biol1$Ym_date <- format(p915_biol1$Date, "%Y-%m")
write.csv(p915_biol1, "Data/P915/Finalized/p915_biol1new.csv")

p915_biol2 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new2.xlsx")
colnames(p915_biol2) <- str_to_title(colnames(p915_biol2))
p915_biol2$Location <- str_to_title(p915_biol2$Location)
p915_biol2 <- p915_biol2 %>% select(-Year) %>% mutate(Year= lubridate::year(Date))
p915_biol2_apply <- as.data.frame(sapply(p915_biol2[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_biol2_apply[p915_biol2_apply== "ERROR"] <- NA
p915_biol2[ , colnames(p915_biol2) %in% colnames(p915_biol2_apply)] <- p915_biol2_apply #replace updated columns in original dataset
p915_biol2$Season <- ifelse(p915_biol2$Month==4 | p915_biol2$Month==5 | p915_biol2$Month==6, "Spring", ifelse(p915_biol2$Month==9 |p915_biol2$Month==10 | p915_biol2$Month==11 | p915_biol2$Month==12, "Fall", ifelse(p915_biol2$Month==7 |p915_biol2$Month==8, "Summer", "Winter")))
p915_biol2$Ym_date <- format(p915_biol2$Date, "%Y-%m")

write.csv(p915_biol2, "Data/P915/Finalized/p915_biol2new.csv")

#Date, year and month columns 
trawl_edt$Date <- as.Date(as.character(trawl_edt$Date), format= '%Y%m%d')
trawl_edt$Year <- format(as.POSIXct(trawl_edt$Date,format= '%Y%m%d'), format= '%Y', tz= "GMT")
trawl_edt$Month <- format(as.POSIXct(trawl_edt$Date,format= '%Y%m%d'), format= '%m', tz= "GMT")

#P120: Look at Lela's CPUE file 
df=read.delim("P120_1019.txt",sep="$",header=TRUE,dec=".")
head(df)

df2=read.delim("P120_7279.txt",sep="$",header=TRUE,dec=".")
head(df2)

df3=read.delim("P120_8089.txt",sep="$",header=TRUE,dec=".")
head(df3)

df4=read.delim("P120_9099.txt",sep="$",header=TRUE,dec=".")
head(df4)

df5=read.delim("P120_0009.txt",sep="$",header=TRUE,dec=".")
head(df5)

fulld=rbind(df,df2,df3,df4,df5)
head(fulld)

#Add this to all datasets! 

#p915_CPUEold <- p915_CPUEold %>% filter(Year < 2020), not this yet 

