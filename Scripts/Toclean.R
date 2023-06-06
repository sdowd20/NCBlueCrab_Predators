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

##Creating new biological data file 
p915_biol1 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new1.xlsx")
p915_biol2 <-read_xlsx("/users/sallydowd/Desktop/P915_biological_new2.xlsx")
p915_bioln <- rbind(p915_biol1, p915_biol2)
colnames(p915_bioln) <- str_to_title(colnames(p915_bioln))
p915_bioln$Location <- str_to_title(p915_bioln$Location)
p915_bioln_apply <- as.data.frame(sapply(p915_bioln[,c(18,19, 31:33)], function(x) gsub('[^[:alnum:] ]', "", x))) #select only columns with ***ERROR*** to modify 
p915_bioln_apply[p915_bioln_apply== "ERROR"] <- NA
p915_bioln[ , colnames(p915_bioln) %in% colnames(p915_bioln_apply)] <- p915_bioln_apply #replace updated columns in original dataset
write.csv("Data/P915/Finalized/p915_biolnew")

#Date, year and month columns 
trawl_edt$Date <- as.Date(as.character(trawl_edt$Date), format= '%Y%m%d')
trawl_edt$Year <- format(as.POSIXct(trawl_edt$Date,format= '%Y%m%d'), format= '%Y', tz= "GMT")
trawl_edt$Month <- format(as.POSIXct(trawl_edt$Date,format= '%Y%m%d'), format= '%m', tz= "GMT")
