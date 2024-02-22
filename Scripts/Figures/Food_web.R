library(Rpath)
library(data.table)
library(dplyr)
library(ggplot2)
standard_theme <- theme_bw() + theme(panel.border = element_rect(fill=NA, colour = "black")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.text.align= 0, legend.title= element_text(size = 12), legend.text = element_text(size= 10), axis.text=element_text(size=10), axis.title=element_text(size=12))

basicinput <- read.csv("pamlicosoundbasicinput.csv")
basicinput$Group <- tolower(basicinput$Group)
basicinput$Group <- gsub(" ", "", basicinput$Group)

groups <- as.vector(basicinput$Group)
groups <- c(groups, "gillnet", "trawl", "pots", "poundnets", "recreational")
# groups <- c('whales', 'seals', 'cod', 'whiting', 'mackerel', 'anchovy', 'shrimp',
#'benthos', 'zooplankton', 'phytoplankton', 'Detritus', 'sealers', 
#'trawlers', 'seiners', 'bait boats', 'shrimpers')
groups
types <- c(rep(0, 40), 1, 1, 2, rep(3, 5))

pam.params <- create.rpath.params(groups, types)

#Basic Input
#Biomass values
for (i in 1:length(basicinput$Group)) {
  pam.params$model[Group == basicinput$Group[i], Biomass := basicinput$Biomass[i]]
  pam.params$model[Group == basicinput$Group[i], PB := basicinput$PB[i]]
  pam.params$model[Group == basicinput$Group[i], EE := basicinput$EE[i]]
  pam.params$model[Group == basicinput$Group[i], QB := basicinput$QB[i]]
  pam.params$model[Group == basicinput$Group[i], ProdCons := basicinput$ProdCons[i]]
  pam.params$model[Group == basicinput$Group[i], Unassim := basicinput$Unassim[i]]
  pam.params$model[Group == basicinput$Group[i], DetInput := basicinput$detritus[i]]
}
pam.params$model[Group == 'detritus', EE := NA]

#Diet values
diet <- as.data.table(read.csv("pamlicosounddietmatrix.csv"))
diet$Group <- tolower(diet$Group)
diet[44,2] <- "Import"
diet$Group <- gsub(" ", "", diet$Group)
diet <- diet[ , -1]
names <- as.vector(diet$Group)
diet[, phytoplankton := NA]
diet[, sav := NA]
names <- c("Group", names)
names <- names[1:43]
names(diet) <- names
pam.params[["diet"]] <- diet

colSums(diet[, 2:43])

#Catch values
catch <- read.csv("pamlicosoundlandings.csv")
catch$Group <- tolower(catch$Group)
catch$Group <- gsub(" ", "", catch$Group)
catch <-  head(catch, - 1)  
catch <- select(catch, gillnet, trawl, pots, poundnets, recreational)
catch[nrow(catch)+5,] <- NA
pam.params[["model"]]$gillnet <- catch$gillnet
pam.params[["model"]]$trawl <- catch$trawl
pam.params[["model"]]$pots <- catch$pots
pam.params[["model"]]$poundnets <- catch$poundnets
pam.params[["model"]]$recreational <- catch$recreational

#Discard values
discards <- read.csv("pamlicosounddiscards.csv")
discards$Group <- tolower(discards$Group)
discards$Group <- gsub(" ", "", discards$Group)
discards <-  head(discards, - 1)  
discards <- select(discards, gillnet, trawl, pots, poundnets, recreational)
discards[nrow(discards)+5,] <- NA
pam.params[["model"]]$gillnet.disc <- discards$gillnet
pam.params[["model"]]$trawl.disc <- discards$trawl
pam.params[["model"]]$pots.disc <- discards$pots
pam.params[["model"]]$poundnets.disc <- discards$poundnets
pam.params[["model"]]$recreational.disc <- discards$recreational

#Biomass accumulation and unassimilated production
pam.params$model[Type < 3, BioAcc  := 0]
pam.params$model[Type < 2, Unassim := 0.2]
pam.params$model[Type == 2, Unassim := 0]
#biomass accumulation
pam.params$model[Group == 'atlanticmenhaden', BioAcc := -0.076]
pam.params$model[Group == 'bluecrabs', BioAcc := -0.071]

# #Detrital Fate
pam.params$model[Type == 2, DetInput := 29.09]
pam.params$model[Type != 2, DetInput := NA]

#DetInput
pam.params$model[Type < 2, detritus := 1]
pam.params$model[Type > 1, detritus := 0]

#Check for issues in your parameter file with
check.rpath.params(pam.params)

#Use Rpath to run the R-based implementation of Ecopath with Ecosim (EwE)
#Solve for EE to balance an unbalanced food web object and convert the model to an Rpath object
PSbal <- rpath(pam.params, 'Pamlico Sound')
PSbal
print(PSbal, morts = T)


#CREATE FIGURE!
tiff("/Users/sallydowd/Desktop/web.plot.tiff", units="in", width=7, height=5, res=600, compression = 'lzw')
webplot(PSbal, fleets= TRUE, eco.name= attr(PSbal, "Pamlico Sound"), line.col= "grey", labels= TRUE, label.pos= 2, label.num= TRUE, label.cex = 0.7) + standard_theme
dev.off()
