source("~/Documents/GitHub/NCBlueCrab_Predators/Scripts/Load.SDM.Final.R")

#Load models
load("/users/sallydowd/Desktop/Ch1Data/Final_results/gJam/Burnin/Both_ind/modo3/both_ind_burn3.RData")
load("/users/sallydowd/Desktop/Ch1Data/Final_results/gJam/Burnin/One_ind/modo3/one_ind_burn3.RData")
load("/users/sallydowd/Desktop/Ch1Data/Final_results/gJam/Burnin/Non_avg/modo2/non_avg2.RData")

#Load data used

#Model 1
df1 <- df_CPUE_length_wide_both 
xdata1 <- df1 %>% dplyr::select(avgdepth, avgstemp, avgssal, avgsdo, NoFishRest, Yearfactor)
ydata1 <- df1 %>% dplyr::select(smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915, smallatlanticcroakerP120, smallbluecrabP120, brownshrimpP120, whiteshrimpP120, pinkshrimpP120, pinfishP120, southernflounderP120, smallspotP120, atlanticmenhadenP120)
species1 <- gjamTrimY(ydata1, 50, OTHER = FALSE)$y %>% colnames() #minimum # of non-zero observations: 50
ydata1 <- ydata1[, species1] %>% as.data.frame() #filter out rare species
tot1 <- cbind(xdata1, ydata1)
set.seed(123)
smp_size1 <- floor(0.70 * nrow(df1))
train_ind1 <- sample(seq_len(nrow(tot1)), size = smp_size1)
train1 <- tot1[train_ind1, ]
test1 <- tot1[-train_ind1, ]
xdata_train1 <- train1[,colnames(xdata1)]
ydata_train1 <- train1[,colnames(ydata1)]
xdata_test1 <- test1[,colnames(xdata1)]
ydata_test1 <- test1[,colnames(ydata1)]

#Model 2 
#May and June, 2001-2019
df2 <- df_CPUE_length_wide_P915
xdata2 <- df2 %>% dplyr::select(avgdepth, avgstemp, avgssal, avgsdo, NoFishRest, Yearfactor)
ydata2 <- df2 %>% dplyr::select(smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915)
species2 <- gjamTrimY(ydata2, 50, OTHER = FALSE)$y %>% colnames() #minimum # of non-zero observations: 50
ydata2 <- ydata2[, species2] %>% as.data.frame() #filter out rare species
tot2 <- cbind(xdata2, ydata2)
set.seed(123)
smp_size2 <- floor(0.70 * nrow(df2))
train_ind2 <- sample(seq_len(nrow(tot2)), size = smp_size2)
train2 <- tot2[train_ind2, ]
test2 <- tot2[-train_ind2, ]
xdata_train2 <- train2[,colnames(xdata2)]
ydata_train2 <- train2[,colnames(ydata2)]
xdata_test2 <- test2[,colnames(xdata2)]
ydata_test2 <- test2[,colnames(ydata2)]

#Model 3 
#May and June, 2001-2019
df3 <- df_CPUE_length_wide_indP915
xdata3 <- df3 %>% dplyr::select(Depth, Stemp, Ssal, Sdo, NoFishRest, Yearfactor)
ydata3 <- df3 %>% dplyr::select(smallatlanticcroakerP915, smallatlanticmenhadenP915, blackdrumP915, pinfishP915, reddrumP915, smallsouthernflounderP915, southernkingfishP915, smallspotP915)
species3 <- gjamTrimY(ydata3, 50, OTHER = FALSE)$y %>% colnames() #minimum # of non-zero observations: 50
ydata3 <- ydata3[, species3] %>% as.data.frame() #filter out rare species
tot3 <- cbind(xdata3, ydata3)
set.seed(123)
smp_size3 <- floor(0.70 * nrow(df3))
train_ind3 <- sample(seq_len(nrow(tot3)), size = smp_size3)
train3 <- tot3[train_ind3, ]
test3 <- tot3[-train_ind3, ]
xdata_train3 <- train3[,colnames(xdata3)]
ydata_train3 <- train3[,colnames(ydata3)]
xdata_test3 <- test3[,colnames(xdata3)]
ydata_test3 <- test3[,colnames(ydata3)]
