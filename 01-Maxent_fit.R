setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")
require(dismo)
require(dplyr)
library(R.utils)

## Toolbox ##
# Global model #
x <- rbind(TB_bkg[,c("Slp","cosasp","brnopn_1ha","brnopn_1km","pipo_1km")],
           TB_nsts[,c("Slp","cosasp","brnopn_1ha","brnopn_1km","pipo_1km")])
p <- c(rep(0,nrow(TB_bkg)),rep(1,nrow(TB_nsts)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold","noaddsamplestobackground",
                       "noautofeature","responsecurves"),
            path="Maxent_models/Toolbox/Global_brnopn")

# Reduced model #
x <- rbind(TB_bkg[,c("brnopn_1ha","brnopn_1km","pipo_1km")],
           TB_nsts[,c("brnopn_1ha","brnopn_1km","pipo_1km")])
p <- c(rep(0,nrow(TB_bkg)),rep(1,nrow(TB_nsts)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold","noaddsamplestobackground",
                       "noautofeature","responsecurves"),
            path="Maxent_models/Toolbox/Reduced_brnopn")
#saveObject(m,"Maxent_models/Toolbox/WHWO_Mxnt_Toolbox_red1")

## Canyon Creek ##
# Global model #
x <- rbind(CC_nsts[,c("Slp","cosasp","brnopn_1ha","brnopn_1km","pipo_1km")],
           CC_bkg[,c("Slp","cosasp","brnopn_1ha","brnopn_1km","pipo_1km")])
p <- c(rep(1,nrow(CC_nsts)),rep(0,nrow(CC_bkg)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold",
                       "noaddsamplestobackground", "noautofeature","responsecurves"),
            path="Maxent_models/CanCrk/Global_brnopn")

# Reduced model #
x <- rbind(CC_nsts[,c("Slp","brnopn_1ha","brnopn_1km")],
           CC_bkg[,c("Slp","brnopn_1ha","brnopn_1km")])
p <- c(rep(1,nrow(CC_nsts)),rep(0,nrow(CC_bkg)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold",
                       "noaddsamplestobackground", "noautofeature","responsecurves"),
            path="Maxent_models/CanCrk/Reduced_brnopn")
#saveObject(m,"Maxent_models/CanCrk/WHWO_Mxnt_CanCrk_red1")

## Both fires ##
# Global #
x <- rbind(CC_nsts[,c("Slp","brnopn_1ha","brnopn_1km","pipo_1km")],
           TB_nsts[,c("Slp","brnopn_1ha","brnopn_1km","pipo_1km")],
           CC_bkg[sample(nrow(CC_bkg),10000,replace = F),
                  c("Slp","brnopn_1ha","brnopn_1km","pipo_1km")],
           TB_bkg[, c("Slp","brnopn_1ha","brnopn_1km","pipo_1km")])
p <- c(rep(1,nrow(CC_nsts)),rep(1,nrow(TB_nsts)),rep(0,nrow(CC_bkg)/2),rep(0,nrow(TB_bkg)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold",
                       "noaddsamplestobackground", "noautofeature","responsecurves"),
            path="Maxent_models/Both_fires/Global_brnopn")

# Reduced #
x <- rbind(CC_nsts[,c("brnopn_1ha","brnopn_1km")],
           TB_nsts[,c("brnopn_1ha","brnopn_1km")],
           CC_bkg[sample(nrow(CC_bkg),10000,replace = F),
                  c("brnopn_1ha","brnopn_1km")],
           TB_bkg[, c("brnopn_1ha","brnopn_1km")])
p <- c(rep(1,nrow(CC_nsts)),rep(1,nrow(TB_nsts)),rep(0,nrow(CC_bkg)/2),rep(0,nrow(TB_bkg)))
m <- maxent(x,p,args=c("noaskoverwrite","noremoveduplicates","nohinge","nothreshold",
                       "noaddsamplestobackground", "noautofeature","responsecurves"),
            path="Maxent_models/Both_fires/Reduced_brnopn")
#saveObject(m,"Maxent_models/Both_fires/WHWO_Mxnt_both_red1")

# Temp check application to BP #
#m <- loadObject("Maxent_models/Both_fires/WHWO_Mxnt_both_red1")
#x <- rbind(BP_nsts[,c("brnopn_1ha","brnopn_1km")],
#           BP_bkg[,c("brnopn_1ha","brnopn_1km")])
#p <- c(rep(1,nrow(BP_nsts)),rep(0,nrow(BP_bkg)))
#HSI <- predict(m, x)
#mean(HSI[which(p == 1)])
#mean(HSI[which(p == 0)])
#hist(HSI[which(p == 1)])
#hist(HSI[which(p == 0)])
#library(PresenceAbsence)
#auc(cbind(ID = 1:length(p), p, HSI))
