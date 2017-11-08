library(WoodpeckerHSI)
library(R.utils)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

covs <- c("brnopn_1ha","brnopn_1km","slope","cosasp","pipo_1km","Snag_25_50","Snag_ovr50",
          "Tree_ovr25","PIPO_perc","PIPO")

## Check for multi-collinearity ##
#TB_checkMC <- checkMC(TB_PAdata, vars)
#print(TB_checkMC)
#print(max(TB_checkMC$VIF))
#print(max(TB_checkMC$cmat[-which(TB_checkMC$cmat == 1)]))

#CC_checkMC <- checkMC(CC_PAdata, vars[-which(vars == "Decay")])
#print(CC_checkMC)
#print(max(CC_checkMC$VIF))
#print(max(CC_checkMC$cmat[-which(CC_checkMC$cmat == 1)]))
#________________________________#

## Remove previous weights and consolidate Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, Nest:PIPO_perc, PIPO) %>%
  group_by(Point_ID) %>%
  summarise_all(funs(mean))

## Generate candidate models, fit models, and compile AIC table for each location ##
locations <- c("TB", "CC")

for(loc in locations) {
  dat <- eval(as.name(paste0(loc, "_PAdata")))
  kmax <- ceiling(sum(dat$Nest == 1) / 10)
  mods <- varCombosLinear(covs, K.max = kmax)
  AICtab <- WLR_AICtable(dat, mods)
  assign(paste0(loc, "_WLR_AICtable"), AICtab)
}
rm(dat, kmax, mods, AICtab, loc, locations)

write.csv(TB_WLR_AICtable, "WtLogReg/TB/TB_WLR_AICtable.csv", row.names = F)
write.csv(CC_WLR_AICtable, "WtLogReg/CC/CC_WLR_AICtable.csv", row.names = F)

TB_WLRtop <- WLR_fit(TB_PAdata, TB_WLR_AICtable$Model[1])
saveObject(TB_WLRtop, "WtLogReg/TB/TB_WLRtop")
CC_WLRtop <- WLR_fit(CC_PAdata, CC_WLR_AICtable$Model[1])
saveObject(CC_WLRtop, "WtLogReg/CC/CC_WLRtop")
