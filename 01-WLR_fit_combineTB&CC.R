library(WoodpeckerHSI)
library(R.utils)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

covs <- c("brnopn_1ha", "brnopn_1km", "pipo_1km", "Tree_ovr25", "PIPO_perc", "PIPO")

## Remove previous weights and consolidate Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, Nest:PIPO_perc, PIPO) %>%
  group_by(Point_ID) %>%
  summarise_all(funs(mean))

## Combine data and generate balanced observation weights ##
PA_data <- TB_PAdata %>%
  select(Nest, brnopn_1ha, brnopn_1km, pipo_1km, Tree_ovr25, PIPO_perc, PIPO) %>%
  mutate(LOC = "TB") %>%
  bind_rows(CC_PAdata %>%
              select(Nest, brnopn_1ha, brnopn_1km, pipo_1km, Tree_ovr25, PIPO_perc, PIPO) %>%
              mutate(LOC = "CC")) %>%
  mutate(wt = 1)

n <- sum(PA_data$Nest[which(PA_data$LOC == "TB")])
PA_data$wt[which(PA_data$Nest == 1 & PA_data$LOC == "CC")] <-
  n / sum(PA_data$Nest == 1 & PA_data$LOC == "CC")
n <- sum(PA_data$wt[which(PA_data$Nest == 1)])
PA_data$wt[which(PA_data$Nest == 0 & PA_data$LOC == "TB")] <-
  (n / 2) / sum(PA_data$Nest == 0 & PA_data$LOC == "TB")
PA_data$wt[which(PA_data$Nest == 0 & PA_data$LOC == "CC")] <-
  (n / 2) / sum(PA_data$Nest == 0 & PA_data$LOC == "CC")

## Generate candidate models, fit models, and compile AIC table for each location ##
kmax <- ceiling(sum(PA_data$wt[PA_data$Nest == 1]) / 10)
mods <- varCombosLinear(covs, K.max = kmax)
AICtab <- WLR_AICtable(PA_data, mods, w = PA_data$wt)
rm(kmax, mods)

write.csv(AICtab, "WtLogReg/WLR_AICtable_TB&CC.csv", row.names = F)

WLRtop <- WLR_fit(PA_data, AICtab$Model[1], w = PA_data$wt)
saveObject(WLRtop, "WtLogReg/WLRtop_TB&CC")
