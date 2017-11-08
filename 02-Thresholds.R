library(R.utils)
library(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

## Remove previous weights and consolidate Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, Nest:PIPO_perc, PIPO) %>%
  group_by(Point_ID) %>%
  summarise_all(funs(mean))
##______________________________________________________##

### Find maxSSS thresholds ###

rows <- c("MX_TB", "MX_CC", "MX_BP", "WLR_TB", "WLR_CC", "WLR_BP")
cols <- c("Threshold", "Sensitivity", "Specificity")
TabThrsh <- matrix("", nrow = length(rows), ncol = length(cols))
dimnames(TabThrsh) <- list(rows, cols)

PA_data <- TB_PAdata %>%
  select(Nest, brnopn_1ha, brnopn_1km, pipo_1km, Tree_ovr25, PIPO_perc) %>%
  mutate(LOC = "TB") %>%
  bind_rows(CC_PAdata %>%
              select(Nest, brnopn_1ha, brnopn_1km, pipo_1km, Tree_ovr25, PIPO_perc) %>%
              mutate(LOC = "CC"))

### Maxent model ###
obs <- PA_data$Nest
hsi <- WHWO_burned(PA_data$brnopn_1ha, PA_data$brnopn_1km)$hsi
thrsh.mxnt <- SnsPlsSpcMax(obs, hsi)
TabThrsh["MX_TB", "Threshold"] <- thrsh.mxnt$HSI_thrshld

dat <- TB_PAdata
obs <- dat$Nest
hsi <- WHWO_burned(TB_PAdata$brnopn_1ha, TB_PAdata$brnopn_1km)$hsi
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["MX_TB", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["MX_TB", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

dat <- CC_PAdata
obs <- dat$Nest
hsi <- WHWO_burned(CC_PAdata$brnopn_1ha, CC_PAdata$brnopn_1km)$hsi
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["MX_CC", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["MX_CC", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

dat <- BP_PAdata
obs <- dat$Nest
hsi <- WHWO_burned(BP_PAdata$brnopn_1ha, BP_PAdata$brnopn_1km)$hsi
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["MX_BP", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["MX_BP", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

### WLR model ###
mod <- loadObject("WtLogReg/WLRtop_TB&CC")
obs <- PA_data$Nest
hsi <- predict(mod, PA_data, type = "response")
thrsh.wlr <- SnsPlsSpcMax(obs, hsi)
TabThrsh["WLR_TB", "Threshold"] <- thrsh.wlr$HSI_thrshld

dat <- TB_PAdata
obs <- dat$Nest
hsi <- predict(mod, dat, type = "response")
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["WLR_TB", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["WLR_TB", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

dat <- CC_PAdata
obs <- dat$Nest
hsi <- predict(mod, dat, type = "response")
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["WLR_CC", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["WLR_CC", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

dat <- BP_PAdata
obs <- dat$Nest
hsi <- predict(mod, dat, type = "response")
thrsh <- TabThrsh["MX_TB", "Threshold"] %>% as.numeric
TabThrsh["WLR_BP", "Sensitivity"] <- (sum(hsi[obs == 1] >= thrsh) / sum(obs == 1)) %>%
  round(digits = 2)
TabThrsh["WLR_BP", "Specificity"] <- (sum(hsi[obs == 0] < thrsh) / sum(obs == 0)) %>%
  round(digits = 2)

write.csv(TabThrsh, "MaxSSS_thresholds.csv", row.names = T)
