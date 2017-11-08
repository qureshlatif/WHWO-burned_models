library(R.utils)
library(WoodpeckerHSI)
library(pROC)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
source("HSI_validation_MS/scripts/Maxent_HSI_functions.r")
load("HSI_validation_MS/Data_compiled.RData")

## Remove previous weights and consolidate Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, Nest:PIPO_perc, PIPO) %>%
  group_by(Point_ID) %>%
  summarise_all(funs(mean))
##______________________________________________________##

rows <- c("MX_TB", "MX_CC", "MX_BP", "WLR_TB", "WLR_CC", "WLR_BP")
cols <- c("TB", "CC", "BOTH")
TabAUC <- matrix("", nrow = length(rows), ncol = length(cols))
dimnames(TabAUC) <- list(rows, cols)

### Maxent (remote-sensed) models ###
## TB model ##
pred <- Maxent_TB(TB_PAdata$brnopn_1ha, TB_PAdata$brnopn_1km, TB_PAdata$pipo_1km)$hsi
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_TB", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- Maxent_TB(CC_PAdata$brnopn_1ha, CC_PAdata$brnopn_1km, CC_PAdata$pipo_1km)$hsi
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_CC", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- Maxent_TB(BP_PAdata$brnopn_1ha, BP_PAdata$brnopn_1km, BP_PAdata$pipo_1km)$hsi
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_BP", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

## CC model ##
pred <- Maxent_CC(TB_PAdata$brnopn_1ha, TB_PAdata$brnopn_1km, TB_PAdata$slope)$hsi
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_TB", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- Maxent_CC(CC_PAdata$brnopn_1ha, CC_PAdata$brnopn_1km, CC_PAdata$slope)$hsi
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_CC", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- Maxent_CC(BP_PAdata$brnopn_1ha, BP_PAdata$brnopn_1km, BP_PAdata$slope)$hsi
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_BP", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

## Model fitted to both locations ##
pred <- WHWO_burned(TB_PAdata$brnopn_1ha, TB_PAdata$brnopn_1km)$hsi
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_TB", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- WHWO_burned(CC_PAdata$brnopn_1ha, CC_PAdata$brnopn_1km)$hsi
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_CC", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- WHWO_burned(BP_PAdata$brnopn_1ha, BP_PAdata$brnopn_1km)$hsi
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["MX_BP", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

### Weighted logistic regression (RS & field-collected) ###
## Toolbox model ##
mod <- loadObject("WtLogReg/TB/TB_WLRtop")
pred <- predict(mod, TB_PAdata, type = "response")
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_TB", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, CC_PAdata, type = "response")
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_CC", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, BP_PAdata, type = "response")
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_BP", "TB"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

## Canyon Creek Model ##
mod <- loadObject("WtLogReg/CC/CC_WLRtop")
pred <- predict(mod, TB_PAdata, type = "response")
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_TB", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, CC_PAdata, type = "response")
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_CC", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, BP_PAdata, type = "response")
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_BP", "CC"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

## Model fitted to both locations ##
mod <- loadObject("WtLogReg/WLRtop_TB&CC")
pred <- predict(mod, TB_PAdata, type = "response")
AUC <- ci(roc(TB_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_TB", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, CC_PAdata, type = "response")
AUC <- ci(roc(CC_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_CC", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

pred <- predict(mod, BP_PAdata, type = "response")
AUC <- ci(roc(BP_PAdata$Nest, pred)) %>% as.numeric %>%
  round(digits = 2)
TabAUC["WLR_BP", "BOTH"] <- str_c(AUC[2], "(", AUC[1], ",", AUC[3], ")")

write.csv(TabAUC, "Predictive_performance.csv", row.names = T)
