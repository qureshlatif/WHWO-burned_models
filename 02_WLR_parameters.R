library(R.utils)
library(stringr)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

rows <- c("Intercept", "LocBrnOpn", "LandBrnOpn", "TreeDens", "PIPO%")
cols <- c("TB", "CC", "TB & CC")
out <- matrix("", nrow = length(rows), ncol = length(cols))
dimnames(out) <- list(rows, cols)

mod <- loadObject("WtLogReg/TB/TB_WLRtop")
out[, "TB"] <- str_c(summary(mod)$coefficients[, "Estimate"] %>%
                       round(digits = 3),
                     " (",
                     summary(mod)$coefficients[, "Std. Error"] %>%
                       round(digits = 3),
                     ")")
mod <- loadObject("WtLogReg/CC/CC_WLRtop")
out[c("Intercept", "LandBrnOpn", "TreeDens"), "CC"] <-
  str_c(summary(mod)$coefficients[, "Estimate"] %>%
                       round(digits = 3),
                     " (",
                     summary(mod)$coefficients[, "Std. Error"] %>%
                       round(digits = 3),
                     ")")
mod <- loadObject("WtLogReg/WLRtop_TB&CC")
out[, "TB & CC"] <-
  str_c(summary(mod)$coefficients[, "Estimate"] %>%
          round(digits = 3),
        " (",
        summary(mod)$coefficients[, "Std. Error"] %>%
          round(digits = 3),
        ")")

write.csv(out, "WtLogReg/Parameter_ests.csv")
