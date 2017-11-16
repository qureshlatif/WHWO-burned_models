library(R.utils)
library(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

cols <- c("Low", "Moderate", "High")
rows <- c("MX_TB_Dens", "MX_TB_Prct", "MX_TB_ArSrv", "MX_CC_Dens", "MX_CC_Prct", "MX_CC_ArSrv")
out <- matrix("", nrow = length(rows), ncol = length(cols))
dimnames(out) <- list(rows, cols)

R <- 5000 # Number of boot-strap samples to draw for class-specific density estimates

###______________________ Maxent model __________________________###
# Toolbox #
A <- 27.80448817 + (30.62204 * 4) # Area surveyed in units of 100 ha X 5 years study duration
nests <- TB_nsts %>% filter(unhatched == 0) %>%
  select(TID, Slp:pipo_1km)
bkg <- TB_bkg %>% select(TID, Slp:pipo_1km)

nests$HSI <- WHWO_burned(nests$brnopn_1ha, nests$brnopn_1km)$hsi
bkg$HSI <- WHWO_burned(bkg$brnopn_1ha, bkg$brnopn_1km)$hsi
transects <- unique(bkg$TID)
thresholds <- c(0.34, 0.6) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
dat.class <- read.csv("Table_cache_Mxnt_TBdensities_BS.csv", header = T, stringsAsFactors = F)
dat.class[, c("Density", "Dens95lo", "Dens95hi")] <- # Convert to 1000 acre units
  dat.class[, c("Density", "Dens95lo", "Dens95hi")] * 4.046863

out["MX_TB_Dens", ] <- str_c(dat.class[, "Density"] %>% round(digits = 2), " (",
                             dat.class[, "Dens95lo"] %>% round(digits = 2), ",",
                             dat.class[, "Dens95hi"] %>% round(digits = 2), ")")
out["MX_TB_Prct", ] <- str_c(dat.class[, "PercNest"] %>% round, " (",
                             dat.class[, "Perc95lo"] %>% round, ",",
                             dat.class[, "Perc95hi"] %>% round, ")")
bkg <- bkg %>% mutate(HSIclass = "low") %>%
  mutate(HSIclass = replace(HSIclass,
                            which(bkg$HSI >= thresholds[1] &
                                    bkg$HSI < thresholds[2]), "mod")) %>%
  mutate(HSIclass = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["MX_TB_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSIclass, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100 * 2.47105) %>% round(digits = 1)

# Canyon Creek #
A <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together
nests <- CC_nsts %>% filter(unhatched == 0) %>% select(TID, Slp:pipo_1km)
bkg <- CC_bkg %>% select(TID, Slp:pipo_1km)

nests$HSI <- WHWO_burned(nests$brnopn_1ha, nests$brnopn_1km)$hsi
bkg$HSI <- WHWO_burned(bkg$brnopn_1ha, bkg$brnopn_1km)$hsi
transects <- unique(bkg$TID)
thresholds <- c(0.34, 0.6) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
dat.class <- read.csv("Table_cache_Mxnt_CCdensities_BS.csv", header = T, stringsAsFactors = F)
dat.class[, c("Density", "Dens95lo", "Dens95hi")] <- # Convert to 1000 acre units
  dat.class[, c("Density", "Dens95lo", "Dens95hi")] * 4.046863

out["MX_CC_Dens", ] <- str_c(dat.class[, "Density"] %>% round(digits = 2), " (",
                             dat.class[, "Dens95lo"] %>% round(digits = 2), ",",
                             dat.class[, "Dens95hi"] %>% round(digits = 2), ")")
out["MX_CC_Prct", ] <- str_c(dat.class[, "PercNest"] %>% round, " (",
                             dat.class[, "Perc95lo"] %>% round, ",",
                             dat.class[, "Perc95hi"] %>% round, ")")
bkg <- bkg %>% mutate(HSIclass = "low") %>%
  mutate(HSIclass = replace(HSIclass,
                            which(bkg$HSI >= thresholds[1] &
                                    bkg$HSI < thresholds[2]), "mod")) %>%
  mutate(HSIclass = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["MX_CC_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSIclass, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100 * 2.47105) %>% round(digits = 1)

write.csv(out, "HSIclass_densities_by_location_GISmanual.csv", row.names = T)
