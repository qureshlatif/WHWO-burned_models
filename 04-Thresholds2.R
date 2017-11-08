library(R.utils)
library(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

cols <- c("Low", "Moderate", "High")
rows <- c("MX_TB_Dens", "MX_TB_Prct", "MX_TB_ArSrv", "MX_CC_Dens", "MX_CC_Prct", "MX_CC_ArSrv",
          "WLR_TB_Dens", "WLR_TB_Prct", "WLR_TB_ArSrv", "WLR_CC_Dens", "WLR_CC_Prct", "WLR_CC_ArSrv")
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
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Table_cache_Mxnt_TBdensities_BS.csv")
dat.class <- read.csv("Table_cache_Mxnt_TBdensities_BS.csv", header = T, stringsAsFactors = F)

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
  mutate(HSI_class = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["MX_TB_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSI_class, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100) %>% round(digits = 1)

# Canyon Creek #
A <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together
nests <- CC_nsts %>% filter(unhatched == 0) %>% select(TID, Slp:pipo_1km)
bkg <- CC_bkg %>% select(TID, Slp:pipo_1km)

nests$HSI <- WHWO_burned(nests$brnopn_1ha, nests$brnopn_1km)$hsi
bkg$HSI <- WHWO_burned(bkg$brnopn_1ha, bkg$brnopn_1km)$hsi
transects <- unique(bkg$TID)
thresholds <- c(0.34, 0.6) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Table_cache_Mxnt_CCdensities_BS.csv")
dat.class <- read.csv("Table_cache_Mxnt_CCdensities_BS.csv", header = T, stringsAsFactors = F)

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
  mutate(HSI_class = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["MX_CC_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSI_class, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100) %>% round(digits = 1)

###_________________________________________________________________###

###______________________ WLR model __________________________###
## Remove previous weights and reduce Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, TID) %>%
  unique() %>%
  left_join((TB_PAdata %>%
               select(Point_ID, Nest:PIPO_perc, PIPO) %>%
               group_by(Point_ID) %>%
               summarise_all(funs(mean))), by = "Point_ID")
##_________________________________________________##

mod <- loadObject("WtLogReg/WLRtop_TB&CC")

# Toolbox #
A <- 22.77014059 + (24.84801204 * 4) # Area surveyed in units of 100 ha X 5 years study duration

nests <- TB_PAdata %>% filter(Nest == 1) %>%
  filter(!Point_ID %in% unhatched.nests) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)
bkg <- (TB_PAdata %>% filter(Nest == 0)) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)

nests$HSI <- predict(mod, nests, type = "response")
bkg$HSI <- predict(mod, bkg, type = "response")
transects <- unique(c(nests$TID, bkg$TID))
thresholds <- c(0.3, 0.53) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Table_cache_WLR_TBdensities_BS.csv")
dat.class <- read.csv("Table_cache_WLR_TBdensities_BS.csv", header = T, stringsAsFactors = F)

out["WLR_TB_Dens", ] <- str_c(dat.class[, "Density"] %>% round(digits = 2), " (",
                             dat.class[, "Dens95lo"] %>% round(digits = 2), ",",
                             dat.class[, "Dens95hi"] %>% round(digits = 2), ")")
out["WLR_TB_Prct", ] <- str_c(dat.class[, "PercNest"] %>% round, " (",
                             dat.class[, "Perc95lo"] %>% round, ",",
                             dat.class[, "Perc95hi"] %>% round, ")")
bkg <- bkg %>% mutate(HSIclass = "low") %>%
  mutate(HSIclass = replace(HSIclass,
                            which(bkg$HSI >= thresholds[1] &
                                    bkg$HSI < thresholds[2]), "mod")) %>%
  mutate(HSI_class = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["WLR_TB_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSI_class, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100) %>% round(digits = 1)

# Canyon Creek #
A <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together

nests <- CC_PAdata %>% filter(Nest == 1) %>%
  filter(!Point_ID %in% unhatched.nests) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)
bkg <- (CC_PAdata %>% filter(Nest == 0)) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)

nests$HSI <- predict(mod, nests, type = "response")
bkg$HSI <- predict(mod, bkg, type = "response")
transects <- unique(c(nests$TID, bkg$TID))
thresholds <- c(0.3, 0.53) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Table_cache_WLR_CCdensities_BS.csv")
dat.class <- read.csv("Table_cache_WLR_CCdensities_BS.csv", header = T, stringsAsFactors = F)

out["WLR_CC_Dens", ] <- str_c(dat.class[, "Density"] %>% round(digits = 2), " (",
                              dat.class[, "Dens95lo"] %>% round(digits = 2), ",",
                              dat.class[, "Dens95hi"] %>% round(digits = 2), ")")
out["WLR_CC_Prct", ] <- str_c(dat.class[, "PercNest"] %>% round, " (",
                              dat.class[, "Perc95lo"] %>% round, ",",
                              dat.class[, "Perc95hi"] %>% round, ")")
bkg <- bkg %>% mutate(HSIclass = "low") %>%
  mutate(HSIclass = replace(HSIclass,
                            which(bkg$HSI >= thresholds[1] &
                                    bkg$HSI < thresholds[2]), "mod")) %>%
  mutate(HSI_class = replace(HSIclass,
                             which(bkg$HSI >= thresholds[2]), "high"))
out["WLR_CC_ArSrv", ] <- ((tapply(bkg$HSI, bkg$HSI_class, length)[c("low", "mod", "high")] /
                           nrow(bkg)) * A * 100) %>% round(digits = 1)

###_________________________________________________________________###

write.csv(out, "HSIclass_densities_by_location.csv", row.names = T)
