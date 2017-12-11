library(R.utils)
require(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

###______________________ Maxent model __________________________###
R <- 5000 # Number of boot-strap samples to draw for class-specific density estimates
A.TB <- 27.80448817 + (30.62204 * 4) # Area surveyed in units of 100 ha X 5 years study duration
A.CC <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together
A <- A.TB + A.CC

## Convert area to units of 1000 acres ##
A.TB <- A.TB * (247.105 / 1000)
A.CC <- A.CC * (247.105 / 1000)
A <- A * (247.105 / 1000)

nests <- TB_nsts %>% filter(unhatched == 0) %>%
  select(TID, Slp:pipo_1km) %>%
  bind_rows(CC_nsts %>% filter(unhatched == 0) %>%
              select(TID, Slp:pipo_1km))
bkg.ratio <- A.CC / A.TB
bkg <- TB_bkg %>% select(TID, Slp:pipo_1km) %>%
  bind_rows(
    CC_bkg[sample(nrow(CC_bkg), (bkg.ratio * nrow(TB_bkg)) %>% round),] %>%
      select(TID, Slp:pipo_1km))

nests$HSI <- WHWO_burned(nests$brnopn_1ha, nests$brnopn_1km)$hsi
bkg$HSI <- WHWO_burned(bkg$brnopn_1ha, bkg$brnopn_1km)$hsi
transects <- unique(bkg$TID)
thresholds <- c(0.34, 0.6) # Thresholds for low, moderate, and high suitability classes

# Get class-specific densities and boot-strap CIs at the transect level. #
dat.class <- read.csv("Plotting_cache_Mxnt_densities_BS.csv", header = T, stringsAsFactors = F)
dat.class[, c("Density", "Dens95lo", "Dens95hi")] <- # Convert to nests per 1000 acres
  (dat.class[, c("Density", "Dens95lo", "Dens95hi")] / 247.105) * 1000

# Compile bin densities
bins <- calcBins(600, nrow(bkg), 4)
dat.bin <- calcBinDensities(nests$HSI, bkg$HSI, bins, A)

# Generate plot #
binPntSize <- 2   # Size of points representing moving-window bin values
classPntSize <- 5   # Size of points representing HSI-class values
tickLabSize <- 15   # Size of axis tick labels
axisLabSize <- 20   # Size of axis title labels

# Coordinates for plot labels
classLabSize <- 6   # Size of labels for low, moderate, and high suitability classes
labxy <- list(Low = c(x = 0.2, y = 1.7 * 4.046863), 
              Moderate = c(x = 0.47, y = 1.7 * 4.046863),
              High = c(x = 0.75, y = 1.7 * 4.046863))

theme_set(theme_bw())
plt <- plotDens(dat.bin, dat.class, nests$HSI, thresholds, binPntSize, classPntSize, axisLabSize,
                   tickLabSize,BS = T, ylabel = "Density - hatched nests per 1000 ac",
                   xlabel = "Habitat Suitability Index (HSI)")
plt <- plt +
  annotate("text", x = labxy$Low["x"], y = labxy$Low["y"], label = "Low", size = classLabSize) +
  annotate("text", x = labxy$Moderate["x"], y = labxy$Moderate["y"], label = "Moderate", size = classLabSize) +
  annotate("text", x = labxy$High["x"], y = labxy$High["y"], label = "High", size = classLabSize)


save_plot("F:/research stuff/FS_PostDoc/Model_application_tool/Fig_WHWO_burn_Dens.tiff",
          plt, ncol = 2.5, nrow = 2.5, dpi = 200)
