library(R.utils)
require(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")

###______________________ Maxent model __________________________###
R <- 5000 # Number of boot-strap samples to draw for class-specific density estimates
A.TB <- 27.80448817 + (30.62204 * 4) # Area surveyed in units of 100 ha X 5 years study duration
A.CC <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together
A <- A.TB + A.CC

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

# Compile class-specific densities and boot-strap CIs at the transect level. #
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Plotting_cache_Mxnt_densities_BS.csv", row.names = F)
dat.class <- read.csv("Plotting_cache_Mxnt_densities_BS.csv", header = T, stringsAsFactors = F)

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
labxy <- list(Low = c(x = 0.2, y = 1.7), 
              Moderate = c(x = 0.47, y = 1.7),
              High = c(x = 0.75, y = 1.7))

plt.mx <- plotDens(dat.bin, dat.class, nests$HSI, thresholds, binPntSize, classPntSize, axisLabSize,
                    tickLabSize,BS = T, ylabel = NULL,
                   xlabel = "Maxent HSI")
plt.mx <- plt.mx +
  annotate("text", x = labxy$Low["x"], y = labxy$Low["y"], label = "Low", size = classLabSize) +
  annotate("text", x = labxy$Moderate["x"], y = labxy$Moderate["y"], label = "Moderate", size = classLabSize) +
  annotate("text", x = labxy$High["x"], y = labxy$High["y"], label = "High", size = classLabSize)

###_________________________________________________________________###

###______________________ WLR model __________________________###

## Remove previous weights and consolidate Toolbox data ##
TB_PAdata <- TB_PAdata %>%
  select(Point_ID, TID) %>%
  unique() %>%
  left_join((TB_PAdata %>%
               select(Point_ID, Nest:PIPO_perc, PIPO) %>%
               group_by(Point_ID) %>%
               summarise_all(funs(mean))), by = "Point_ID")
##______________________________________________________##

R <- 5000 # Number of boot-strap samples to draw for class-specific density estimates
A.TB <- 22.77014059 + (24.84801204 * 4) # Area surveyed in units of 100 ha X 5 years study duration
A.CC <- 43.46927683 + 47.26782927 # Area surveyed in units of 100 ha for each years added together
A <- A.TB + A.CC

mod <- loadObject("WtLogReg/WLRtop_TB&CC")

nests <- TB_PAdata %>% filter(Nest == 1) %>%
  filter(!Point_ID %in% unhatched.nests) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc) %>%
  bind_rows(CC_PAdata %>% filter(Nest == 1) %>%
              filter(!Point_ID %in% unhatched.nests) %>%
              select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc))
bkg.ratio <- A.CC / A.TB
bkg.TB <- (TB_PAdata %>% filter(Nest == 0))
bkg.TB <- bkg.TB %>% slice(sample(nrow(bkg.TB), 10000, replace = T)) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)
bkg.CC <- CC_PAdata %>% filter(Nest == 0)
bkg.CC <- bkg.CC %>% slice(sample(nrow(bkg.CC), (10000 * bkg.ratio) %>% round, replace = T)) %>%
  select(TID, brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc)

bkg <- bkg.TB %>% bind_rows(bkg.CC)
rm(bkg.TB, bkg.CC)

nests$HSI <- predict(mod, nests, type = "response")
bkg$HSI <- predict(mod, bkg, type = "response")
transects <- unique(c(nests$TID, bkg$TID))
thresholds <- c(0.3, 0.53) # Thresholds for low, moderate, and high suitability classes

# Compile class-specific densities and boot-strap CIs at the transect level. #
#dat.class <- calcClassDensities(nests$HSI, bkg$HSI, thresholds, A)
#dat.class$PercNest <- (((dat.class$Density) / sum(dat.class$Density))*100) %>% round
#dat.class <- dat.class %>% HSIClassDensityBS(nests, bkg, transects, thresholds, A, R, UnitID = "TID", HSI = "HSI")
#write.csv(dat.class, "Plotting_cache_WLR_densities_BS.csv")
dat.class <- read.csv("Plotting_cache_WLR_densities_BS.csv", header = T, stringsAsFactors = F)

# Compile bin densities
bins <- calcBins(1500, nrow(bkg), 2)
dat.bin <- calcBinDensities(nests$HSI, bkg$HSI, bins, A)

# Generate plot #
binPntSize <- 2   # Size of points representing moving-window bin values
classPntSize <- 5   # Size of points representing HSI-class values
tickLabSize <- 15   # Size of axis tick labels
axisLabSize <- 20   # Size of axis title labels

# Coordinates for plot labels
classLabSize <- 6   # Size of labels for low, moderate, and high suitability classes
labxy <- list(Low = c(x = 0.17, y = 1.25), 
              Moderate = c(x = 0.4, y = 1.25, angle = 0),
              High = c(x = 0.7, y = 1.25))

#plt.wlr <- plotDens(dat.bin, dat.class, nests$HSI, thresholds, binPntSize, classPntSize, axisLabSize,
#                   tickLabSize, classLabSize, labxy, BS = T, ylabel = NULL,
#                   xlabel = "WLR HSI")
plt.wlr <- plotDens(dat.bin, dat.class, nests$HSI, thresholds, binPntSize, classPntSize, axisLabSize,
                   tickLabSize,BS = T, ylabel = NULL,
                   xlabel = "WLR HSI")
plt.wlr <- plt.wlr +
  annotate("text", x = labxy$Low["x"], y = labxy$Low["y"], label = "Low", size = classLabSize) +
  annotate("text", x = labxy$Moderate["x"], y = labxy$Moderate["y"], label = "Moderate", size = classLabSize) +
  annotate("text", x = labxy$High["x"], y = labxy$High["y"], label = "High", size = classLabSize)

###_________________________________________________________________###

###___________________Put it all together___________________________###

theme_set(theme_bw())
p <- ggdraw() + 
  draw_plot(plt.mx, x = 0.05, y = 0, width = 0.475, height = 1) +
  draw_plot(plt.wlr, x = 0.525, y = 0, width = 0.475, height = 1) +
  draw_plot_label(label = "Density - hatched nests per 100 ha", size = 20, x = 0.01, y = 0.12,
                  hjust = 0, angle = 90)
#p

save_plot("HSI_validation_MS/Figure_HSI_densities.tiff", p, ncol = 3, nrow = 1.5, dpi = 200)
