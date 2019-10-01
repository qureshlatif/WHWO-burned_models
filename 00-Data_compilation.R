setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")

require(foreign)
require(dplyr)
require(stringr)

#############################################
# Presence-background, remotely sensed data #
#############################################

## Toolbox ##
TB_Data <- read.dbf("E:/GISData/WHWO/brn_frst_project/Toolbox/TB_points_surveyed.dbf", as.is = T) %>%
  tbl_df() %>%
  select(-early_nest) %>%
  mutate(ccov_1ha = (ccov_1ha/9)*100) %>%
  mutate(brn_1ha = (brn_1ha/9)*100) %>%
  mutate(ccov_1km = (ccov_1km/3409)*100) %>%
  mutate(brnopn_1ha = (brnopn_1ha/9)*100) %>%
  mutate(brnopn_1km = (brnopn_1km/3409)*100) %>%
  mutate(log03_1ha = (log03_1ha/9)*100) %>%
  mutate(log03_1km = (log03_1km/3409)*100) %>%
  mutate(log04_1ha = (log04_1ha/9)*100) %>%
  mutate(log04_1km = (log04_1km/3409)*100) %>%
  mutate(log05_1ha = (log05_1ha/9)*100) %>%
  mutate(log05_1km = (log05_1km/3409)*100) %>%
  mutate(log06_1ha = (log06_1ha/9)*100) %>%
  mutate(log06_1km = (log06_1km/3409)*100) %>%
  mutate(log07_1ha = (log07_1ha/9)*100) %>%
  mutate(log07_1km = (log07_1km/3409)*100) %>%
  select(-TID) %>%
  rename(TID = BSunit) %>%
  select(Id, TID, X:nest, Slp, cosasp, brnopn_1ha, brnopn_1km, pipo_1km, log03_1ha:log07_1km, unhatched)

#write.csv(TB_Data,"Maxent_models/Toolbox/lndscp.csv",row.names=F)

# Nests
Pres_2003 <- TB_Data %>% filter(nest==2003) %>% select(Id:Y, unhatched, Slp:pipo_1km, log03_1ha, log03_1km) %>%
  rename(log_1ha = log03_1ha, log_1km = log03_1km)
Pres_2004 <- TB_Data %>% filter(nest==2004) %>% select(Id:Y, unhatched, Slp:pipo_1km, log04_1ha, log04_1km) %>%
  rename(log_1ha = log04_1ha, log_1km = log04_1km)
Pres_2005 <- TB_Data %>% filter(nest==2005) %>% select(Id:Y, unhatched, Slp:pipo_1km, log05_1ha, log05_1km) %>%
  rename(log_1ha = log05_1ha, log_1km = log05_1km)
Pres_2006 <- TB_Data %>% filter(nest==2006) %>% select(Id:Y, unhatched, Slp:pipo_1km, log06_1ha, log06_1km) %>%
  rename(log_1ha = log06_1ha, log_1km = log06_1km)
Pres_2007 <- TB_Data %>% filter(nest==2007) %>% select(Id:Y, unhatched, Slp:pipo_1km, log07_1ha, log07_1km) %>%
  rename(log_1ha = log07_1ha, log_1km = log07_1km)
Pres_data <- Pres_2003 %>% bind_rows(Pres_2004, Pres_2005, Pres_2006, Pres_2007) %>%
  mutate(Species = "WHWO") %>% select(Species, TID:log_1km)
rm(Pres_2003, Pres_2004, Pres_2005, Pres_2006, Pres_2007)
#write.csv(Pres_data, "Maxent_models/Toolbox/nests.csv",row.names=F)

TB_bkg <- TB_Data[sample(nrow(TB_Data), 10000, replace=F), ] %>%
  mutate(log_1ha = (log03_1ha + log04_1ha + log05_1ha + log06_1ha + log07_1ha) / 5,
         log_1km = (log03_1km + log04_1km + log05_1km + log06_1km + log07_1km) / 5)
ind <- which(TB_bkg$TID %in% c("J_669313", "J_669913"))
TB_bkg[ind, ] <- TB_bkg[ind, ] %>%
  mutate(log_1ha = (log04_1ha + log05_1ha + log06_1ha + log07_1ha) / 4,
         log_1km = (log04_1km + log05_1km + log06_1km + log07_1km) / 4)
TB_bkg <- TB_bkg %>% select(TID:pipo_1km, log_1ha, log_1km)
rm(ind)
TB_bkg <- TB_bkg %>% mutate(Species = "Background") %>%
  select(Species, TID:Y, Slp:log_1km)
#write.csv(TB_bkg, "Maxent_models/Toolbox/bkgrd.csv", row.names=F)
rm(TB_Data)

TB_nsts <- Pres_data
rm(Pres_data)

## Barry Point ##
BP_nsts <- read.csv("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/Maxent_models/Barry_point/BP_nests.csv",
                    header=T, stringsAsFactors=F) %>% tbl_df %>%
  mutate(brnopn_1ha_ravg = brnopn_1ha) %>%
  mutate(brnopn_1km_ravg = brnopn_1km) %>%
  mutate(brnopn_1ha = brnopn_1ha_mtbs) %>%
  mutate(brnopn_1km = brnopn_1km_mtbs) %>%
  select(Species:Y, slope, cosasp, brnopn_1ha, brnopn_1km, pipo_1km,
         brnopn_1ha_mtbs, brnopn_1km_mtbs, brnopn_1ha_ravg, brnopn_1km_ravg)
names(BP_nsts) <- names(TB_nsts)[-which(names(TB_nsts) %in% c("TID", "nest", "log_1ha", "log_1km", "unhatched"))]

BP_bkg <- read.csv("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/Maxent_models/Barry_point/BP_bkgrd.csv",
                   header=T, stringsAsFactors=F) %>% tbl_df %>%
  mutate(brnopn_1ha_ravg = brnopn_1ha) %>%
  mutate(brnopn_1km_ravg = brnopn_1km) %>%
  mutate(brnopn_1ha = brnopn_1ha_mtbs) %>%
  mutate(brnopn_1km = brnopn_1km_mtbs) %>%
  select(Species:Y, slope, cosasp, brnopn_1ha, brnopn_1km, pipo_1km,
         brnopn_1ha_mtbs, brnopn_1km_mtbs, brnopn_1ha_ravg, brnopn_1km_ravg)
names(BP_bkg) <- names(TB_bkg)[-which(names(TB_nsts) %in% c("TID", "nest", "log_1ha", "log_1km", "unhatched"))]

## Canyon Creek ##
CCC.data <- read.dbf("E:/GISData/WHWO/brn_frst_project/CanCrk/grid30.dbf", as.is = T) %>%
  tbl_df %>%
  mutate(brnopn_1ha = (brnopn_1ha / 9) * 100) %>%
  mutate(brnopn_1km = (brnopn_1km / 3409) * 100) %>%
  mutate(pipo_1km = (pipo_1km / 3409) * 100) %>%
  mutate(log_1ha = log_1ha * 100) %>%
  mutate(log_1km = log_1km * 100) %>%
  mutate(log1ha2016 = log1ha2016 * 100) %>%
  mutate(log1km2016 = log1km2016 * 100) %>%
  rename(log1ha2017 = log_1ha, log1km2017 = log_1km) %>%
  select(-TID) %>%
  rename(TID = BSunits)

CC_nsts2016 <- CCC.data %>% filter(WHWO_nest == 2016) %>%
  select(POINTID, TID, unhatched, slope:HSI_TB, log1ha2016, log1km2016) %>%
  rename(log_1ha = log1ha2016, log_1km = log1km2016)
CC_nsts2017 <- CCC.data %>% filter(WHWO_nest == 2017) %>%
  select(POINTID, TID, unhatched, slope:HSI_TB, log1ha2017, log1km2017) %>%
  rename(log_1ha = log1ha2017, log_1km = log1km2017)

CC_bkg2016 <- CCC.data %>% filter(TID %>% str_sub(1, 2) != "OC") %>%
  select(POINTID, TID, slope:HSI_TB, log1ha2016, log1km2016) %>%
  rename(log_1ha = log1ha2016, log_1km = log1km2016)
CC_bkg2016 <- CC_bkg2016[sample(nrow(CC_bkg2016), 10000, replace = F),]
CC_bkg2017 <- CCC.data %>% filter(TID %>% str_sub(1, 2) != "EF") %>%
  select(POINTID, TID, slope:HSI_TB, log1ha2017, log1km2017) %>%
  rename(log_1ha = log1ha2017, log_1km = log1km2017)
CC_bkg2017 <- CC_bkg2017[sample(nrow(CC_bkg2017), 10000, replace = F),]

names(CC_nsts2016)[-which(names(CC_nsts2016) %in% c("POINTID", "TID", "HSI_TB", "unhatched"))] <-
  names(CC_nsts2017)[-which(names(CC_nsts2017) %in% c("POINTID", "TID", "HSI_TB", "unhatched"))] <-
  names(CC_bkg2016)[-which(names(CC_bkg2016) %in% c("POINTID", "TID", "HSI_TB"))] <- 
  names(CC_bkg2017)[-which(names(CC_bkg2017) %in% c("POINTID", "TID", "HSI_TB"))] <-
  names(TB_bkg)[-which(names(TB_bkg) %in% c("Species", "X", "Y", "TID", "HSI_TB", "unhatched"))]

CC_nsts <- CC_nsts2016 %>% bind_rows(CC_nsts2017)
CC_bkg <- CC_bkg2016 %>% bind_rows(CC_bkg2017)
rm(CC_nsts2016, CC_nsts2017, CC_bkg2016, CC_bkg2017, CCC.data)

############################################################
# Presence-absence, remotely sensed & field-collected data #
############################################################

## Toolbox ##
TB_PAdata <- read.csv("WtLogReg/TB/WLR_data.csv", header = T, stringsAsFactors = F) %>%
  tbl_df

## Barry Point ##
BP_PAdata <- read.csv("WtLogReg/BP/WLR_data.csv", header = T, stringsAsFactors = F) %>%
  tbl_df

## Canyon Creek ##
CC_PAdata <- read.csv("WtLogReg/CC/WLR_data.csv", header = T, stringsAsFactors = F) %>%
  tbl_df

# Calc additional tree-level variables #
TB_PAdata <- TB_PAdata %>%
  mutate(PIPO = (Species == "PIPO") %>% as.numeric,
         Decay = (status == "decayed snag") %>% as.numeric)
BP_PAdata <- BP_PAdata %>%
  mutate(PIPO = (Species == "PIPO") %>% as.numeric,
         Decay = (status == "decayed snag") %>% as.numeric)
CC_PAdata <- CC_PAdata %>%
  mutate(PIPO = (Species == "PIPO") %>% as.numeric)

# Identify unhatched nests #
unhatched.nests <- c("TBORAA_NA1_2007", "TBOR08_NB1_2005", "CCORSG_NG01_2016", "CCORSG_NF01_2017",
                     "YV-A01")

save.image("HSI_validation_MS/Data_compiled.RData")

