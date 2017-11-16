setwd("F:/research stuff/FS_PostDoc/data/Data_for_databases/CCC/exported/")
library(dplyr)
library(foreign)
library(stringr)

### Remote-sensed data ###
Dat.remote <- read.dbf("E:/GISData/WHWO/brn_frst_project/CanCrk/FieldMeasPnts.dbf",as.is=T) %>%
  tbl_df %>%
  mutate(pipo_1km = (pipo_1km/3409)*100) %>%
  mutate(log_1ha = log_1ha * 100) %>%
  mutate(log_1km = log_1km * 100) %>%
  mutate(log1ha2016 = log1ha2016 * 100) %>%
  mutate(log1km2016 = log1km2016 * 100) %>%
  rename(log1ha2017 = log_1ha, log1km2017 = log_1km) %>%
  select(-TID) %>%
  rename(TID = BSunits)

### Field-collected record IDs ###
Dat.nest <- read.table("CCC_birds_nest_locations.txt",
                      stringsAsFactors = F, header = T, sep = ",") %>% tbl_df %>%
  select(Nest_ID, Species, Year_located, Tree_sp, Tree_ht, DBH, Slope, Aspect, Tree_Snag_Log)
randIDs <- read.table("CCC_guide_stations.txt",
                      stringsAsFactors = F, header = T, sep = ",") %>% tbl_df %>%
  mutate(Station_ID =
           Station_ID %>% str_sub(1, 1) %>%
           str_c(Station_ID %>% str_sub(2) %>%
                   str_pad(width = 3, side = "left", pad = "0"))) %>%
  mutate(Point_ID = str_c("CCOR", Unit_ID, "_", Station_ID))
VegHead <- read.table("Veg_Header_Info.txt",
                      stringsAsFactors = F, header = T, sep = ",") %>% tbl_df %>%
  mutate(Nest = Point_ID %in% Dat.nest$Nest_ID %>% as.numeric)
ind.rand <- which(VegHead$Nest == 0)
VegHead[ind.rand, ] <- VegHead[ind.rand, ] %>%
  mutate(Point_ID = Point_ID %>% str_sub(1, 8) %>%
           str_c(Point_ID %>% str_sub(9) %>%
                   str_pad(width = 3, side = "left", pad = "0"))) %>%
  mutate(Measurement_ID = Point_ID %>% str_c(Measurement_ID %>% str_sub(-5, -1)))
Dat.nest <- Dat.nest %>% filter(Species == "WHWO")
VegHead <- VegHead %>% #***3 randoms are not in guide station table***#
  filter(Point_ID %in%
           Dat.nest$Nest_ID | Point_ID %in% randIDs$Point_ID |
           Point_ID %in% c("CCORUF_D148","CCORUF_D149","CCORUF_E064"))
rm(ind.rand, randIDs)

Dat.remote <- Dat.remote %>% # Pad IDs in remote-sensed data to match VegHead
  mutate(Nest = Point_ID %in% Dat.nest$Nest_ID %>% as.numeric)
Dat.remote[which(Dat.remote$Nest == 0), ] <- Dat.remote[which(Dat.remote$Nest == 0), ] %>%
  mutate(Point_ID = Point_ID %>% str_sub(1, 8) %>%
           str_c(Point_ID %>% str_sub(9) %>%
                   str_pad(width = 3, side = "left", pad = "0")))
Dat.remote <- Dat.remote %>%
  filter(Point_ID %in% VegHead$Point_ID)
#sort(unique(Dat.remote$Point_ID[which(!Dat.remote$Point_ID %in% VegHead$Point_ID)])) # Should be 0
#sort(unique(VegHead$Point_ID[which(!is.element(VegHead$Point_ID,Dat.remote$Point_ID))])) # Should be 0

### Merge remotely sensed with Field records ###
Dat.final <- VegHead %>% mutate(Year = str_sub(Measurement_date, 1, 4)) %>%
  left_join(Dat.remote, by = "Point_ID") %>%
  select(Measurement_ID, Point_ID, TID, Year, UTM_E.x, UTM_N.x, Nest.x, slope:log1km2016) %>%
  rename(UTM_E = UTM_E.x, UTM_N = UTM_N.x, Nest = Nest.x) %>%
  filter(!(Nest == 0 & D2WHWO < 35)) %>%
  select(-D2WHWO) %>%
  mutate(log_1ha = log1ha2017) %>%
  mutate(log_1km = log1km2017)
Dat.final[which(Dat.final$Year == "2016"),] <- Dat.final[which(Dat.final$Year == "2016"),] %>%
  mutate(log_1ha = log1ha2016) %>%
  mutate(log_1km = log1km2016)
Dat.final <- Dat.final %>%
  select(-Year) %>%
  select(Measurement_ID:pipo_1km, log_1ha, log_1km)
 
### Field collected variable compilation ###
# Snag data #
Dat.sng <- read.table("Veg04_Snags_Ovr_23.txt",
                      header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  select(Measurement_ID, Snag_species, DBH, Center_snag)
ind.rand <- which(!Dat.sng$Measurement_ID %in% Dat.final$Measurement_ID[which(Dat.final$Nest == 1)])
Dat.sng[ind.rand, ] <- Dat.sng[ind.rand, ] %>%
  mutate(Measurement_ID = Measurement_ID %>% str_sub(1, 8) %>%
           str_c(Measurement_ID %>% str_sub(9, -6) %>%
                   str_pad(width = 3, side = "left", pad = "0"),
                 Measurement_ID %>% str_sub(-5)))
Dat.sng <- Dat.sng %>% filter(Measurement_ID %in% Dat.final$Measurement_ID) %>%
  filter(!Snag_species == "NONE")

Snag.append <- read.table("Veg99_Snags_LIDAR.txt",
                          header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  select(Measurement_ID, Snag_species, DBH, Center_snag) %>%
  filter(DBH >= 23) %>%
  filter(Measurement_ID %in% Dat.final$Measurement_ID)
Dat.final <- Dat.final %>% mutate(Plot_MS = 1200)
Dat.final$Plot_MS[which(Dat.final$Measurement_ID %in% Snag.append$Measurement_ID)] <- 4000
Dat.sng <- Dat.sng %>% bind_rows(Snag.append)
rm(Snag.append)

#sum(Dat.sng$DBH < 23) # Should be 0.
#sort(unique(Dat.sng$DBH))
#sort(unique(Dat.sng$Snag_species))

SnagSum <- Dat.sng %>% group_by(Measurement_ID) %>%
  summarise(Snag_25_50 = sum(DBH >=25 & DBH < 50), Snag_ovr50 = sum(DBH >= 50))

Dat.final <- Dat.final %>% left_join(SnagSum, by  = "Measurement_ID") %>%
  mutate(Snag_25_50 = (Snag_25_50 / Plot_MS) * 10000) %>%
  mutate(Snag_25_50 = replace(Snag_25_50, is.na(Snag_25_50), 0)) %>%
  mutate(Snag_ovr50 = Snag_ovr50 / 0.4) %>%
  mutate(Snag_ovr50 = replace(Snag_ovr50, is.na(Snag_ovr50), 0))

# Tree data #
Dat.tre <- read.table("Veg02_Live_Trees_Over_23.txt",
                      header=T, stringsAsFactors=F, sep=",") %>% tbl_df
ind.rand <- which(!Dat.tre$Measurement_ID %in% Dat.final$Measurement_ID[which(Dat.final$Nest == 1)])
Dat.tre[ind.rand, ] <- Dat.tre[ind.rand, ] %>%
  mutate(Measurement_ID = Measurement_ID %>% str_sub(1, 8) %>%
           str_c(Measurement_ID %>% str_sub(9, -6) %>%
                   str_pad(width = 3, side = "left", pad = "0"),
                 Measurement_ID %>% str_sub(-5)))
Dat.tre <- Dat.tre %>% filter(Measurement_ID %in% Dat.final$Measurement_ID) %>%
  filter(!Tree_species == "NONE")

  # Impute missing DBH #
ind <- which(Dat.tre$DBH != 999 & Dat.tre$Tree_species == "PSME")
#plot(Dat.tre$DBH[ind], Dat.tre$Height[ind])
mod <- lm(DBH ~ Height, data = Dat.tre[ind,])
#summary(mod)
Dat.tre$DBH[which(Dat.tre$DBH == 999)] <-
  predict(mod, newdata = Dat.tre[which(Dat.tre$DBH == 999), ])
rm(mod, ind)
  #___________________#

#sum(is.na(Dat.tre$DBH)) # Should be 0.
#sum(Dat.tre$DBH < 23) # Should be 0.
#sort(unique(Dat.tre$DBH))
#sort(unique(Dat.tre$Tree_species))

TreeSum <- Dat.tre %>% group_by(Measurement_ID) %>%
  summarise(Tree_ovr25 = sum(DBH >=25))
Dat.final <- Dat.final %>% left_join(TreeSum, by  = "Measurement_ID") %>%
  mutate(Tree_ovr25 = Tree_ovr25 / 0.12) %>%
  mutate(Tree_ovr25 = replace(Tree_ovr25, is.na(Tree_ovr25), 0))

# Species composition #
Dat.sngtre <- Dat.sng %>% rename(Species = Snag_species, Center = Center_snag) %>%
  mutate(status = "snag") %>%
  bind_rows(Dat.tre %>%
              select(Measurement_ID, Tree_species, DBH, Center_tree) %>%
              rename(Species = Tree_species, Center = Center_tree) %>%
              mutate(status = "tree"))
SpecSum <- Dat.sngtre %>% group_by(Measurement_ID) %>%
  summarise(PIPO_perc = (sum(Species == "PIPO") / n()) * 100)
Dat.final <- Dat.final %>% left_join(SpecSum, by = "Measurement_ID")

# Center tree or snag data #
  # Nest trees
Dat.nest <- Dat.nest %>%
  mutate(status = (Tree_Snag_Log == "S") %>% as.numeric %>% as.character) %>%
  mutate(status = replace(status, which(status == "0"), "tree")) %>%
  mutate(status = replace(status, which(status == "1"), "snag")) %>%
  select(Nest_ID, Tree_sp, DBH, status) %>%
  rename(Species = Tree_sp, Measurement_ID = Nest_ID)
Dat.cntN <- Dat.sngtre %>% filter(Measurement_ID %in% Dat.nest$Measurement_ID) %>%
  filter(Center == "Y") %>% select(-Center)
Dat.cntN <- Dat.cntN %>% bind_rows(
    Dat.nest %>% filter(!Measurement_ID %in% Dat.cntN$Measurement_ID)
  )

  # Random trees #
Dat.cntR <- Dat.sngtre %>% filter(!Measurement_ID %in% Dat.nest$Measurement_ID) %>%
  filter(DBH >= 25 & status == "snag")
    # Fill in missing center trees with random from plot for 4 random sites #
CentY.sum <- Dat.cntR %>% group_by(Measurement_ID) %>%
  summarise(CY = any(Center == "Y"))
CentY.none <- CentY.sum$Measurement_ID[which(CentY.sum$CY == F)] # Length = 4 sites without indicated center trees
for(i in CentY.none) Dat.cntR$Center[which(Dat.cntR$Measurement_ID == i)][
    sample(sum(CentY.sum$Measurement_ID == i), 1)] <- "Y"
Dat.cntR <- Dat.cntR %>% filter(Center == "Y") %>% select(-Center)
Dat.cnt <- Dat.cntN %>% bind_rows(Dat.cntR)
rm(CentY.sum, Dat.cntR, Dat.cntN)

Dat.final <- Dat.final %>% left_join(Dat.cnt, by = "Measurement_ID") %>%
  filter(!is.na(Species))

# Logging #
Dat.stmps <- read.table("Veg05_Stumps.txt",
                        header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  filter(!Species == "NONE")
ind.rand <- which(!Dat.stmps$Measurement_ID %in% Dat.final$Measurement_ID[which(Dat.final$Nest == 1)])
Dat.stmps[ind.rand, ] <- Dat.stmps[ind.rand, ] %>%
  mutate(Measurement_ID = Measurement_ID %>% str_sub(1, 8) %>%
           str_c(Measurement_ID %>% str_sub(9, -6) %>%
                   str_pad(width = 3, side = "left", pad = "0"),
                 Measurement_ID %>% str_sub(-5)))
rm(ind.rand)
Dat.stmps <- Dat.stmps %>% filter(Measurement_ID %in% Dat.final$Measurement_ID) %>%
  filter(Diameter >= 15) %>%
  filter(!Comments %>% str_detect("BB") &
           !Comments %>% str_detect("BEFORE BURN") &
           !Comments %>% str_detect("before burn") &
           !Comments %>% str_detect("before fire") &
           !Comments %>% str_detect("CUT BEFORE") &
           !Comments %>% str_detect("cut before") &
           !Comments %>% str_detect("burnt") &
           !Comments %>% str_detect("BURNT") &
           !Comments %>% str_detect("Burnt") &
           !Comments %>% str_detect("Manmade (old)") &
           !Comments %>% str_detect("natural") &
           !Comments %>% str_detect("Natural") &
           !Comments %>% str_detect("netural") &
           !Comments %>% str_detect("burned") &
           !Comments == "Burned too far" &
           !Comments == "Cut before/during fire" &
           !Comments == "Manmade (old)" &
           !Comments %>% str_detect("BURNED"))
StmpSum <- Dat.stmps %>% group_by(Measurement_ID) %>%
  summarise(StumpDens = n() / 0.4)

Dat.final <- Dat.final %>% left_join(StmpSum, by = "Measurement_ID") %>%
  mutate(StumpDens = replace(StumpDens, is.na(StumpDens), 0)) %>%
  mutate(logging = StumpDens / (StumpDens + Tree_ovr25 + Snag_25_50 + Snag_ovr50)) %>%
  select(Measurement_ID:status, logging)

setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
write.csv(Dat.final,"WtLogReg/CC/WLR_data.csv",row.names=F)
