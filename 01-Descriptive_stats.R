setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")
require(dplyr)
library(R.utils)
library(stringr)

# Toolbox #
TB_sum <- TB_PAdata %>%
  mutate(NEST = "nest") %>%
  mutate(NEST = replace(NEST, which(Nest == 0), "non-nest")) %>%
  select(NEST, slope:pipo_1km, log_1ha, log_1km, Snag_25_50:PIPO_perc, PIPO, Decay, DBH, logging) %>%
  group_by(NEST) %>%
  summarise_all(funs(mean, sd))
MN <- TB_sum %>%
  select(-NEST) %>%
  select(ends_with("mean")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(MN) <- c("nest", "non-nest")
SD <- TB_sum %>%
  select(-NEST) %>%
  select(ends_with("sd")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(SD) <- c("nest", "non-nest")

SumTab <- cbind(paste0(MN[,"nest"], "(", SD[, "nest"], ")"),
      paste0(MN[,"non-nest"], "(", SD[, "non-nest"], ")"))
dimnames(SumTab) <- list(str_sub(dimnames(MN)[[1]], 1, -6), c("TB_nest", "TB_non-nest"))

# Canyon Creek #
CC_sum <- CC_PAdata %>%
  mutate(NEST = "nest") %>%
  mutate(NEST = replace(NEST, which(Nest == 0), "non-nest")) %>%
  mutate(Decay = 9) %>%
  select(NEST, slope:pipo_1km, log_1ha, log_1km, Snag_25_50:PIPO_perc, PIPO, Decay, DBH, logging) %>%
  group_by(NEST) %>%
  summarise_all(funs(mean, sd))
MN <- CC_sum %>%
  select(-NEST) %>%
  select(ends_with("mean")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(MN) <- c("nest", "non-nest")
SD <- CC_sum %>%
  select(-NEST) %>%
  select(ends_with("sd")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(SD) <- c("nest", "non-nest")

SumTab <- cbind(SumTab, paste0(MN[,"nest"], "(", SD[, "nest"], ")"),
                paste0(MN[,"non-nest"], "(", SD[, "non-nest"], ")"))
dimnames(SumTab)[[2]][3:4] <- c("CC_nest", "CC_non-nest")

# Barry Point #
BP_sum <- BP_PAdata %>%
  mutate(NEST = "nest") %>%
  mutate(NEST = replace(NEST, which(Nest == 0), "non-nest")) %>%
  mutate(log_1ha = 0, log_1km = 0, logging = 0) %>%
  select(NEST, slope:cosasp, brnopn_1ha:pipo_1km, log_1ha, log_1km,
         Snag_25_50:Snag_ovr50, Tree_ovr25, PIPO_perc, PIPO, Decay, DBH, logging) %>%
  group_by(NEST) %>%
  summarise_all(funs(mean, sd))
MN <- BP_sum %>%
  select(-NEST) %>%
  select(ends_with("mean")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(MN) <- c("nest", "non-nest")
SD <- BP_sum %>%
  select(-NEST) %>%
  select(ends_with("sd")) %>%
  as.matrix %>% t %>%
  apply(c(1, 2),
        function(x) ifelse(x<1, round(x, digits = 2), round(x, digits = 1)))
colnames(SD) <- c("nest", "non-nest")

SumTab <- cbind(SumTab, paste0(MN[,"nest"], "(", SD[, "nest"], ")"),
                paste0(MN[,"non-nest"], "(", SD[, "non-nest"], ")"))
dimnames(SumTab)[[2]][5:6] <- c("BP_nest", "BP_non-nest")

write.csv(SumTab, "Descriptives.csv", row.names = T)
