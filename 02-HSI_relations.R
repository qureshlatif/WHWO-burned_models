library(R.utils)
#devtools::install_github("qureshlatif/WoodpeckerHSI") # Run this upon first use.
# Note: You might get some disconcerting warnings upon first install.
#To avoid them, restart R after installing package.
require(WoodpeckerHSI)
setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
load("HSI_validation_MS/Data_compiled.RData")
source("HSI_validation_MS/scripts/Maxent_HSI_functions.r")

###________________ Maxent relationships __________________###
# LocBrnOpn relationship #
brnopn_1ha <- seq(0.0, 100.0, length.out = 20)
brnopn_1km <- mean(c(mean(TB_bkg$brnopn_1km), mean(CC_bkg$brnopn_1km)))
dat.plot <- expand.grid(brnopn_1ha = brnopn_1ha, brnopn_1km = brnopn_1km)
dat.plot$HSI <- WHWO_burned(dat.plot$brnopn_1ha, dat.plot$brnopn_1km)$hsi

plt.LocBrnOpn <- ggplot(dat.plot, aes(brnopn_1ha, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab("Maxent HSI (pooled)") + xlab("LocBrnOpn") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# LandBrnOpn relationship #
brnopn_1km <- seq(10.4136110296275, 96.6265767087122, length.out = 20)
brnopn_1ha <- mean(c(mean(TB_bkg$brnopn_1ha), mean(CC_bkg$brnopn_1ha)))
dat.plot <- expand.grid(brnopn_1ha = brnopn_1ha, brnopn_1km = brnopn_1km)
dat.plot$HSI <- WHWO_burned(dat.plot$brnopn_1ha, dat.plot$brnopn_1km)$hsi

plt.LandBrnOpn <- ggplot(dat.plot, aes(brnopn_1km, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab("Maxent HSI (pooled)") + xlab("LandBrnOpn") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# LandPIPO relationship #
pipo_1km <- seq(38.0, 91.0, length.out = 20)
brnopn_1ha <- mean(TB_bkg$brnopn_1ha)
brnopn_1km <- mean(TB_bkg$brnopn_1km)
dat.plot <- expand.grid(pipo_1km = pipo_1km, brnopn_1ha = brnopn_1ha, brnopn_1km = brnopn_1km)
dat.plot$HSI <- Maxent_TB(dat.plot$brnopn_1ha, dat.plot$brnopn_1km, dat.plot$pipo_1km)$hsi

plt.LandPIPO <- ggplot(dat.plot, aes(pipo_1km, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab("Maxent HSI (Toolbox)") + xlab("LandPIPO") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# SLOPE relationship #
slope <- seq(0.0, 78.7753, length.out = 20)
brnopn_1ha <- mean(CC_bkg$brnopn_1ha)
brnopn_1km <- mean(CC_bkg$brnopn_1km)
dat.plot <- expand.grid(slope = slope, brnopn_1ha = brnopn_1ha, brnopn_1km = brnopn_1km)
dat.plot$HSI <- Maxent_CC(dat.plot$brnopn_1ha, dat.plot$brnopn_1km, dat.plot$slope)$hsi

plt.Slope <- ggplot(dat.plot, aes(slope, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab("Maxent HSI (Canyon Creek)") + xlab("Slope") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# Put it all together #

theme_set(theme_bw())
p <- ggdraw() + 
  draw_plot(plt.LocBrnOpn, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(plt.LandBrnOpn, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(plt.LandPIPO, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(plt.Slope, x = 0.5, y = 0, width = 0.5, height = 0.5)
#p

save_plot("HSI_validation_MS/Figure_Maxent_relations.tiff", p, ncol = 3, nrow = 3, dpi = 200)


###________________________________________________________###


###________________ WLR relationships __________________###
mod <- loadObject("WtLogReg/WLRtop_TB&CC")
ref.table <- TB_PAdata %>% filter(Nest == 0) %>%
  select(brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc) %>%
  mutate(Location = "TB") %>%
  bind_rows(CC_PAdata %>% filter(Nest == 0) %>%
              select(brnopn_1ha, brnopn_1km, Tree_ovr25, PIPO_perc) %>%
              mutate(Location = "CC"))

# LocBrnOpn relationship #
brnopn_1ha <- seq(min(ref.table$brnopn_1ha), max(ref.table$brnopn_1ha), length.out = 20)
dat.plot <- expand.grid(brnopn_1ha = brnopn_1ha, brnopn_1km = ref.table$brnopn_1km %>% mean,
                        Tree_ovr25 = ref.table$Tree_ovr25 %>% mean, PIPO_perc = ref.table$PIPO_perc %>% mean)
dat.plot$HSI <- predict(mod, dat.plot, type = "response")

plt.LocBrnOpn <- ggplot(dat.plot, aes(brnopn_1ha, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab(NULL) + xlab("LocBrnOpn") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# LandBrnOpn relationship #
brnopn_1km <- seq(min(ref.table$brnopn_1km), max(ref.table$brnopn_1km), length.out = 20)
dat.plot <- expand.grid(brnopn_1ha = ref.table$brnopn_1ha %>% mean, brnopn_1km = brnopn_1km,
                        Tree_ovr25 = ref.table$Tree_ovr25 %>% mean, PIPO_perc = ref.table$PIPO_perc %>% mean)
dat.plot$HSI <- predict(mod, dat.plot, type = "response")

plt.LandBrnOpn <- ggplot(dat.plot, aes(brnopn_1km, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab(NULL) + xlab("LandBrnOpn") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# Tree_ovr25 relationship #
Tree_ovr25 <- seq(min(ref.table$Tree_ovr25), max(ref.table$Tree_ovr25), length.out = 20)
dat.plot <- expand.grid(brnopn_1ha = ref.table$brnopn_1ha %>% mean, brnopn_1km = ref.table$brnopn_1km %>% mean,
                        Tree_ovr25 = Tree_ovr25, PIPO_perc = ref.table$PIPO_perc %>% mean)
dat.plot$HSI <- predict(mod, dat.plot, type = "response")

plt.Tree_ovr25 <- ggplot(dat.plot, aes(Tree_ovr25, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab(NULL) + xlab("Tree_ovr25") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# PIPO_perc relationship #
PIPO_perc <- seq(min(ref.table$PIPO_perc), max(ref.table$PIPO_perc), length.out = 20)
dat.plot <- expand.grid(brnopn_1ha = ref.table$brnopn_1ha %>% mean, brnopn_1km = ref.table$brnopn_1km %>% mean,
                        Tree_ovr25 = ref.table$Tree_ovr25 %>% mean, PIPO_perc = PIPO_perc)
dat.plot$HSI <- predict(mod, dat.plot, type = "response")

plt.PIPO_perc <- ggplot(dat.plot, aes(PIPO_perc, HSI)) +
  geom_line(size = 3) +
  ylim(0,1) +
  ylab(NULL) + xlab("PIPO_perc") +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30))

# Put it all together #

theme_set(theme_bw())
p <- ggdraw() + 
  draw_plot(plt.LocBrnOpn, x = 0.05, y = 0.5, width = 0.475, height = 0.5) +
  draw_plot(plt.LandBrnOpn, x = 0.525, y = 0.5, width = 0.475, height = 0.5) +
  draw_plot(plt.Tree_ovr25, x = 0.05, y = 0, width = 0.475, height = 0.5) +
  draw_plot(plt.PIPO_perc, x = 0.525, y = 0, width = 0.475, height = 0.5) +
  draw_plot_label(label = "WLR HSI", size = 35, x = 0, y = 0.4,
                  angle = 90)
#p

save_plot("HSI_validation_MS/Figure_WLR_relations.tiff", p, ncol = 3, nrow = 3, dpi = 200)


###________________________________________________________###
