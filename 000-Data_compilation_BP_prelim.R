setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/WtLogReg/BP/")
library(dplyr)

#Remote-sensed variables
Dat.remote <- read.csv("WLR_data_remote.csv", header=T, stringsAsFactors=F) %>% tbl_df
Dat.all <- Dat.remote %>%
  arrange(PID)

#No. trees & snags
Dat.TRSN <- read.table("Barry_Point_trees&snags.txt", header=T, stringsAsFactors=F, sep=",") %>%
  tbl_df
Dat.TRSN[is.na(Dat.TRSN)] <- 0
Dat.TRSN <- Dat.TRSN[,-(which(apply(Dat.TRSN[,-1],2,sum)==0)+1)] #Remove columns with all zero counts.
#Dat.TRSN$Snag_10_25 <- apply(Dat.TRSN[,c("Snags10.25_Meas1_ABCO","Snags10.25_Meas1_ABGR","Snags10.25_Meas1_CADE27",
#    "Snags10.25_Meas1_CELEI","Snags10.25_Meas1_JUOC","Snags10.25_Meas1_PIPO","Snags10.25_Meas1_UNKN")],1,sum) # Overall snag density 10-25 cm dbh
#Dat.TRSN$Snag_ovr25 <- apply(Dat.TRSN[,c("Snags25.50_Meas1_ABCO","Snags25.50_Meas1_ABGR","Snags25.50_Meas1_CADE27",
#    "Snags25.50_Meas1_CELEI","Snags25.50_Meas1_JUOC","Snags25.50_Meas1_PIPO","Snags50_Meas1_ABCO","Snags50_Meas1_ABGR",
#    "Snags50_Meas1_CADE27","Snags50_Meas1_JUOC","Snags50_Meas1_PIPO")],1,sum) # Overall snag density over 25 cm dbh
#Dat.TRSN$Snag_25_50 <- apply(Dat.TRSN[,c("Snags25.50_Meas1_ABCO","Snags25.50_Meas1_ABGR","Snags25.50_Meas1_CADE27",
#    "Snags25.50_Meas1_CELEI","Snags25.50_Meas1_JUOC","Snags25.50_Meas1_PIPO")],1,sum) # Overall snag density 25-50 cm dbh
#Dat.TRSN$Snag_ovr50 <- apply(Dat.TRSN[,c("Snags50_Meas1_ABCO","Snags50_Meas1_ABGR","Snags50_Meas1_CADE27","Snags50_Meas1_JUOC",
#    "Snags50_Meas1_PIPO")],1,sum) # Overall snag density over 50 cm dbh
#Dat.TRSN$Tree_10_25 <- apply(Dat.TRSN[,c("Trees10.25_Meas1_ABCO","Trees10.25_Meas1_ABGR","Trees10.25_Meas1_CADE27",
#    "Trees10.25_Meas1_JUOC","Trees10.25_Meas1_PIPO")],1,sum) # Overall tree density 10-25 cm dbh
#Dat.TRSN$Tree_ovr25 <- apply(Dat.TRSN[,c("Trees25.50_Meas1_ABCO","Trees25.50_Meas1_ABGR","Trees25.50_Meas1_CADE27",
#    "Trees25.50_Meas1_JUOC","Trees25.50_Meas1_PIPO","Trees50_Meas1_ABCO","Trees50_Meas1_ABGR","Trees50_Meas1_CADE27",
#    "Trees50_Meas1_JUOC","Trees50_Meas1_PIPO")],1,sum) # Overall tree density over 25 cm dbh
#Dat.TRSN$Tree_25_50 <- apply(Dat.TRSN[,c("Trees25.50_Meas1_ABCO","Trees25.50_Meas1_ABGR","Trees25.50_Meas1_CADE27",
#    "Trees25.50_Meas1_JUOC","Trees25.50_Meas1_PIPO")],1,sum) # Overall tree density over 25 cm dbh
#Dat.TRSN$Tree_ovr50 <- apply(Dat.TRSN[,c("Trees50_Meas1_ABCO","Trees50_Meas1_ABGR","Trees50_Meas1_CADE27",
#    "Trees50_Meas1_JUOC","Trees50_Meas1_PIPO")],1,sum) # Overall tree density over 25 cm dbh
##***Notes on correlations: Snags 10-25 and 25-50 were correlated (r~0.7) and all live tree densities were
      #correlated, so all live tree categories were combined and snags were broken into two groups: 10-50 & >50.

Dat.TRSN$Snag_10_25 <- apply(Dat.TRSN[,c("Snags10.25_Meas1_ABCO","Snags10.25_Meas1_ABGR",
    "Snags10.25_Meas1_CADE27","Snags10.25_Meas1_CELEI","Snags10.25_Meas1_JUOC","Snags10.25_Meas1_PIPO",
    "Snags10.25_Meas1_UNKN","Snags25.50_Meas1_ABCO","Snags25.50_Meas1_ABGR","Snags25.50_Meas1_CADE27",
    "Snags25.50_Meas1_CELEI","Snags25.50_Meas1_JUOC","Snags25.50_Meas1_PIPO")],1,sum) # Overall snag density 10-25 cm dbh
Dat.TRSN$Snag_25_50 <- apply(Dat.TRSN[,c("Snags25.50_Meas1_ABCO","Snags25.50_Meas1_ABGR","Snags25.50_Meas1_CADE27",
    "Snags25.50_Meas1_CELEI","Snags25.50_Meas1_JUOC","Snags25.50_Meas1_PIPO")],1,sum) # Overall snag density 10-25 cm dbh
Dat.TRSN$Snag_ovr50 <- apply(Dat.TRSN[,c("Snags50_Meas1_ABCO","Snags50_Meas1_ABGR","Snags50_Meas1_CADE27","Snags50_Meas1_JUOC",
    "Snags50_Meas1_PIPO")],1,sum) # Overall snag density over 50 cm dbh
Dat.TRSN$Tree_ovr10 <- apply(Dat.TRSN[,c("Trees10.25_Meas1_ABCO","Trees10.25_Meas1_ABGR","Trees10.25_Meas1_CADE27",
    "Trees10.25_Meas1_JUOC","Trees10.25_Meas1_PIPO","Trees25.50_Meas1_ABCO","Trees25.50_Meas1_ABGR","Trees25.50_Meas1_CADE27",
    "Trees25.50_Meas1_JUOC","Trees25.50_Meas1_PIPO","Trees50_Meas1_ABCO","Trees50_Meas1_ABGR","Trees50_Meas1_CADE27",
    "Trees50_Meas1_JUOC","Trees50_Meas1_PIPO")],1,sum) # Overall tree density over 10 cm dbh
Dat.TRSN$Tree_ovr25 <- apply(Dat.TRSN[,c("Trees25.50_Meas1_ABCO","Trees25.50_Meas1_ABGR","Trees25.50_Meas1_CADE27",
    "Trees25.50_Meas1_JUOC","Trees25.50_Meas1_PIPO","Trees50_Meas1_ABCO","Trees50_Meas1_ABGR","Trees50_Meas1_CADE27",
    "Trees50_Meas1_JUOC","Trees50_Meas1_PIPO")],1,sum) # Overall tree density over 10 cm dbh
Dat.TRSN$PIPO_perc <- (apply(Dat.TRSN[,c("Trees25.50_Meas1_PIPO","Trees50_Meas1_PIPO",
  "Snags25.50_Meas1_PIPO","Snags50_Meas1_PIPO")],1,sum)/
  (Dat.TRSN$Tree_ovr25 + Dat.TRSN$Snag_25_50 + Dat.TRSN$Snag_ovr50))*100
Dat.TRSN <- Dat.TRSN %>%
  select(Point_ID, Snag_10_25:PIPO_perc) %>%
  mutate(Point_ID = str_sub(Point_ID, 6, -1))
ind.nest <- which(substr(Dat.TRSN$Point_ID,1,1)=="N")
Dat.TRSN$Point_ID[ind.nest] <- substr(Dat.TRSN$Point_ID[ind.nest],2,(nchar(Dat.TRSN$Point_ID[ind.nest])-5))
Dat.TRSN <- Dat.TRSN[order(Dat.TRSN$Point_ID),]

Dat.all <- Dat.all[order(Dat.all$PID),]
Dat.TRSN <- Dat.TRSN[order(Dat.TRSN$Point_ID),]
Dat.all <- cbind(Dat.all,Dat.TRSN[,-1])

# Center tree or snag data #
Dat.nst <- read.table("Birds_nest_locations.txt", header=T, stringsAsFactors=F, sep=",") %>%
  tbl_df %>%
  filter(str_sub(Nest_ID, 1, 4) == "EMBP" & Species == "WHWO") %>%
  select(Nest_ID, Tree_Snag_Log:DBH, Decay_class) %>%
  mutate(live = (Tree_Snag_Log == "T") %>% as.numeric)
Dat.cnt <- Dat.nst %>% select(-Tree_Snag_Log) %>%
  mutate(status = "tree")
Dat.cnt <- Dat.cnt %>%
  mutate(status = replace(status, which(Dat.cnt$live==0&Dat.cnt$Decay_class==1), "sound snag")) %>%
  mutate(status = replace(status, which(Dat.cnt$live==0&Dat.cnt$Decay_class!=1), "decayed snag"))
Dat.cnt <- Dat.cnt %>% select(-Decay_class) %>% select(-live) %>%
  rename(Measurement_ID = Nest_ID, Species = Tree_sp, Height = Tree_ht)

#### Alternative 1: random sample of trees within patch ####
#Dat.tre <- read.table("Veg02_Live_trees_over_25.txt",header=T,stringsAsFactors=F,sep=",")
#Dat.tre <- Dat.tre[which(substr(Dat.tre$Measurement_ID,1,4)=="EMBP"),]
#Dat.tre <- Dat.tre[-which(Dat.tre$Tree_species=="NONE"),]
#Dat.tre <- Dat.tre[-which(is.element(Dat.tre$Measurement_ID,Dat.nst$Nest_ID)),]
#Dat.tre <- Dat.tre[,c(1,4,7,6)]
#Dat.tre$status <- "tree"
#Dat.sng <- read.table("Veg04_Snags.txt",header=T,stringsAsFactors=F,sep=",")
#Dat.sng <- Dat.sng[which(substr(Dat.sng$Measurement_ID,1,4)=="EMBP"),]
#Dat.sng <- Dat.sng[-which(Dat.sng$DBH<25),]
#Dat.sng <- Dat.sng[-which(Dat.sng$Snag_species=="NONE"),]
#Dat.sng <- Dat.sng[-which(is.element(Dat.sng$Measurement_ID,Dat.nst$Nest_ID)),]
#Dat.sng <- Dat.sng[,c(1,4,7,6,5)]
#Dat.sng$status <- "sound snag"
#Dat.sng$status[which(Dat.sng$Decay_class!=1)] <- "decayed snag"
#Dat.sng <- Dat.sng[,-5]
#names(Dat.sng) <- names(Dat.tre)
#names(Dat.sng)[2] <- names(Dat.tre)[2] <- "Species"

#MID.rnd <- sort(unique(c(Dat.sng$Measurement_ID,Dat.tre$Measurement_ID)))
##for(i in 1:length(MID.rnd)) { #Snags or trees
##  trees <- Dat.tre[which(Dat.tre$Measurement_ID==MID.rnd[i]),]
##  snags <- Dat.sng[which(Dat.sng$Measurement_ID==MID.rnd[i]),]
##  both <- rbind(trees,snags)
##  names(both) <- names(Dat.cnt)
##  Dat.cnt <- rbind(Dat.cnt,both[sample(nrow(both),1),])
##}
#for(i in 1:length(MID.rnd)) {#Snags only
#  snags <- Dat.sng[which(Dat.sng$Measurement_ID==MID.rnd[i]),]
#  names(snags) <- names(Dat.cnt)
#  Dat.cnt <- rbind(Dat.cnt,snags[sample(nrow(snags),1),])
#}
############################################################

#### Alternative 2: center tree/snag if DBH > 25 ####
Dat.tre <- read.table("Veg02_Live_trees_over_25.txt",header=T,stringsAsFactors=F,sep=",")
Dat.tre <- Dat.tre[which(substr(Dat.tre$Measurement_ID,1,4)=="EMBP"),]
Dat.tre <- Dat.tre[-which(Dat.tre$Tree_species=="NONE"),]
Dat.tre <- Dat.tre[-which(is.element(Dat.tre$Measurement_ID,Dat.nst$Nest_ID)),]
Dat.tre <- Dat.tre[,c(1,11,4,7,6)]
Dat.tre$status <- "tree"
Dat.rnd <- Dat.tre[which(Dat.tre$Center_tree=="Y"),-2]

Dat.sng <- read.table("Veg04_Snags.txt",header=T,stringsAsFactors=F,sep=",")
Dat.sng <- Dat.sng[which(substr(Dat.sng$Measurement_ID,1,4)=="EMBP"),]
Dat.sng <- Dat.sng[-which(is.element(Dat.sng$Measurement_ID,Dat.nst$Nest_ID)),]
Dat.sng <- Dat.sng[-which(Dat.sng$Snag_species=="NONE"),]
Dat.sng <- Dat.sng[,c(1,8,4,7,6,5)]
Dat.sng$status <- "sound snag"
Dat.sng$status[which(Dat.sng$Decay_class!=1)] <- "decayed snag"
Dat.sng <- Dat.sng[,-6]
names(Dat.sng)[-2] <- names(Dat.tre)[-2] <- names(Dat.rnd)
names(Dat.sng)[3] <- names(Dat.tre)[3] <- names(Dat.rnd)[2] <- "Species"
names(Dat.sng)[2] <- names(Dat.tre)[2] <- "Center"

Dat.rnd <- rbind(Dat.rnd,Dat.sng[which(Dat.sng$Center=="Y"),-2])
Dat.rnd <- Dat.rnd[-which(is.element(Dat.rnd$Measurement_ID,c("EMBP_LHC08_2013","EMBP_YV01_2013"))),]
                #Remove random points with no large (DBH > 25) snags (only live trees or small snags) in plot.

repl.needed <- which(Dat.rnd$DBH<25|Dat.rnd$status=="tree")
for(i in repl.needed) {
  mid <- Dat.rnd$Measurement_ID[i]
  pool <- Dat.sng[which(Dat.sng$Measurement_ID==mid),]
  pool <- pool[-which(pool$DBH<25),]
  Dat.rnd[i,] <- pool[sample(nrow(pool),1),-2]
}
#repl.needed <- which(Dat.rnd$DBH<25) #Snags or trees
#for(i in repl.needed) {
#  mid <- Dat.rnd$Measurement_ID[i]
#  pool <- rbind(Dat.tre[which(Dat.tre$Measurement_ID==mid),],Dat.sng[which(Dat.sng$Measurement_ID==mid),])
#  pool <- pool[-which(pool$DBH<25),]
#  Dat.rnd[i,] <- pool[sample(nrow(pool),1),-2]
#}

Dat.cnt <- rbind(Dat.cnt,Dat.rnd)

##############################################################
names(Dat.cnt)[1] <- "ID"
Dat.cnt$ID <- substr(Dat.cnt$ID,6,nchar(Dat.cnt$ID))
ind.nest <- which(substr(Dat.cnt$ID,1,1)=="N")
Dat.cnt$ID[ind.nest] <- substr(Dat.cnt$ID[ind.nest],2,nchar(Dat.cnt$ID[ind.nest]))
rm(ind.nest)
Dat.cnt$ID <- substr(Dat.cnt$ID,1,(nchar(Dat.cnt$ID)-5))
Dat.cnt <- Dat.cnt[order(Dat.cnt$ID),]

Dat.rmt <- Dat.all[,-c(15:20)]
write.csv(Dat.rmt,"WLR_data_remote.csv",row.names=F)

Dat.cmb <- Dat.all[-which(is.element(Dat.all$PID,c("LHC08","YV01"))),]#Remove random points with no large (DBH > 25)
                                                        #snags (only live trees or small snags) in plot (n = 2).

Dat.cmb <- cbind(Dat.cmb,Dat.cnt[,-1])

#Rescale snag/tree counts to no./ha
Dat.cmb[,c(15:19)] <- Dat.cmb[,c(15:19)]/0.4

write.csv(Dat.cmb,"WLR_data.csv",row.names=F)
