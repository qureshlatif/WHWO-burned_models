setwd("F:/research stuff/FS_PostDoc/WHWO/burn_forest_modeling/")
library(dplyr)
library(foreign)
library(stringr)

# Remote-sensed variables
Dat.remote <- read.dbf("E:/GISData/WHWO/brn_frst_project/Toolbox/WHWO_nst_and_rand.dbf",as.is=T) %>%
  tbl_df %>%
  mutate(brnopn_1ha = (brnopn_1ha/9)*100) %>%
  mutate(brnopn_1km = (brnopn_1km/3409)*100)
ind <- (grepl("log", names(Dat.remote)) & grepl("_1ha", names(Dat.remote))) %>%
  which
Dat.remote[, ind] <- (Dat.remote[, ind]/9)*100 #Rescale local-scale logging variables
ind <- (grepl("log", names(Dat.remote)) & grepl("_1km", names(Dat.remote))) %>%
  which
Dat.remote[, ind] <- (Dat.remote[, ind]/3409)*100 #Rescale landscape-scale logging variables
rm(ind)

Dat.remote <- Dat.remote[-which(Dat.remote$Unit %in% c("AA","DD","GG","II","EE")), ] #Remove nests from units with no random points.
Nest <- as.numeric(Dat.remote$Spp=="WHWO")
Dat.remote$Unit[which(nchar(Dat.remote$Unit)==1&!is.na(as.numeric(Dat.remote$Unit)))] <-
  paste("0",Dat.remote$Unit[which(nchar(Dat.remote$Unit)==1&!is.na(as.numeric(Dat.remote$Unit)))],sep="")
Dat.remote$Unit <- paste("TBOR",Dat.remote$Unit,sep="")

pnts <- which(Nest==0)
Dat.remote$ID[pnts][which(nchar(Dat.remote$ID[pnts])==1)] <-
  paste("00",Dat.remote$ID[pnts][which(nchar(Dat.remote$ID[pnts])==1)],sep="")
Dat.remote$ID[pnts][which(nchar(Dat.remote$ID[pnts])==2)] <-
  paste("0",Dat.remote$ID[pnts][which(nchar(Dat.remote$ID[pnts])==2)],sep="")
sort(unique(Dat.remote$ID[pnts]),na.last=T)

nsts03 <- which(Nest==1&Dat.remote$Year==2003)
nsts04 <- which(Nest==1&Dat.remote$Year!=2003)
nsts <- which(Nest==1)
for(i in 30:33) Dat.remote$ID[i] <- strsplit(Dat.remote$ID[i],"-")[[1]][2]
sort(unique(Dat.remote$ID[c(nsts03,nsts04)]),na.last=T)

Point_ID <- character(length=nrow(Dat.remote))
Point_ID[pnts] <- paste(Dat.remote$Unit[pnts],"_",Dat.remote$ID[pnts],sep="")
Point_ID[nsts] <- paste(Dat.remote$Unit[nsts],"_N",Dat.remote$ID[nsts],"_",Dat.remote$Year[nsts],sep="")

Dat.remote <- Dat.remote %>% tbl_df %>%
  mutate(Point_ID = Point_ID, Nest = Nest) %>%
  select(Point_ID, TID, Nest, slope, cosasp, brnopn_1ha:log07_1km)

#No. trees & snags
Dat.nest <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Nest_data.txt",
                       header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  filter(str_sub(Nest_ID, 1, 4) == "TBOR") %>%
  select(Nest_ID, Species) %>%
  filter(Species == "WHWO") %>%
  filter(!str_sub(Nest_ID, 5, 6) %in% c("AA","DD","GG","II","EE"))

Dat.head <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Veg01A_header_info.txt",
                       header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  filter(str_sub(Point_ID, 1, 4) == "TBOR") %>%
  mutate(Nest = regexpr("_N", Point_ID) %>% as.numeric != -1) %>%
  filter(!(Nest & !(Point_ID %in% Dat.nest$Nest_ID)))

#sort(unique(Dat.remote$Point_ID[which(!is.element(Dat.remote$Point_ID,Dat.head$Point_ID))]))
#sort(unique(Dat.head$Point_ID[which(!is.element(Dat.head$Point_ID,Dat.remote$Point_ID))]))

Dat.sng <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Veg13_snags_ovr_23.txt",
                      header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  filter(Measurement_ID %in% Dat.head$Measurement_ID)
Dat.tre <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Veg11_live_trees_over_23.txt",
                      header=T, stringsAsFactors=F, sep=",") %>%
  filter(Measurement_ID %in% Dat.head$Measurement_ID)

Dat.head$PIPO_perc <- Dat.head$Tree_ovr25 <- Dat.head$Snag_ovr50 <- Dat.head$Snag_25_50 <- numeric(nrow(Dat.head))
for(i in 1:nrow(Dat.head)) {
  sngs <- Dat.sng[which(Dat.sng$Measurement_ID==Dat.head$Measurement_ID[i]),]
  Dat.head$Snag_25_50[i] <- length(which(sngs$DBH>=25&sngs$DBH<50))
  Dat.head$Snag_ovr50[i] <- length(which(sngs$DBH>=50))
  trs <- Dat.tre[which(Dat.tre$Measurement_ID==Dat.head$Measurement_ID[i]),]
  Dat.head$Tree_ovr25[i] <- length(which(trs$DBH>25))
  Dat.head$PIPO_perc[i] <- ((length(which(trs$DBH>=25&trs$Tree_species=="PIPO"))+
                               length(which(sngs$DBH>=25&sngs$Snag_species=="PIPO")))/
                              (length(which(trs$DBH>=25))+length(which(sngs$DBH>=25))))*100
}

# Center tree or snag data #
Dat.nst <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Nest_site_habitat.txt",
                      header=T, stringsAsFactors=F, sep=",")  %>% tbl_df %>%
  filter(Nest_ID %in% Dat.remote$Point_ID) %>%
  select(Nest_ID, Tree_or_Snag, Tree_sp, Decay_class, Tree_ht, DBH) %>%
  mutate(live = (Tree_or_Snag == "T") %>% as.numeric)
  
Dat.cnt <- Dat.nst %>% select(-Tree_or_Snag) %>%
  mutate(status = "tree")
Dat.cnt <- Dat.cnt %>%
  mutate(status = replace(status, which(Dat.cnt$live==0&Dat.cnt$Decay_class==1), "sound snag")) %>%
  mutate(status = replace(status, which(Dat.cnt$live==0&Dat.cnt$Decay_class!=1), "decayed snag"))
Dat.cnt <- Dat.cnt %>% select(-Decay_class) %>% select(-live) %>%
  rename(Measurement_ID = Nest_ID, Species = Tree_sp, Height = Tree_ht)

Dat.sng <- Dat.sng %>% mutate(status = "sound snag") %>%
  mutate(status = replace(status, which(Dat.sng$Decay_class!=1), "decayed snag"))
Dat.tre <- Dat.tre %>% mutate(status = "tree")

Dat.Csng <- Dat.sng[which(Dat.sng$Center_snag=="Y"),]
Dat.Ctre <- Dat.tre[which(Dat.tre$Center_tree=="Y"),]
MID.rnd <- Dat.head$Measurement_ID[which(Dat.head$Nest==F)]
for(i in 1:length(MID.rnd)) {
  ctre <- Dat.Ctre[which(Dat.Ctre$Measurement_ID==MID.rnd[i]),c("Measurement_ID","Tree_species","Height","DBH","status")]
  csng <- Dat.Csng[which(Dat.Csng$Measurement_ID==MID.rnd[i]),c("Measurement_ID","Snag_species","Height","DBH","status")]
  names(ctre) <- names(csng) <- names(Dat.cnt)
  add <- rbind(ctre,csng)
  if(nrow(add)==1&add$DBH[1]>=25) Dat.cnt <- rbind(Dat.cnt,add)
  if(nrow(add)!=1|add$DBH[1]<25) {
    trees <- Dat.tre[which(Dat.tre$Measurement_ID==MID.rnd[i]&Dat.tre$DBH>=25),
                     c("Measurement_ID","Tree_species","Height","DBH","status")]
    snags <- Dat.sng[which(Dat.sng$Measurement_ID==MID.rnd[i]&Dat.sng$DBH>=25),
                     c("Measurement_ID","Snag_species","Height","DBH","status")]
    names(trees) <- names(snags) <- names(Dat.cnt)
    both <- rbind(trees,snags)
    Dat.cnt <- rbind(Dat.cnt,both[sample(nrow(both),1),])
  }
}

Dat.head$Species <- Dat.head$status <- ""
Dat.head$Height <- Dat.head$DBH <- NA
for(i in 1:nrow(Dat.head)) {
  Dat.head$Species[i] <- Dat.cnt$Species[which(Dat.cnt$Measurement_ID==Dat.head$Measurement_ID[i])]
  Dat.head$Height[i] <- Dat.cnt$Height[which(Dat.cnt$Measurement_ID==Dat.head$Measurement_ID[i])]
  Dat.head$DBH[i] <- Dat.cnt$DBH[which(Dat.cnt$Measurement_ID==Dat.head$Measurement_ID[i])]
  Dat.head$status[i] <- Dat.cnt$status[which(Dat.cnt$Measurement_ID==Dat.head$Measurement_ID[i])]  
}

#Rescale snag/tree counts to no./ha
Dat.head[,c("Snag_25_50","Snag_ovr50")] <- Dat.head[,c("Snag_25_50","Snag_ovr50")]/0.4
Dat.head[,c("Tree_ovr25")] <- Dat.head[,c("Tree_ovr25")]/0.12

#Logging intensity
Dat.stmps <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/BBN/from_database/Veg14_stumps.txt",
                        header=T, stringsAsFactors=F, sep=",") %>% tbl_df %>%
  filter(Measurement_ID %in% Dat.head$Measurement_ID)

Dat.head$CutStumps <- NA
for(i in 1:nrow(Dat.head)) Dat.head$CutStumps[i] <-
  sum(Dat.stmps$Cut_tally_totl[which(Dat.stmps$Measurement_ID==Dat.head$Measurement_ID[i])])
Dat.head$CutStumps <- Dat.head$CutStumps/0.04
Dat.head$logging <- Dat.head$CutStumps/(Dat.head$Snag_25_50+Dat.head$Snag_ovr50+Dat.head$Tree_ovr25+Dat.head$CutStumps)

#Timing of logging
Dat.remote$log_year <- 9999
for(i in 1:nrow(Dat.remote)) {
  log.levels <- as.numeric(unique(Dat.remote[i,] %>% select(starts_with("log")) %>% select(ends_with("1ha"))))
  log.change <- log.levels[2:5]-log.levels[1:4]
  if(any(log.levels>0)&length(unique(log.levels))>1)
    Dat.remote$log_year[i] <- c(2004,2005,2006,2007)[which(log.change>0)]
  if(any(log.levels>0)&length(unique(log.levels))==1)
    Dat.remote$log_year[i] <- 2003
}
length(which(Dat.remote$log_year==9999&(Dat.remote$log03_1ha+Dat.remote$log04_1ha+Dat.remote$log05_1ha+
                                          Dat.remote$log06_1ha+Dat.remote$log07_1ha)>0)) #Should be none.

Dat.final <- cbind(Dat.remote,matrix(NA,nrow(Dat.remote),10),stringsAsFactors=F)
names(Dat.final)[(ncol(Dat.final) - 9):ncol(Dat.final)] <- names(Dat.head %>% select(Snag_25_50:logging))
Dat.final$status <- Dat.final$Species <- ""
Dat.error <- Dat.final

Dat.final$FC_year <- 0
Dat.final$wt <- 0
Dat.final$wt[which(Dat.final$Nest==1)] <- sum(Dat.final$Nest==0)/sum(Dat.final$Nest==1)
Dat.final$wt[which(Dat.final$Nest==0)] <- 1
rmv <- numeric(length=nrow(Dat.final))
Dat.add <- data.frame()
for(i in 1:nrow(Dat.final)) {
  obs <- which(Dat.head$Point_ID==Dat.final$Point_ID[i])
  if(length(obs)==1) {
    Dat.final[i, which(names(Dat.final) == "Snag_25_50"):which(names(Dat.final) == "logging")] <-
      Dat.head[obs, ] %>% select(Snag_25_50:logging)
    Dat.final[i,"FC_year"] <- substr(Dat.head[obs,"Measurement_ID"],(nchar(Dat.head[obs,"Measurement_ID"])-3),
                                   nchar(Dat.head[obs,"Measurement_ID"]))}
  if(length(obs)>1) {
    add <- rbind(Dat.final[i,],Dat.final[i,])
    FC.dat <- Dat.head[obs,]
    add[, which(names(Dat.final) == "Snag_25_50"):which(names(Dat.final) == "logging")] <-
      FC.dat %>% select(Snag_25_50:logging)
    for(j in 1:nrow(add)) {
      add[j,"wt"] <- add[j,"wt"]/nrow(add)
      add[j,"FC_year"] <- as.numeric(substr(FC.dat[j,"Measurement_ID"],nchar(FC.dat[j,"Measurement_ID"])-3,
                                            nchar(FC.dat[j,"Measurement_ID"])))
    }
    Dat.add <- rbind(Dat.add,add)
    rmv[i] <- 1
  }
}
Dat.final <- Dat.final[-which(rmv==1),]
rm(rmv)
Dat.final <- rbind(Dat.final,Dat.add)
rm(Dat.add)

Dat.final$log_1km <- Dat.final$log_1ha <- 0
for(i in which(Dat.final$Nest==1)) {
  yr <- as.numeric(substr(Dat.final$Point_ID[i],nchar(Dat.final$Point_ID[i])-3,nchar(Dat.final$Point_ID[i])))
  if(yr==2003) Dat.final$log_1ha[i] <- Dat.final$log03_1ha[i]; Dat.final$log_1km[i] <- Dat.final$log03_1km[i]
  if(yr==2004) Dat.final$log_1ha[i] <- Dat.final$log04_1ha[i]; Dat.final$log_1km[i] <- Dat.final$log04_1km[i]
  if(yr==2005) Dat.final$log_1ha[i] <- Dat.final$log05_1ha[i]; Dat.final$log_1km[i] <- Dat.final$log05_1km[i]
  if(yr==2006) Dat.final$log_1ha[i] <- Dat.final$log06_1ha[i]; Dat.final$log_1km[i] <- Dat.final$log06_1km[i]
  if(yr==2007) Dat.final$log_1ha[i] <- Dat.final$log07_1ha[i]; Dat.final$log_1km[i] <- Dat.final$log07_1km[i]
}
Dat.add <- data.frame()
for(i in which(Dat.final$Nest==0)) {
  add <- rbind(Dat.final[i,],Dat.final[i,],Dat.final[i,],Dat.final[i,],Dat.final[i,])
  if(nrow(add)==5) {
    add$wt <- add$wt/5
    add[,"log_1ha"] <- c(add[1,"log03_1ha"],add[1,"log04_1ha"],add[1,"log05_1ha"],add[1,"log06_1ha"],add[1,"log07_1ha"])
    add[,"log_1km"] <- c(add[1,"log03_1km"],add[1,"log04_1km"],add[1,"log05_1km"],add[1,"log06_1km"],add[1,"log07_1km"])
    if(any(add$logging>0)&add$FC_year[1]>2003&sum(add$log_1km==max(add$log_1km))<5&sum(add$log_1ha==max(add$log_1ha))<5)
      add$logging[1:min(c((as.numeric(add$FC_year[1])-2003),max(which(add$log_1km<max(add$log_1km))),
                          max(which(add$log_1ha<max(add$log_1ha)))))] <- 0
    if(any(add$logging>0)&add$FC_year[1]>2003&sum(add$log_1km==max(add$log_1km))<5&sum(add$log_1ha==max(add$log_1ha))==5)
      add$logging[1:min(c((as.numeric(add$FC_year[1])-2003),max(which(add$log_1km<max(add$log_1km)))))] <- 0
    Dat.add <- rbind(Dat.add,add)
  }
  if(nrow(add)==10) {
    add$wt <- add$wt/10
    add[,"log_1ha"] <- c(add[1,"log03_1ha"],add[1,"log03_1ha"],add[1,"log04_1ha"],add[1,"log04_1ha"],
                         add[1,"log05_1ha"],add[1,"log05_1ha"],add[1,"log06_1ha"],add[1,"log06_1ha"],
                         add[1,"log07_1ha"],add[1,"log07_1ha"])
    add[,"log_1km"] <- c(add[1,"log03_1km"],add[1,"log03_1km"],add[1,"log04_1km"],add[1,"log04_1km"],
                         add[1,"log05_1km"],add[1,"log05_1km"],add[1,"log06_1km"],add[1,"log06_1km"],
                         add[1,"log07_1km"],add[1,"log07_1km"])
    if(any(add$logging>0)&add$FC_year[1]>2003&sum(add$log_1km==max(add$log_1km))<10&sum(add$log_1ha==max(add$log_1ha))<10)
      add$logging[1:min(c((as.numeric(add$FC_year[1])-2003)*2,max(which(add$log_1km<max(add$log_1km))),
                          max(which(add$log_1ha<max(add$log_1ha)))))] <- 0
    if(any(add$logging>0)&add$FC_year[1]>2003&sum(add$log_1km==max(add$log_1km))<10&sum(add$log_1ha==max(add$log_1ha))==10)
      add$logging[1:min(c((as.numeric(add$FC_year[1])-2003)*2,max(which(add$log_1km<max(add$log_1km)))))] <- 0
    Dat.add <- rbind(Dat.add,add)
  }
}
Dat.final <- Dat.final[-which(Dat.final$Nest==0),]
Dat.final <- rbind(Dat.final,Dat.add)
rm(Dat.add)

Dat.final <- Dat.final %>% tbl_df %>%
  select(Point_ID:pipo_1km, Snag_25_50:logging, log_1ha, log_1km, wt)

write.csv(Dat.final,"WtLogReg/TB/WLR_data.csv",row.names=F)
