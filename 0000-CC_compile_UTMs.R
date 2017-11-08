require(dplyr)
VegHead <- read.table("F:/research stuff/FS_PostDoc/data/Data_for_databases/CCC/exported/Veg_Header_Info.txt",
                      stringsAsFactors = F, header = T, sep = ",") %>% tbl_df %>%
  select(Point_ID, UTM_E, UTM_N)

write.table(VegHead, "E:/GISData/WHWO/brn_frst_project/CanCrk/NestRandUTM.txt", row.names = F, sep = ",")
