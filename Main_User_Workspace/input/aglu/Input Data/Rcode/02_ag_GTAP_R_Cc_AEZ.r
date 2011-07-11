#import data from CSV files
ctry_reg_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/ctry_reg_GTAP.csv",header=T,sep=',')
ag_crop_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/ag_crop_GTAP.csv",header=T,sep=',')
ag_HA_Ha_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/ag_HA_Ha_GTAP.csv",header=T,sep=',')
LC_Ha_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/LC_Ha_GTAP.csv",header=T,sep=',')
ag_Prod_t_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/ag_Prod_t_GTAP.csv",header=T,sep=',')

#add lookup vectors to each of the tables
LC_Ha_GTAP$GCAM_region_ID<-ctry_reg_GTAP$GCAM_region_ID[match(LC_Ha_GTAP$ctry,ctry_reg_GTAP$ctry)]
ag_HA_Ha_GTAP$GCAM_region_ID<-ctry_reg_GTAP$GCAM_region_ID[match(ag_HA_Ha_GTAP$ctry,ctry_reg_GTAP$ctry)]
ag_Prod_t_GTAP$GCAM_region_ID<-ctry_reg_GTAP$GCAM_region_ID[match(ag_Prod_t_GTAP$ctry,ctry_reg_GTAP$ctry)]
ag_HA_Ha_GTAP$GCAM_commodity<-ag_crop_GTAP$GCAM_commodity[match(ag_HA_Ha_GTAP$GTAP_crop,ag_crop_GTAP$GTAP_crop)]
ag_Prod_t_GTAP$GCAM_commodity<-ag_crop_GTAP$GCAM_commodity[match(ag_Prod_t_GTAP$GTAP_crop,ag_crop_GTAP$GTAP_crop)]

#build tables collapsed by GCAM regions and crop names
LC_Ha_R_AEZ<-aggregate(LC_Ha_GTAP[,3:20],by=list(LC_Ha_GTAP$GCAM_region_ID,LC_Ha_GTAP$landtype),FUN=sum)
names(LC_Ha_R_AEZ)[names(LC_Ha_R_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(LC_Ha_R_AEZ)[names(LC_Ha_R_AEZ)=="Group.2"]<-"landtype"
ag_HA_Ha_R_Cc_AEZ<-aggregate(ag_HA_Ha_GTAP[,3:20],by=list(ag_HA_Ha_GTAP$GCAM_region_ID,ag_HA_Ha_GTAP$GCAM_commodity),FUN=sum)
names(ag_HA_Ha_R_Cc_AEZ)[names(ag_HA_Ha_R_Cc_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(ag_HA_Ha_R_Cc_AEZ)[names(ag_HA_Ha_R_Cc_AEZ)=="Group.2"]<-"GCAM_commodity"
ag_HA_Ha_R_Cc_AEZ<-ag_HA_Ha_R_Cc_AEZ[ag_HA_Ha_R_Cc_AEZ$GCAM_commodity!="na",]
ag_Prod_t_R_Cc_AEZ<-aggregate(ag_Prod_t_GTAP[,3:20],by=list(ag_Prod_t_GTAP$GCAM_region_ID,ag_Prod_t_GTAP$GCAM_commodity),FUN=sum)
names(ag_Prod_t_R_Cc_AEZ)[names(ag_Prod_t_R_Cc_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(ag_Prod_t_R_Cc_AEZ)[names(ag_Prod_t_R_Cc_AEZ)=="Group.2"]<-"GCAM_commodity"
ag_Prod_t_R_Cc_AEZ<-ag_Prod_t_R_Cc_AEZ[ag_Prod_t_R_Cc_AEZ$GCAM_commodity!="na",]

#convert to desired units (Mt and bm2)
LC_bm2_R_AEZ<-cbind(LC_Ha_R_AEZ[,1:2],LC_Ha_R_AEZ[3:20]/100000)
ag_HA_bm2_R_Cc_AEZ<-cbind(ag_HA_Ha_R_Cc_AEZ[,1:2],ag_HA_Ha_R_Cc_AEZ[3:20]/100000)
ag_Prod_Mt_R_Cc_AEZ<-cbind(ag_Prod_t_R_Cc_AEZ[,1:2],ag_Prod_t_R_Cc_AEZ[3:20]/1000000)

#export final tables as CSV files
write.table(ag_HA_bm2_R_Cc_AEZ,file="Rdata_out/ag_HA_bm2_R_Cc_AEZ_02.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_AEZ,file="Rdata_out/LC_bm2_R_AEZ_02.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Prod_Mt_R_Cc_AEZ,file="Rdata_out/ag_Prod_Mt_R_Cc_AEZ_02.csv",sep=",",col.names=TRUE,row.names=FALSE)

