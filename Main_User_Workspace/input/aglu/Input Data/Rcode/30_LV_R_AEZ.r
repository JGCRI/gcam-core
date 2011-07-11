#import data from CSV files
ctry87_reg_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/ctry87_reg_GTAP.csv",header=T,sep=',')
value_milUSD_GTAP<-read.table("Inventory Data/GTAP/Rdata_in/value_milUSD_GTAP.csv",header=T,sep=',')
GTAP_use_agg<-read.table("Inventory Data/GTAP/Rdata_in/GTAP_use_agg.csv",header=T,sep=',')

#Calculate total value of each AEZ (not including animal production uses)
#Add identifier vectors for GCAM regions and aggregated GTAP uses
value_milUSD_GTAP$GCAM_region_ID<-ctry87_reg_GTAP$GCAM_region_ID[match(value_milUSD_GTAP$ctry87,ctry87_reg_GTAP$ctry87)]
value_milUSD_GTAP$GTAP_use_agg<-GTAP_use_agg$GTAP_use_agg[match(value_milUSD_GTAP$GTAP_use,GTAP_use_agg$GTAP_use)]
LV_milUSD_R_C_AEZ<-value_milUSD_GTAP[value_milUSD_GTAP$GTAP_use_agg!="na",]
LV_milUSD_R_AEZ<-aggregate(value_milUSD_GTAP[,3:20],list(GCAM_region_ID = value_milUSD_GTAP$GCAM_region_ID),sum)

#Convert 2001$ to 1975$ (multiply by 0.3711)
conv_2001_1975_USD<-0.3711
LV_milUSD75_R_AEZ<-LV_milUSD_R_AEZ
LV_milUSD75_R_AEZ[,2:19]<-LV_milUSD_R_AEZ[,2:19]*conv_2001_1975_USD

#Subset total crop and managed forest land in each region and AEZ
LC_bm2_R_HarvCropLand_2005_AEZ<-LC_bm2_R_HarvCropLand_Yh_AEZ[LC_bm2_R_HarvCropLand_Yh_AEZ$year==2005,]
LC_bm2_R_MgdForest_2005_AEZ<-LC_bm2_R_MgdForest_Yh_AEZ[LC_bm2_R_MgdForest_Yh_AEZ$year==2005,]
LC_bm2_R_HarvCropMgdFor_2005_AEZ<-LC_bm2_R_HarvCropLand_2005_AEZ
LC_bm2_R_HarvCropMgdFor_2005_AEZ[,4:21]<-LC_bm2_R_HarvCropLand_2005_AEZ[,4:21] + LC_bm2_R_MgdForest_2005_AEZ[,4:21]

#Calculate land value as economic output divided by land cover
LV_USD75_m2_R_AEZ<-LV_milUSD75_R_AEZ
LV_USD75_m2_R_AEZ[,2:19]<-LV_milUSD75_R_AEZ[,2:19] * 0.001 / LC_bm2_R_HarvCropMgdFor_2005_AEZ[,4:21]
LV_USD75_m2_R_AEZ[is.na(LV_USD75_m2_R_AEZ)]<-0
LV_USD75_m2_R_AEZ[,2:19]<-apply(LV_USD75_m2_R_AEZ[,2:19],2,function(x){x[x=="Inf"]=0;x})

write.table(LV_USD75_m2_R_AEZ,file="Rdata_out/LV_USD75_m2_R_AEZ_30.csv",sep=",",col.names=TRUE,row.names=FALSE)
