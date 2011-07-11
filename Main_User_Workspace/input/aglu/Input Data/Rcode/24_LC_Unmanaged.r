#Calculate initial estimates of shrubland, unmanaged pasture, and unmanaged forest
#shrubland and grassland are taken directly from SAGE minus HYDE and WDPA
LC_bm2_R_Shrub_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Shrubland",]
write.table(LC_bm2_R_Shrub_Yh_AEZ,file="Rdata_out/LC_bm2_R_Shrub_Yh_AEZ_24.csv",sep=",",col.names=TRUE,row.names=FALSE)

LC_bm2_R_Grass_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Grassland",]
write.table(LC_bm2_R_Grass_Yh_AEZ,file="Rdata_out/LC_bm2_R_Grass_Yh_AEZ_24.csv",sep=",",col.names=TRUE,row.names=FALSE)

#unmanaged pasture is equal to total pasture from Hyde minus managed pasture
LC_bm2_R_Pasture_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Pasture",]
LC_bm2_R_UnMgdPasture_Yh_AEZ<-cbind(LC_bm2_R_Pasture_Yh_AEZ[,1:3],LC_bm2_R_Pasture_Yh_AEZ[,4:21] - LC_bm2_R_MgdPasture_Yh_AEZ[,4:21])
LC_bm2_R_UnMgdPasture_Yh_AEZ$Land_Type<-"UnmanagedPasture"

write.table(LC_bm2_R_UnMgdPasture_Yh_AEZ,file="Rdata_out/LC_bm2_R_UnMgdPasture_Yh_AEZ_24.csv",sep=",",col.names=TRUE,row.names=FALSE)

#unmanaged forest is equal to total forest from Hyde minus managed forest
LC_bm2_R_Forest_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Forest" & LC_bm2_R_LT_Yh_AEZ$year!=2000,]
LC_bm2_R_UnMgdForest_Yh_AEZ<-cbind(LC_bm2_R_Forest_Yh_AEZ[,1:3],LC_bm2_R_Forest_Yh_AEZ[,4:21] - LC_bm2_R_MgdForest_Yh_AEZ[,4:21])
LC_bm2_R_UnMgdForest_Yh_AEZ$Land_Type<-"UnmanagedForest"

write.table(LC_bm2_R_UnMgdForest_Yh_AEZ,file="Rdata_out/LC_bm2_R_UnMgdForest_Yh_AEZ_24.csv",sep=",",col.names=TRUE,row.names=FALSE)

#These four all have land deducted to cover the "extra" cropland that came from setting a maximum harvested:cropped ratio.
#This deduction takes place according to the relative shares of land cover.
#Calculate these shares
LC_All_Unmanaged_R_Yh_AEZ<-cbind(LC_bm2_R_Shrub_Yh_AEZ[,1:3],LC_bm2_R_Shrub_Yh_AEZ[,4:21] + LC_bm2_R_Grass_Yh_AEZ[,4:21] + LC_bm2_R_UnMgdPasture_Yh_AEZ[,4:21]+LC_bm2_R_UnMgdForest_Yh_AEZ[,4:21])
LC_All_Unmanaged_R_Yh_AEZ$Land_Type<-"All_Unmanaged"
LC_ShrubShare_R_Yh_AEZ<-cbind(LC_bm2_R_Shrub_Yh_AEZ[,1:3],LC_bm2_R_Shrub_Yh_AEZ[,4:21] / (LC_All_Unmanaged_R_Yh_AEZ[,4:21]+1e-9))
LC_GrassShare_R_Yh_AEZ<-cbind(LC_bm2_R_Grass_Yh_AEZ[,1:3],LC_bm2_R_Grass_Yh_AEZ[,4:21] / (LC_All_Unmanaged_R_Yh_AEZ[,4:21]+1e-9))
LC_UmMgdPastureShare_R_Yh_AEZ<-cbind(LC_bm2_R_UnMgdPasture_Yh_AEZ[,1:3],LC_bm2_R_UnMgdPasture_Yh_AEZ[,4:21] / (LC_All_Unmanaged_R_Yh_AEZ[,4:21]+1e-9))
LC_UmMgdForestShare_R_Yh_AEZ<-cbind(LC_bm2_R_UnMgdForest_Yh_AEZ[,1:3],LC_bm2_R_UnMgdForest_Yh_AEZ[,4:21] / (LC_All_Unmanaged_R_Yh_AEZ[,4:21]+1e-9))

#Calculate deduction as "extra" cropland multiplied by these shares
LC_ShrubtoCrop_R_Yh_AEZ<-cbind(LC_ShrubShare_R_Yh_AEZ[,1:3],LC_ShrubShare_R_Yh_AEZ[,4:21] * LC_bm2_R_ExtraCropLand_Yh_AEZ[,4:21])
LC_GrasstoCrop_R_Yh_AEZ<-cbind(LC_GrassShare_R_Yh_AEZ[,1:3],LC_GrassShare_R_Yh_AEZ[,4:21] * LC_bm2_R_ExtraCropLand_Yh_AEZ[,4:21])
LC_UnMgdPasturetoCrop_R_Yh_AEZ<-cbind(LC_UmMgdPastureShare_R_Yh_AEZ[,1:3],LC_UmMgdPastureShare_R_Yh_AEZ[,4:21] * LC_bm2_R_ExtraCropLand_Yh_AEZ[,4:21])
LC_UnMgdForesttoCrop_R_Yh_AEZ<-cbind(LC_UmMgdForestShare_R_Yh_AEZ[,1:3],LC_UmMgdForestShare_R_Yh_AEZ[,4:21] * LC_bm2_R_ExtraCropLand_Yh_AEZ[,4:21])

#Adjusted land cover totals = original minus the land switched to cropland
LC_bm2_R_Shrub_Yh_AEZ_adj<-cbind(LC_bm2_R_Shrub_Yh_AEZ[,1:3],LC_bm2_R_Shrub_Yh_AEZ[,4:21] - LC_ShrubtoCrop_R_Yh_AEZ[,4:21])
LC_bm2_R_Grass_Yh_AEZ_adj<-cbind(LC_bm2_R_Grass_Yh_AEZ[,1:3],LC_bm2_R_Grass_Yh_AEZ[,4:21] - LC_GrasstoCrop_R_Yh_AEZ[,4:21])
LC_bm2_R_UnMgdPasture_Yh_AEZ_adj<-cbind(LC_bm2_R_UnMgdPasture_Yh_AEZ[,1:3],LC_bm2_R_UnMgdPasture_Yh_AEZ[,4:21] - LC_UnMgdPasturetoCrop_R_Yh_AEZ[,4:21])
LC_bm2_R_UnMgdForest_Yh_AEZ_adj<-cbind(LC_bm2_R_UnMgdForest_Yh_AEZ[,1:3],LC_bm2_R_UnMgdForest_Yh_AEZ[,4:21] - LC_UnMgdForesttoCrop_R_Yh_AEZ[,4:21])

#add identifier vectors
LC_bm2_R_Shrub_Yh_AEZ_adj$ID_R_LT_Y<-paste(LC_bm2_R_Shrub_Yh_AEZ_adj$GCAM_region_ID,LC_bm2_R_Shrub_Yh_AEZ_adj$Land_Type,LC_bm2_R_Shrub_Yh_AEZ_adj$year,sep="")
LC_bm2_R_Grass_Yh_AEZ_adj$ID_R_LT_Y<-paste(LC_bm2_R_Grass_Yh_AEZ_adj$GCAM_region_ID,LC_bm2_R_Grass_Yh_AEZ_adj$Land_Type,LC_bm2_R_Grass_Yh_AEZ_adj$year,sep="")
LC_bm2_R_UnMgdPasture_Yh_AEZ_adj$ID_R_LT_Y<-paste(LC_bm2_R_UnMgdPasture_Yh_AEZ_adj$GCAM_region_ID,LC_bm2_R_UnMgdPasture_Yh_AEZ_adj$Land_Type,LC_bm2_R_UnMgdPasture_Yh_AEZ_adj$year,sep="")
LC_bm2_R_UnMgdForest_Yh_AEZ_adj$ID_R_LT_Y<-paste(LC_bm2_R_UnMgdForest_Yh_AEZ_adj$GCAM_region_ID,LC_bm2_R_UnMgdForest_Yh_AEZ_adj$Land_Type,LC_bm2_R_UnMgdForest_Yh_AEZ_adj$year,sep="")

write.table(LC_bm2_R_Shrub_Yh_AEZ_adj,file="Rdata_out/LC_bm2_R_Shrub_Yh_AEZ_adj_24.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_Grass_Yh_AEZ_adj,file="Rdata_out/LC_bm2_R_Grass_Yh_AEZ_adj_24.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_UnMgdPasture_Yh_AEZ_adj,file="Rdata_out/LC_bm2_R_UnMgdPasture_Yh_AEZ_adj_24.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_UnMgdForest_Yh_AEZ_adj,file="Rdata_out/LC_bm2_R_UnMgdForest_Yh_AEZ_adj_24.csv",sep=",",col.names=TRUE,row.names=FALSE)

print(ifelse(any(LC_bm2_R_Shrub_Yh_AEZ_adj[,4:21]<0|LC_bm2_R_Grass_Yh_AEZ_adj[,4:21]<0|LC_bm2_R_UnMgdPasture_Yh_AEZ_adj[,4:21]<0|LC_bm2_R_UnMgdForest_Yh_AEZ_adj[,4:21]<0),"Increase in cropland exceeds available unmanaged land","Increase in cropland balanced by deduction from unmanaged land"))
