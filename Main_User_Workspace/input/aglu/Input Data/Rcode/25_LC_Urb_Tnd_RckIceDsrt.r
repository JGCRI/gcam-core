
#Urban land: read from Hyde
LC_bm2_R_UrbanLand_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Urbanland",]
LC_bm2_R_UrbanLand_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_UrbanLand_Yh_AEZ$GCAM_region_ID,LC_bm2_R_UrbanLand_Yh_AEZ$Land_Type,LC_bm2_R_UrbanLand_Yh_AEZ$year,sep="")
write.table(LC_bm2_R_UrbanLand_Yh_AEZ,file="Rdata_out/LC_bm2_R_UrbanLand_Yh_AEZ_25.csv",sep=",",col.names=TRUE,row.names=FALSE)

#subset Tundra and RockIceDesert.
LC_bm2_R_Tundra_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Tundra",]
LC_bm2_R_Tundra_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_Tundra_Yh_AEZ$GCAM_region_ID,LC_bm2_R_Tundra_Yh_AEZ$Land_Type,LC_bm2_R_Tundra_Yh_AEZ$year,sep="")
write.table(LC_bm2_R_Tundra_Yh_AEZ,file="Rdata_out/LC_bm2_R_Tundra_Yh_AEZ_25.csv",sep=",",col.names=TRUE,row.names=FALSE)

LC_bm2_R_RckIceDsrt_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="RockIceDesert",]
LC_bm2_R_RckIceDsrt_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_RckIceDsrt_Yh_AEZ$GCAM_region_ID,LC_bm2_R_RckIceDsrt_Yh_AEZ$Land_Type,LC_bm2_R_RckIceDsrt_Yh_AEZ$year,sep="")
write.table(LC_bm2_R_RckIceDsrt_Yh_AEZ,file="Rdata_out/LC_bm2_R_RckIceDsrt_Yh_AEZ_25.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Protected lands
#NOTE: If protected lands are included, uncomment the text below 
#LC_bm2_R_Protected_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Protected"&LC_bm2_R_LT_Yh_AEZ$year!=2000,]
#LC_bm2_R_Protected_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_Protected_Yh_AEZ$GCAM_region_ID,LC_bm2_R_Protected_Yh_AEZ$Land_Type,LC_bm2_R_Protected_Yh_AEZ$year,sep="")
#write.table(LC_bm2_R_Protected_Yh_AEZ,file="Rdata_out/LC_bm2_R_Protected_Yh_AEZ_25.csv",sep=",",col.names=TRUE,row.names=FALSE)









