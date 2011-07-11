#Calculation of natural vegetation land cover for protected lands, croplands, and pasture
#PROTECTED LANDS
#Subset protected lands in 2005 from GIS table
#NOTE: If protected lands are included, uncomment the text block below
#LC_km2_Prot_2005<-LC_km2_GIS[LC_km2_GIS$LT_WDPA=="Protected" & LC_km2_GIS$Year==2005,]

#Aggregate by SAGE type. Non-existent combinations are not written out, indicated by LCi
#LCi_bm2_R_LTProt_AEZ<-aggregate(LC_km2_Prot_2005$Area_bm2,list(GCAM_region_ID=LC_km2_Prot_2005$GCAM_region_ID,Land_Type=LC_km2_Prot_2005$LT_SAGE,AEZ=LC_km2_Prot_2005$AEZ),sum)
#names(LCi_bm2_R_LTProt_AEZ)[names(LCi_bm2_R_LTProt_AEZ)=="x"]<-"Area_bm2"
#LCi_bm2_R_LTProt_AEZ$ID_R_LT<-paste(LCi_bm2_R_LTProt_AEZ$GCAM_region_ID,LCi_bm2_R_LTProt_AEZ$Land_Type,sep="")

#Create table with the SAGE land types represented, all region/LT combinations written out, and AEZs as columns
#LC_bm2_R_LTProt_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$year==2005 & LC_bm2_R_LT_Yh_AEZ$Land_Type %in% LCi_bm2_R_LTProt_AEZ$Land_Type,1:2]
#LC_bm2_R_LTProt_AEZ$ID_R_LT<-paste(LC_bm2_R_LTProt_AEZ$GCAM_region_ID,LC_bm2_R_LTProt_AEZ$Land_Type,sep="")

#Divide incomplete land cover table into separate tables, one for each AEZ
#LCi_bm2_R_LTProt_AEZ1<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==1,]
#LCi_bm2_R_LTProt_AEZ2<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==2,]
#LCi_bm2_R_LTProt_AEZ3<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==3,]
#LCi_bm2_R_LTProt_AEZ4<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==4,]
#LCi_bm2_R_LTProt_AEZ5<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==5,]
#LCi_bm2_R_LTProt_AEZ6<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==6,]
#LCi_bm2_R_LTProt_AEZ7<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==7,]
#LCi_bm2_R_LTProt_AEZ8<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==8,]
#LCi_bm2_R_LTProt_AEZ9<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==9,]
#LCi_bm2_R_LTProt_AEZ10<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==10,]
#LCi_bm2_R_LTProt_AEZ11<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==11,]
#LCi_bm2_R_LTProt_AEZ12<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==12,]
#LCi_bm2_R_LTProt_AEZ13<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==13,]
#LCi_bm2_R_LTProt_AEZ14<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==14,]
#LCi_bm2_R_LTProt_AEZ15<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==15,]
#LCi_bm2_R_LTProt_AEZ16<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==16,]
#LCi_bm2_R_LTProt_AEZ17<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==17,]
#LCi_bm2_R_LTProt_AEZ18<-LCi_bm2_R_LTProt_AEZ[LCi_bm2_R_LTProt_AEZ$AEZ==18,]

#Paste in land cover estimates
#LC_bm2_R_LTProt_AEZ$AEZ1<-LCi_bm2_R_LTProt_AEZ1$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ1$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ2<-LCi_bm2_R_LTProt_AEZ2$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ2$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ3<-LCi_bm2_R_LTProt_AEZ3$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ3$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ4<-LCi_bm2_R_LTProt_AEZ4$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ4$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ5<-LCi_bm2_R_LTProt_AEZ5$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ5$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ6<-LCi_bm2_R_LTProt_AEZ6$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ6$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ7<-LCi_bm2_R_LTProt_AEZ7$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ7$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ8<-LCi_bm2_R_LTProt_AEZ8$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ8$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ9<-LCi_bm2_R_LTProt_AEZ9$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ9$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ10<-LCi_bm2_R_LTProt_AEZ10$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ10$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ11<-LCi_bm2_R_LTProt_AEZ11$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ11$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ12<-LCi_bm2_R_LTProt_AEZ12$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ12$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ13<-LCi_bm2_R_LTProt_AEZ13$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ13$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ14<-LCi_bm2_R_LTProt_AEZ14$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ14$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ15<-LCi_bm2_R_LTProt_AEZ15$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ15$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ16<-LCi_bm2_R_LTProt_AEZ16$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ16$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ17<-LCi_bm2_R_LTProt_AEZ17$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ17$ID_R_LT)]
#LC_bm2_R_LTProt_AEZ$AEZ18<-LCi_bm2_R_LTProt_AEZ18$Area_bm2[match(LC_bm2_R_LTProt_AEZ$ID_R_LT,LCi_bm2_R_LTProt_AEZ18$ID_R_LT)]

#Sort, move ID vector to last column, and replace NA's with 0
#LC_bm2_R_LTProt_AEZ<-LC_bm2_R_LTProt_AEZ[order(LC_bm2_R_LTProt_AEZ$GCAM_region_ID,LC_bm2_R_LTProt_AEZ$Land_Type),]
#LC_bm2_R_LTProt_AEZ<-LC_bm2_R_LTProt_AEZ[,c(1:2,4:21,3)]
#LC_bm2_R_LTProt_AEZ[is.na(LC_bm2_R_LTProt_AEZ)]<-0

#Write this table out
#write.table(LC_bm2_R_LTProt_AEZ,file="Rdata_out/LC_bm2_R_LTProt_AEZ_27.csv",sep=",",col.names=TRUE,row.names=FALSE)

#CROPLANDS
#Subset croplands in 2005 from GIS table (note that this step does not consider any changes to cropland cover from the GIS data)
LC_km2_Crop_2005<-LC_km2_GIS[LC_km2_GIS$LT_HYDE=="Cropland" & LC_km2_GIS$Year==2005,]

#Aggregate by SAGE type. Non-existent combinations are not written out, indicated by LCi
LCi_bm2_R_LTCrop_AEZ<-aggregate(LC_km2_Crop_2005$Area_bm2,list(GCAM_region_ID=LC_km2_Crop_2005$GCAM_region_ID,Land_Type=LC_km2_Crop_2005$LT_SAGE,AEZ=LC_km2_Crop_2005$AEZ),sum)
names(LCi_bm2_R_LTCrop_AEZ)[names(LCi_bm2_R_LTCrop_AEZ)=="x"]<-"Area_bm2"
LCi_bm2_R_LTCrop_AEZ$ID_R_LT<-paste(LCi_bm2_R_LTCrop_AEZ$GCAM_region_ID,LCi_bm2_R_LTCrop_AEZ$Land_Type,sep="")

#Create table with the SAGE land types represented, all region/LT combinations written out, and AEZs as columns
LC_bm2_R_LTCrop_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$year==2005 & LC_bm2_R_LT_Yh_AEZ$Land_Type %in% LCi_bm2_R_LTCrop_AEZ$Land_Type,1:2]
LC_bm2_R_LTCrop_AEZ$ID_R_LT<-paste(LC_bm2_R_LTCrop_AEZ$GCAM_region_ID,LC_bm2_R_LTCrop_AEZ$Land_Type,sep="")

#Divide incomplete land cover table into separate tables, one for each AEZ
LCi_bm2_R_LTCrop_AEZ1<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==1,]
LCi_bm2_R_LTCrop_AEZ2<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==2,]
LCi_bm2_R_LTCrop_AEZ3<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==3,]
LCi_bm2_R_LTCrop_AEZ4<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==4,]
LCi_bm2_R_LTCrop_AEZ5<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==5,]
LCi_bm2_R_LTCrop_AEZ6<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==6,]
LCi_bm2_R_LTCrop_AEZ7<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==7,]
LCi_bm2_R_LTCrop_AEZ8<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==8,]
LCi_bm2_R_LTCrop_AEZ9<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==9,]
LCi_bm2_R_LTCrop_AEZ10<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==10,]
LCi_bm2_R_LTCrop_AEZ11<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==11,]
LCi_bm2_R_LTCrop_AEZ12<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==12,]
LCi_bm2_R_LTCrop_AEZ13<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==13,]
LCi_bm2_R_LTCrop_AEZ14<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==14,]
LCi_bm2_R_LTCrop_AEZ15<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==15,]
LCi_bm2_R_LTCrop_AEZ16<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==16,]
LCi_bm2_R_LTCrop_AEZ17<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==17,]
LCi_bm2_R_LTCrop_AEZ18<-LCi_bm2_R_LTCrop_AEZ[LCi_bm2_R_LTCrop_AEZ$AEZ==18,]

#Paste in land cover estimates
LC_bm2_R_LTCrop_AEZ$AEZ1<-LCi_bm2_R_LTCrop_AEZ1$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ1$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ2<-LCi_bm2_R_LTCrop_AEZ2$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ2$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ3<-LCi_bm2_R_LTCrop_AEZ3$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ3$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ4<-LCi_bm2_R_LTCrop_AEZ4$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ4$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ5<-LCi_bm2_R_LTCrop_AEZ5$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ5$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ6<-LCi_bm2_R_LTCrop_AEZ6$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ6$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ7<-LCi_bm2_R_LTCrop_AEZ7$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ7$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ8<-LCi_bm2_R_LTCrop_AEZ8$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ8$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ9<-LCi_bm2_R_LTCrop_AEZ9$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ9$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ10<-LCi_bm2_R_LTCrop_AEZ10$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ10$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ11<-LCi_bm2_R_LTCrop_AEZ11$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ11$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ12<-LCi_bm2_R_LTCrop_AEZ12$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ12$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ13<-LCi_bm2_R_LTCrop_AEZ13$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ13$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ14<-LCi_bm2_R_LTCrop_AEZ14$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ14$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ15<-LCi_bm2_R_LTCrop_AEZ15$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ15$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ16<-LCi_bm2_R_LTCrop_AEZ16$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ16$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ17<-LCi_bm2_R_LTCrop_AEZ17$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ17$ID_R_LT)]
LC_bm2_R_LTCrop_AEZ$AEZ18<-LCi_bm2_R_LTCrop_AEZ18$Area_bm2[match(LC_bm2_R_LTCrop_AEZ$ID_R_LT,LCi_bm2_R_LTCrop_AEZ18$ID_R_LT)]

#Sort, move ID vector to last column, and replace NA's with 0
LC_bm2_R_LTCrop_AEZ<-LC_bm2_R_LTCrop_AEZ[order(LC_bm2_R_LTCrop_AEZ$GCAM_region_ID,LC_bm2_R_LTCrop_AEZ$Land_Type),]
LC_bm2_R_LTCrop_AEZ<-LC_bm2_R_LTCrop_AEZ[,c(1:2,4:21,3)]
LC_bm2_R_LTCrop_AEZ[is.na(LC_bm2_R_LTCrop_AEZ)]<-0

#Write this table out
write.table(LC_bm2_R_LTCrop_AEZ,file="Rdata_out/LC_bm2_R_LTCrop_AEZ_27.csv",sep=",",col.names=TRUE,row.names=FALSE)

#PASTURE
#Subset pasture in 2005 from GIS table
LC_km2_Past_2005<-LC_km2_GIS[LC_km2_GIS$LT_HYDE=="Pasture" & LC_km2_GIS$Year==2005,]

#Aggregate by SAGE type. Non-existent combinations are not written out, indicated by LCi
LCi_bm2_R_LTPast_AEZ<-aggregate(LC_km2_Past_2005$Area_bm2,list(GCAM_region_ID=LC_km2_Past_2005$GCAM_region_ID,Land_Type=LC_km2_Past_2005$LT_SAGE,AEZ=LC_km2_Past_2005$AEZ),sum)
names(LCi_bm2_R_LTPast_AEZ)[names(LCi_bm2_R_LTPast_AEZ)=="x"]<-"Area_bm2"
LCi_bm2_R_LTPast_AEZ$ID_R_LT<-paste(LCi_bm2_R_LTPast_AEZ$GCAM_region_ID,LCi_bm2_R_LTPast_AEZ$Land_Type,sep="")

#Create table with the SAGE land types represented, all region/LT combinations written out, and AEZs as columns
LC_bm2_R_LTPast_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$year==2005 & LC_bm2_R_LT_Yh_AEZ$Land_Type %in% LCi_bm2_R_LTPast_AEZ$Land_Type,1:2]
LC_bm2_R_LTPast_AEZ$ID_R_LT<-paste(LC_bm2_R_LTPast_AEZ$GCAM_region_ID,LC_bm2_R_LTPast_AEZ$Land_Type,sep="")

#Divide incomplete land cover table into separate tables, one for each AEZ
LCi_bm2_R_LTPast_AEZ1<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==1,]
LCi_bm2_R_LTPast_AEZ2<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==2,]
LCi_bm2_R_LTPast_AEZ3<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==3,]
LCi_bm2_R_LTPast_AEZ4<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==4,]
LCi_bm2_R_LTPast_AEZ5<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==5,]
LCi_bm2_R_LTPast_AEZ6<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==6,]
LCi_bm2_R_LTPast_AEZ7<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==7,]
LCi_bm2_R_LTPast_AEZ8<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==8,]
LCi_bm2_R_LTPast_AEZ9<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==9,]
LCi_bm2_R_LTPast_AEZ10<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==10,]
LCi_bm2_R_LTPast_AEZ11<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==11,]
LCi_bm2_R_LTPast_AEZ12<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==12,]
LCi_bm2_R_LTPast_AEZ13<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==13,]
LCi_bm2_R_LTPast_AEZ14<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==14,]
LCi_bm2_R_LTPast_AEZ15<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==15,]
LCi_bm2_R_LTPast_AEZ16<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==16,]
LCi_bm2_R_LTPast_AEZ17<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==17,]
LCi_bm2_R_LTPast_AEZ18<-LCi_bm2_R_LTPast_AEZ[LCi_bm2_R_LTPast_AEZ$AEZ==18,]

#Paste in land cover estimates
LC_bm2_R_LTPast_AEZ$AEZ1<-LCi_bm2_R_LTPast_AEZ1$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ1$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ2<-LCi_bm2_R_LTPast_AEZ2$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ2$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ3<-LCi_bm2_R_LTPast_AEZ3$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ3$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ4<-LCi_bm2_R_LTPast_AEZ4$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ4$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ5<-LCi_bm2_R_LTPast_AEZ5$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ5$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ6<-LCi_bm2_R_LTPast_AEZ6$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ6$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ7<-LCi_bm2_R_LTPast_AEZ7$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ7$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ8<-LCi_bm2_R_LTPast_AEZ8$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ8$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ9<-LCi_bm2_R_LTPast_AEZ9$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ9$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ10<-LCi_bm2_R_LTPast_AEZ10$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ10$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ11<-LCi_bm2_R_LTPast_AEZ11$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ11$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ12<-LCi_bm2_R_LTPast_AEZ12$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ12$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ13<-LCi_bm2_R_LTPast_AEZ13$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ13$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ14<-LCi_bm2_R_LTPast_AEZ14$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ14$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ15<-LCi_bm2_R_LTPast_AEZ15$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ15$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ16<-LCi_bm2_R_LTPast_AEZ16$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ16$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ17<-LCi_bm2_R_LTPast_AEZ17$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ17$ID_R_LT)]
LC_bm2_R_LTPast_AEZ$AEZ18<-LCi_bm2_R_LTPast_AEZ18$Area_bm2[match(LC_bm2_R_LTPast_AEZ$ID_R_LT,LCi_bm2_R_LTPast_AEZ18$ID_R_LT)]

#Sort, move ID vector to last column, and replace NA's with 0
LC_bm2_R_LTPast_AEZ<-LC_bm2_R_LTPast_AEZ[order(LC_bm2_R_LTPast_AEZ$GCAM_region_ID,LC_bm2_R_LTPast_AEZ$Land_Type),]
LC_bm2_R_LTPast_AEZ<-LC_bm2_R_LTPast_AEZ[,c(1:2,4:21,3)]
LC_bm2_R_LTPast_AEZ[is.na(LC_bm2_R_LTPast_AEZ)]<-0

#Write this table out
write.table(LC_bm2_R_LTPast_AEZ,file="Rdata_out/LC_bm2_R_LTPast_AEZ_27.csv",sep=",",col.names=TRUE,row.names=FALSE)
