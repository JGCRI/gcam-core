#take a subset of the land cover table: cropland, and in only the years that are also in the HA table (modeled timestep years)
LC_bm2_R_CropLand_Y_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland" & LC_bm2_R_LT_Yh_AEZ$year %in% ag_HA_bm2_R_C_Y_AEZ$year,]

#compile harvested area across all crops
ag_HA_bm2_R_Y_AEZ<-aggregate(ag_HA_bm2_R_C_Y_AEZ[,4:21],by=list(ag_HA_bm2_R_C_Y_AEZ$GCAM_region_ID,ag_HA_bm2_R_C_Y_AEZ$year),FUN=sum)
names(ag_HA_bm2_R_Y_AEZ)[names(ag_HA_bm2_R_Y_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(ag_HA_bm2_R_Y_AEZ)[names(ag_HA_bm2_R_Y_AEZ)=="Group.2"]<-"year"

#compile the ratio of "temporary crops" to total arable land in each region
fallowland_kha_RESOURCESTAT<-read.table("Inventory Data/FAO/Rdata_in/fallowland_kha_RESOURCESTAT.csv",header=T,sep=',')
CL_kha_RESOURCESTAT<-read.table("Inventory Data/FAO/Rdata_in/CL_kha_RESOURCESTAT.csv",header=T,sep=',')
harv_CL_kha_RESOURCESTAT<-read.table("Inventory Data/FAO/Rdata_in/harv_CL_kha_RESOURCESTAT.csv",header=T,sep=',')
ctry_reg_RESOURCESTAT<-read.table("Inventory Data/FAO/Rdata_in/ctry_reg_RESOURCESTAT.csv",header=T,sep=',')

#make table with fallow land compared to total arable land
cropland_fallow<-data.frame(CL_kha_RESOURCESTAT[,c(1,ncol(CL_kha_RESOURCESTAT))])
names(cropland_fallow)[names(cropland_fallow)=="avg"]<-"cropland"
cropland_fallow_adj<-cropland_fallow[cropland_fallow$countries %in% fallowland_kha_RESOURCESTAT$countries,]
cropland_fallow_adj$fallow<-fallowland_kha_RESOURCESTAT$avg[match(cropland_fallow_adj$countries,fallowland_kha_RESOURCESTAT$countries)]
cropland_fallow_adj$GCAM_region_ID<-ctry_reg_RESOURCESTAT$GCAM_region_ID[match(cropland_fallow_adj$countries,ctry_reg_RESOURCESTAT$countries)]
cropland_fallow_R<-aggregate(cropland_fallow_adj[,2:3],by=list(cropland_fallow_adj$GCAM_region_ID),FUN=sum)
names(cropland_fallow_R)[names(cropland_fallow_R)=="Group.1"]<-"GCAM_region_ID"
cropland_fallow_R$fallow_frac<-cropland_fallow_R$fallow/cropland_fallow_R$cropland

#make table with cropped land compared to total arable land
cropland_cropped<-data.frame(CL_kha_RESOURCESTAT[,c(1,ncol(CL_kha_RESOURCESTAT))])
names(cropland_cropped)[names(cropland_cropped)=="avg"]<-"cropland"
cropland_cropped_adj<-cropland_fallow[cropland_cropped$countries %in% harv_CL_kha_RESOURCESTAT$countries,]
cropland_cropped_adj$cropped<-harv_CL_kha_RESOURCESTAT$avg[match(cropland_cropped_adj$countries,harv_CL_kha_RESOURCESTAT$countries)]
cropland_cropped_adj$GCAM_region_ID<-ctry_reg_RESOURCESTAT$GCAM_region_ID[match(cropland_cropped_adj$countries,ctry_reg_RESOURCESTAT$countries)]
cropland_cropped_R<-aggregate(cropland_cropped_adj[,2:3],by=list(cropland_cropped_adj$GCAM_region_ID),FUN=sum)
names(cropland_cropped_R)[names(cropland_cropped_R)=="Group.1"]<-"GCAM_region_ID"
cropland_cropped_R$cropped_frac<-cropland_cropped_R$cropped/cropland_cropped_R$cropland

#calculate the average amount of cropland that is not in production as the fallow land fraction, where available, or else the non-cropped cropland
nonharvested_cropland_R<-LC_bm2_R_CropLand_Y_AEZ[LC_bm2_R_CropLand_Y_AEZ$year==1990,1:2]
nonharvested_cropland_R$fallow_frac<-cropland_fallow_R$fallow_frac[match(nonharvested_cropland_R$GCAM_region_ID,cropland_fallow_R$GCAM_region_ID)]
nonharvested_cropland_R$uncropped_frac<-1-cropland_cropped_R$cropped_frac[match(nonharvested_cropland_R$GCAM_region_ID,cropland_cropped_R$GCAM_region_ID)]
nonharvested_cropland_R$nonharvested_frac<-ifelse(is.na(nonharvested_cropland_R$fallow_frac),nonharvested_cropland_R$uncropped_frac,nonharvested_cropland_R$fallow_frac)

#make a table with the available cropland for harvest, by region, year, and AEZ (assume same nonharvested_frac in all AEZs)
LC_bm2_R_AvailableCropLand_Y_AEZ<-LC_bm2_R_CropLand_Y_AEZ
LC_bm2_R_AvailableCropLand_Y_AEZ$nonharvested_frac<-nonharvested_cropland_R$nonharvested_frac[match(LC_bm2_R_AvailableCropLand_Y_AEZ$GCAM_region_ID,nonharvested_cropland_R$GCAM_region_ID)]
LC_bm2_R_AvailableCropLand_Y_AEZ<-cbind(LC_bm2_R_AvailableCropLand_Y_AEZ[,1:3],LC_bm2_R_AvailableCropLand_Y_AEZ[,4:21]*(1-LC_bm2_R_AvailableCropLand_Y_AEZ$nonharvested_frac))

#Calculate fallow land as the total cropland times nonharvested fraction. This is one component to "other arable land"
LC_bm2_R_FallowLand_Y_AEZ<-LC_bm2_R_CropLand_Y_AEZ
LC_bm2_R_FallowLand_Y_AEZ$nonharvested_frac<-nonharvested_cropland_R$nonharvested_frac[match(LC_bm2_R_FallowLand_Y_AEZ$GCAM_region_ID,nonharvested_cropland_R$GCAM_region_ID)]
LC_bm2_R_FallowLand_Y_AEZ<-cbind(LC_bm2_R_FallowLand_Y_AEZ[,1:3],LC_bm2_R_FallowLand_Y_AEZ[,4:21]*LC_bm2_R_FallowLand_Y_AEZ$nonharvested_frac)
LC_bm2_R_FallowLand_Y_AEZ$Land_Type<-"FallowLand"

write.table(LC_bm2_R_FallowLand_Y_AEZ,file="Rdata_out/LC_bm2_R_FallowLand_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Calculate the harvested to cropped land ratio for all crops, by region, year, and AEZ. Set the minimum to 1
ag_HA_to_CropLand_R_Y_AEZ<-cbind(ag_HA_bm2_R_Y_AEZ[,1:2],ag_HA_bm2_R_Y_AEZ[,3:20]/(LC_bm2_R_AvailableCropLand_Y_AEZ[,4:21]+1e-9))
ag_HA_to_CropLand_R_Y_AEZ<-cbind(ag_HA_to_CropLand_R_Y_AEZ[,1:2],apply(ag_HA_to_CropLand_R_Y_AEZ[,3:20],2,function(x){x[x<1]=1;x}))
#There should also be a maximum (assumed here to be 2.5), but setting this maximum will cause cropland to expand
#This additional cropland will need to be balanced by a deduction from other land types later on, so it is tracked below
print(ifelse(any(ag_HA_to_CropLand_R_Y_AEZ[,3:20]>2.5),"Cropland increased where harvested:cropped ratio exceeded threshold value (2.5)","No adjustment applied to Hyde cropland estimates"))
ag_HA_to_CropLand_R_Y_AEZ<-cbind(ag_HA_to_CropLand_R_Y_AEZ[,1:2],apply(ag_HA_to_CropLand_R_Y_AEZ[,3:20],2,function(x){x[x>2.5]=2.5;x}))

write.table(ag_HA_to_CropLand_R_Y_AEZ,file="Rdata_out/ag_HA_to_CropLand_R_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#The ag_HA_to_CropLand ratio is assumed to be a property of the region and AEZ (not individual crops), as individual sites are planted with different crops
#Calculate cropland requirements of each crop as harvested area divided by regional ag_HA_to_CropLand ratio
#First, make a dataframe where the ag_HA_to_CropLand table has the same dimensions as the cropland table. It needs to be copied by the number of crops.
ag_HA_to_CropLand_R_C_Y_AEZ<-ag_HA_to_CropLand_R_Y_AEZ[rep(1:nrow(ag_HA_to_CropLand_R_Y_AEZ), times=(nrow(ag_HA_bm2_R_C_Y_AEZ)/nrow(ag_HA_bm2_R_Y_AEZ))), ]
#sort
ag_HA_to_CropLand_R_C_Y_AEZ<-ag_HA_to_CropLand_R_C_Y_AEZ[order(ag_HA_to_CropLand_R_C_Y_AEZ$year,ag_HA_to_CropLand_R_C_Y_AEZ$GCAM_region_ID),]
ag_HA_bm2_R_Y_C_AEZ<-ag_HA_bm2_R_C_Y_AEZ[order(ag_HA_bm2_R_C_Y_AEZ$year,ag_HA_bm2_R_C_Y_AEZ$GCAM_region_ID),]
LC_bm2_R_HarvCropLand_Y_C_AEZ<-cbind(ag_HA_bm2_R_Y_C_AEZ[,1:3],ag_HA_bm2_R_Y_C_AEZ[,4:21]/ag_HA_to_CropLand_R_C_Y_AEZ[,3:20])
#sort by year, commodity, and then region
LC_bm2_R_HarvCropLand_C_Y_AEZ<-LC_bm2_R_HarvCropLand_Y_C_AEZ[order(LC_bm2_R_HarvCropLand_Y_C_AEZ$year,LC_bm2_R_HarvCropLand_Y_C_AEZ$GCAM_commodity,LC_bm2_R_HarvCropLand_Y_C_AEZ$GCAM_region_ID),]

write.table(LC_bm2_R_HarvCropLand_C_Y_AEZ,file="Rdata_out/LC_bm2_R_HarvCropLand_C_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#aggregate across crops to get the new total harvested cropland area, by region/aez/year
LC_bm2_R_HarvCropLand_Y_AEZ<-aggregate(LC_bm2_R_HarvCropLand_C_Y_AEZ[,4:21],by=list(LC_bm2_R_HarvCropLand_C_Y_AEZ$GCAM_region_ID,LC_bm2_R_HarvCropLand_C_Y_AEZ$year),FUN=sum)
names(LC_bm2_R_HarvCropLand_Y_AEZ)[names(LC_bm2_R_HarvCropLand_Y_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(LC_bm2_R_HarvCropLand_Y_AEZ)[names(LC_bm2_R_HarvCropLand_Y_AEZ)=="Group.2"]<-"year"
LC_bm2_R_HarvCropLand_Y_AEZ$Land_Type<-"HarvCropLand"
LC_bm2_R_HarvCropLand_Y_AEZ<-LC_bm2_R_HarvCropLand_Y_AEZ[,c(1,21,2:20)]

#Calculate economic yield as production divided by harvested cropland. Write out this preliminary table.
ag_EcYield_kgm2_R_C_Y_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_Y_AEZ[,1:3],ag_Prod_Mt_R_C_Y_AEZ_adj[4:21]/(LC_bm2_R_HarvCropLand_C_Y_AEZ[,4:21]+1e-9))

write.table(ag_EcYield_kgm2_R_C_Y_AEZ,file="Rdata_out/ag_EcYield_kgm2_R_C_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Repeat method from code file #4, this time adjusting cropland instead of production
#Generate vectors for assessment of yields
maxEcYieldUSA<-apply(ag_EcYield_kgm2_R_C_Y_AEZ[ag_EcYield_kgm2_R_C_Y_AEZ$GCAM_region_ID==1&ag_EcYield_kgm2_R_C_Y_AEZ$year==2005,4:21],1,max)
ag_maxEcYieldUSA_C_2005<-cbind(ag_EcYield_kgm2_R_C_Y_AEZ[ag_EcYield_kgm2_R_C_Y_AEZ$GCAM_region_ID==1&ag_EcYield_kgm2_R_C_Y_AEZ$year==2005,1:3],maxEcYieldUSA)
#The USA doesn't grow palmfruit, so hard-wiring a maximum yield for palm fruit that is equal to the global max
ag_maxEcYieldUSA_C_2005$maxEcYieldUSA[ag_maxEcYieldUSA_C_2005$GCAM_commodity=="PalmFruit"]<-max(ag_EcYield_kgm2_R_C_Y_AEZ[ag_EcYield_kgm2_R_C_Y_AEZ$GCAM_commodity=="PalmFruit",4:21])
ag_EcYield_kgm2_R_C_Y_AEZ$max<-ag_maxEcYieldUSA_C_2005$maxEcYieldUSA[match(ag_EcYield_kgm2_R_C_Y_AEZ$GCAM_commodity,ag_maxEcYieldUSA_C_2005$GCAM_commodity)]

#Calculate the regional average yield for each region and crop
ag_Prod_Mt_R_C_Y_AEZ_adj$total<-apply(ag_Prod_Mt_R_C_Y_AEZ_adj[,4:21],1,sum)
LC_bm2_R_HarvCropLand_C_Y_AEZ$total<-apply(LC_bm2_R_HarvCropLand_C_Y_AEZ[,4:21],1,sum)
ag_EcYield_kgm2_R_C_Y_AEZ$avg<-ag_Prod_Mt_R_C_Y_AEZ_adj$total/LC_bm2_R_HarvCropLand_C_Y_AEZ$total

#Set rule: if yield in AEZ is greater than the USA max yield for that crop, and that AEZ accounts for less than 1.5%
#of total regional production of that crop, set the yield in that AEZ equal to the regional average for that crop.
#Note that prodfrac table is prior to adjustment in production; this is not of consequence
ag_EcYield_kgm2_R_C_Y_AEZ_adj<-ag_EcYield_kgm2_R_C_Y_AEZ[,1:3]
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ1<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ1>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ1<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ1)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ2<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ2>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ2<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ2)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ3<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ3>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ3<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ3)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ4<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ4>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ4<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ4)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ5<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ5>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ5<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ5)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ6<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ6>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ6<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ6)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ7<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ7>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ7<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ7)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ8<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ8>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ8<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ8)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ9<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ9>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ9<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ9)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ10<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ10>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ10<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ10)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ11<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ11>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ11<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ11)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ12<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ12>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ12<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ12)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ13<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ13>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ13<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ13)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ14<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ14>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ14<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ14)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ15<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ15>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ15<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ15)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ16<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ16>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ16<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ16)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ17<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ17>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ17<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ17)
ag_EcYield_kgm2_R_C_Y_AEZ_adj$AEZ18<-ifelse(ag_EcYield_kgm2_R_C_Y_AEZ$AEZ18>ag_EcYield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ18<0.015,ag_EcYield_kgm2_R_C_Y_AEZ$avg,ag_EcYield_kgm2_R_C_Y_AEZ$AEZ18)

#calculate adjusted cropland as production divided by adjusted yields
LC_bm2_R_HarvCropLand_C_Y_AEZ_adj<-cbind(ag_EcYield_kgm2_R_C_Y_AEZ_adj[,1:3],ag_Prod_Mt_R_C_Y_AEZ_adj[,4:21]/(ag_EcYield_kgm2_R_C_Y_AEZ_adj[,4:21]+1e-9))

write.table(ag_EcYield_kgm2_R_C_Y_AEZ_adj,file="Rdata_out/ag_EcYield_kgm2_R_C_Y_AEZ_adj_22.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_HarvCropLand_C_Y_AEZ_adj,file="Rdata_out/LC_bm2_R_HarvCropLand_C_Y_AEZ_adj_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Aggregate crops in adjusted harvested cropland table
LC_bm2_R_HarvCropLand_Y_AEZ_adj<-aggregate(LC_bm2_R_HarvCropLand_C_Y_AEZ_adj[,4:21],by=list(LC_bm2_R_HarvCropLand_C_Y_AEZ_adj$GCAM_region_ID,LC_bm2_R_HarvCropLand_C_Y_AEZ_adj$year),FUN=sum)
names(LC_bm2_R_HarvCropLand_Y_AEZ_adj)[names(LC_bm2_R_HarvCropLand_Y_AEZ_adj)=="Group.1"]<-"GCAM_region_ID"
names(LC_bm2_R_HarvCropLand_Y_AEZ_adj)[names(LC_bm2_R_HarvCropLand_Y_AEZ_adj)=="Group.2"]<-"year"
LC_bm2_R_HarvCropLand_Y_AEZ_adj$Land_Type<-"HarvCropLand"
LC_bm2_R_HarvCropLand_Y_AEZ_adj<-cbind(LC_bm2_R_HarvCropLand_Y_AEZ_adj[,c(1:2,21)],LC_bm2_R_HarvCropLand_Y_AEZ_adj[,3:20])

#The minimum threshold on HA:CL means that land reported as cropland in Hyde will not be assigned to a crop. This needs to be re-mapped to "other arable land"
#The maximum threshold on HA:CL and on economic yield means that the land reported as cropland in Hyde may be less than harvested area,
# even allowing for double or triple cropping. In this case land needs to be mapped to cropland from other uses.
#Calculate the residual land cover that is cropland
LC_bm2_R_ResidualCropLand_Y_AEZ<-cbind(LC_bm2_R_HarvCropLand_Y_AEZ_adj[,1:3],LC_bm2_R_HarvCropLand_Y_AEZ_adj[,4:21]-LC_bm2_R_AvailableCropLand_Y_AEZ[,4:21])
LC_bm2_R_ResidualCropLand_Y_AEZ$Land_Type<-"ResidualCropLand"

#Where residuals are negative, this is "unused" cropland that will be mapped to other arable land
LC_bm2_R_UnusedCropLand_Y_AEZ<-cbind(LC_bm2_R_ResidualCropLand_Y_AEZ[,1:3],-1*apply(LC_bm2_R_ResidualCropLand_Y_AEZ[,4:21],2,function(x){x[x>0]=0;x}))
LC_bm2_R_UnusedCropLand_Y_AEZ$Land_Type<-"UnusedCropLand"

#Where residuals are positive, this is "extra" land that will later be deducted from other categories.
LC_bm2_R_ExtraCropLand_Y_AEZ<-cbind(LC_bm2_R_ResidualCropLand_Y_AEZ[,1:3],apply(LC_bm2_R_ResidualCropLand_Y_AEZ[,4:21],2,function(x){x[x<0]=0;x}))
LC_bm2_R_ExtraCropLand_Y_AEZ$Land_Type<-"ExtraCropLand"

write.table(LC_bm2_R_UnusedCropLand_Y_AEZ,file="Rdata_out/LC_bm2_R_UnusedCropLand_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(LC_bm2_R_ExtraCropLand_Y_AEZ,file="Rdata_out/LC_bm2_R_ExtraCropLand_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Other arable land = known fallow plus unused land
LC_bm2_R_OtherArableLand_Y_AEZ<-cbind(LC_bm2_R_FallowLand_Y_AEZ[,1:3],LC_bm2_R_FallowLand_Y_AEZ[,4:21] + LC_bm2_R_UnusedCropLand_Y_AEZ[,4:21])
LC_bm2_R_OtherArableLand_Y_AEZ$Land_Type<-"OtherArableLand"

write.table(LC_bm2_R_OtherArableLand_Y_AEZ,file="Rdata_out/LC_bm2_R_OtherArableLand_Y_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Land use history
#subset cropland history in separate tables
LC_bm2_R_CropLand_1700_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1700,]
LC_bm2_R_CropLand_1750_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1750,]
LC_bm2_R_CropLand_1800_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1800,]
LC_bm2_R_CropLand_1850_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1850,]
LC_bm2_R_CropLand_1900_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1900,]
LC_bm2_R_CropLand_1950_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1950,]
LC_bm2_R_CropLand_1975_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1975,]
LC_bm2_R_CropLand_1990_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Cropland"&LC_bm2_R_LT_Yh_AEZ$year==1990,]

#calculate each region and aez's ratio of cropland in each historical year, relative to 1990
CropLandRatio_R_1700_AEZ<-cbind(LC_bm2_R_CropLand_1700_AEZ[,1:3],LC_bm2_R_CropLand_1700_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1750_AEZ<-cbind(LC_bm2_R_CropLand_1750_AEZ[,1:3],LC_bm2_R_CropLand_1750_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1800_AEZ<-cbind(LC_bm2_R_CropLand_1800_AEZ[,1:3],LC_bm2_R_CropLand_1800_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1850_AEZ<-cbind(LC_bm2_R_CropLand_1850_AEZ[,1:3],LC_bm2_R_CropLand_1850_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1900_AEZ<-cbind(LC_bm2_R_CropLand_1900_AEZ[,1:3],LC_bm2_R_CropLand_1900_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1950_AEZ<-cbind(LC_bm2_R_CropLand_1950_AEZ[,1:3],LC_bm2_R_CropLand_1950_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))
CropLandRatio_R_1975_AEZ<-cbind(LC_bm2_R_CropLand_1975_AEZ[,1:3],LC_bm2_R_CropLand_1975_AEZ[,4:21]/(LC_bm2_R_CropLand_1990_AEZ[,4:21]+1e-9))

#Historical other arable land can now be calculated, using these regional ratios. First subset 1990.
LC_bm2_R_OtherArableLand_1990_AEZ<-LC_bm2_R_OtherArableLand_Y_AEZ[LC_bm2_R_OtherArableLand_Y_AEZ$year==1990,]
#Multiply historical cropland ratios by other arable land in 1990
LC_bm2_R_OtherArableLand_1700_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1700_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1700_AEZ$year<-1700
LC_bm2_R_OtherArableLand_1750_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1750_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1750_AEZ$year<-1750
LC_bm2_R_OtherArableLand_1800_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1800_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1800_AEZ$year<-1800
LC_bm2_R_OtherArableLand_1850_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1850_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1850_AEZ$year<-1850
LC_bm2_R_OtherArableLand_1900_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1900_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1900_AEZ$year<-1900
LC_bm2_R_OtherArableLand_1950_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1950_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1950_AEZ$year<-1950
LC_bm2_R_OtherArableLand_1975_AEZ<-cbind(LC_bm2_R_OtherArableLand_1990_AEZ[,1:3],LC_bm2_R_OtherArableLand_1990_AEZ[,4:21]*CropLandRatio_R_1975_AEZ[,4:21])
LC_bm2_R_OtherArableLand_1975_AEZ$year<-1975

#Combine all tables by rows to compile land use history
LC_bm2_R_OtherArableLand_Yh_AEZ<-rbind(LC_bm2_R_OtherArableLand_1700_AEZ,LC_bm2_R_OtherArableLand_1750_AEZ,LC_bm2_R_OtherArableLand_1800_AEZ,LC_bm2_R_OtherArableLand_1850_AEZ,LC_bm2_R_OtherArableLand_1900_AEZ,LC_bm2_R_OtherArableLand_1950_AEZ,LC_bm2_R_OtherArableLand_1975_AEZ,LC_bm2_R_OtherArableLand_Y_AEZ)
#add lookup vector
LC_bm2_R_OtherArableLand_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_OtherArableLand_Yh_AEZ$GCAM_region_ID,LC_bm2_R_OtherArableLand_Yh_AEZ$Land_Type,LC_bm2_R_OtherArableLand_Yh_AEZ$year,sep="")

write.table(LC_bm2_R_OtherArableLand_Yh_AEZ,file="Rdata_out/LC_bm2_R_OtherArableLand_Yh_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Historical extra cropland is calculated in similar fashion: extra in 1990 multiplied by historical cropland ratios to 1990
# Historical extra cropland could be assumed 0 in historical periods, but probably bad idea b/c it will cause a land use change emission in 1990
LC_bm2_R_ExtraCropLand_1990_AEZ<-LC_bm2_R_ExtraCropLand_Y_AEZ[LC_bm2_R_ExtraCropLand_Y_AEZ$year==1990,]
LC_bm2_R_ExtraCropLand_1700_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1700_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1700_AEZ$year<-1700
LC_bm2_R_ExtraCropLand_1750_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1750_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1750_AEZ$year<-1750
LC_bm2_R_ExtraCropLand_1800_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1800_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1800_AEZ$year<-1800
LC_bm2_R_ExtraCropLand_1850_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1850_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1850_AEZ$year<-1850
LC_bm2_R_ExtraCropLand_1900_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1900_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1900_AEZ$year<-1900
LC_bm2_R_ExtraCropLand_1950_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1950_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1950_AEZ$year<-1950
LC_bm2_R_ExtraCropLand_1975_AEZ<-cbind(LC_bm2_R_ExtraCropLand_1990_AEZ[,1:3],LC_bm2_R_ExtraCropLand_1990_AEZ[,4:21]*CropLandRatio_R_1975_AEZ[,4:21])
LC_bm2_R_ExtraCropLand_1975_AEZ$year<-1975

#Combine all tables by rows to compile land use history
LC_bm2_R_ExtraCropLand_Yh_AEZ<-rbind(LC_bm2_R_ExtraCropLand_1700_AEZ,LC_bm2_R_ExtraCropLand_1750_AEZ,LC_bm2_R_ExtraCropLand_1800_AEZ,LC_bm2_R_ExtraCropLand_1850_AEZ,LC_bm2_R_ExtraCropLand_1900_AEZ,LC_bm2_R_ExtraCropLand_1950_AEZ,LC_bm2_R_ExtraCropLand_1975_AEZ,LC_bm2_R_ExtraCropLand_Y_AEZ)
#add lookup vector
LC_bm2_R_ExtraCropLand_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_ExtraCropLand_Yh_AEZ$GCAM_region_ID,LC_bm2_R_ExtraCropLand_Yh_AEZ$Land_Type,LC_bm2_R_ExtraCropLand_Yh_AEZ$year,sep="")

write.table(LC_bm2_R_ExtraCropLand_Yh_AEZ,file="Rdata_out/LC_bm2_R_ExtraCropLand_Yh_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#subset 1990 from the harvested cropland by crop table                                   
LC_bm2_R_HarvCropLand_C_1990_AEZ<-LC_bm2_R_HarvCropLand_C_Y_AEZ_adj[LC_bm2_R_HarvCropLand_C_Y_AEZ_adj$year==1990,]
#Repeat cropland ratio tables by number of crop types.
CropLandRatio_R_C_1700_AEZ<-CropLandRatio_R_1700_AEZ[rep(1:nrow(CropLandRatio_R_1700_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1700_AEZ))), ]
CropLandRatio_R_C_1750_AEZ<-CropLandRatio_R_1750_AEZ[rep(1:nrow(CropLandRatio_R_1750_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1750_AEZ))), ]
CropLandRatio_R_C_1800_AEZ<-CropLandRatio_R_1800_AEZ[rep(1:nrow(CropLandRatio_R_1800_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1800_AEZ))), ]
CropLandRatio_R_C_1850_AEZ<-CropLandRatio_R_1850_AEZ[rep(1:nrow(CropLandRatio_R_1850_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1850_AEZ))), ]
CropLandRatio_R_C_1900_AEZ<-CropLandRatio_R_1900_AEZ[rep(1:nrow(CropLandRatio_R_1900_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1900_AEZ))), ]
CropLandRatio_R_C_1950_AEZ<-CropLandRatio_R_1950_AEZ[rep(1:nrow(CropLandRatio_R_1950_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1950_AEZ))), ]
CropLandRatio_R_C_1975_AEZ<-CropLandRatio_R_1975_AEZ[rep(1:nrow(CropLandRatio_R_1975_AEZ), times=(nrow(LC_bm2_R_HarvCropLand_C_1990_AEZ)/nrow(CropLandRatio_R_1975_AEZ))), ]

#Multiply historical cropland ratios by harvested cropland by crop in 1990
LC_bm2_R_HarvCropLand_C_1700_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1700_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1700_AEZ$year<-1700
LC_bm2_R_HarvCropLand_C_1750_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1750_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1750_AEZ$year<-1750
LC_bm2_R_HarvCropLand_C_1800_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1800_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1800_AEZ$year<-1800
LC_bm2_R_HarvCropLand_C_1850_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1850_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1850_AEZ$year<-1850
LC_bm2_R_HarvCropLand_C_1900_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1900_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1900_AEZ$year<-1900
LC_bm2_R_HarvCropLand_C_1950_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1950_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1950_AEZ$year<-1950
LC_bm2_R_HarvCropLand_C_1975_AEZ<-cbind(LC_bm2_R_HarvCropLand_C_1990_AEZ[,1:3],LC_bm2_R_HarvCropLand_C_1990_AEZ[,4:21]*CropLandRatio_R_C_1975_AEZ[,4:21])
LC_bm2_R_HarvCropLand_C_1975_AEZ$year<-1975

#Combine all tables by rows to get land use history
LC_bm2_R_HarvCropLand_C_Yh_AEZ<-rbind(LC_bm2_R_HarvCropLand_C_1700_AEZ,LC_bm2_R_HarvCropLand_C_1750_AEZ,LC_bm2_R_HarvCropLand_C_1800_AEZ,LC_bm2_R_HarvCropLand_C_1850_AEZ,LC_bm2_R_HarvCropLand_C_1900_AEZ,LC_bm2_R_HarvCropLand_C_1950_AEZ,LC_bm2_R_HarvCropLand_C_1975_AEZ,LC_bm2_R_HarvCropLand_C_Y_AEZ_adj)
#add lookup vector
LC_bm2_R_HarvCropLand_C_Yh_AEZ$ID_R_C_Y<-paste(LC_bm2_R_HarvCropLand_C_Yh_AEZ$GCAM_region_ID,LC_bm2_R_HarvCropLand_C_Yh_AEZ$GCAM_commodity,LC_bm2_R_HarvCropLand_C_Yh_AEZ$year,sep="")

write.table(LC_bm2_R_HarvCropLand_C_Yh_AEZ,file="Rdata_out/LC_bm2_R_HarvCropLand_C_Yh_AEZ_22.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Combine crop types to get land use history
LC_bm2_R_HarvCropLand_Yh_AEZ<-aggregate(LC_bm2_R_HarvCropLand_C_Yh_AEZ[,4:21],by=list(LC_bm2_R_HarvCropLand_C_Yh_AEZ$GCAM_region_ID,LC_bm2_R_HarvCropLand_C_Yh_AEZ$year),FUN=sum)
names(LC_bm2_R_HarvCropLand_Yh_AEZ)[names(LC_bm2_R_HarvCropLand_Yh_AEZ)=="Group.1"]<-"GCAM_region_ID"
names(LC_bm2_R_HarvCropLand_Yh_AEZ)[names(LC_bm2_R_HarvCropLand_Yh_AEZ)=="Group.2"]<-"year"
LC_bm2_R_HarvCropLand_Yh_AEZ$Land_Type<-"HarvCropLand"
LC_bm2_R_HarvCropLand_Yh_AEZ<-LC_bm2_R_HarvCropLand_Yh_AEZ[,c(1,21,2:20)]
