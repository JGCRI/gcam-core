#Calculate yields of pasture by AEZ from the GTAP data on FodderGrass (hay) production. Assume the global average by AEZ in all regions.
#Production
ag_Prod_t_ctry_Grass_AEZ<-ag_Prod_t_GTAP[ag_Prod_t_GTAP$GCAM_commodity=="FodderGrass",]
ag_Prod_t_Grass_AEZ<-aggregate(ag_Prod_t_ctry_Grass_AEZ[,3:20],by=list(ag_Prod_t_ctry_Grass_AEZ$GCAM_commodity),FUN=sum)
names(ag_Prod_t_Grass_AEZ)[names(ag_Prod_t_Grass_AEZ)=="Group.1"]<-"GCAM_commodity"

#Harvested area
ag_HA_Ha_ctry_Grass_AEZ<-ag_HA_Ha_GTAP[ag_HA_Ha_GTAP$GCAM_commodity=="FodderGrass",]
ag_HA_Ha_Grass_AEZ<-aggregate(ag_HA_Ha_ctry_Grass_AEZ[,3:20],by=list(ag_HA_Ha_ctry_Grass_AEZ$GCAM_commodity),FUN=sum)
names(ag_HA_Ha_Grass_AEZ)[names(ag_HA_Ha_Grass_AEZ)=="Group.1"]<-"GCAM_commodity"

#Yield
ag_Yield_tHa_Grass_AEZ<-ag_Prod_t_Grass_AEZ
ag_Yield_tHa_Grass_AEZ[,2:19]<-ag_Prod_t_Grass_AEZ[,2:19] / ag_HA_Ha_Grass_AEZ[,2:19]
ag_Yield_tHa_Grass_AEZ[is.na(ag_Yield_tHa_Grass_AEZ)]<-0
ag_Yield_kgm2_R_Pasture_AEZ<-ag_Yield_tHa_Grass_AEZ
ag_Yield_kgm2_R_Pasture_AEZ[,2:19]<-ag_Yield_tHa_Grass_AEZ[,2:19] / 10

#Calculate bottom-up estimate of pasture production by region and AEZ (yield times land area)
LC_bm2_R_Pasture_Y_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Pasture" & LC_bm2_R_LT_Yh_AEZ$year %in% ag_Feed_Mt_R_Pasture_Y$year,]
ag_Yield_kgm2_R_Pasture_AEZ_repR_Y<-ag_Yield_kgm2_R_Pasture_AEZ[rep(1, times=nrow(LC_bm2_R_Pasture_Y_AEZ)),]
ag_potentialProd_Mt_R_Pasture_Y_AEZ<-cbind(LC_bm2_R_Pasture_Y_AEZ[,1:3],LC_bm2_R_Pasture_Y_AEZ[,4:21]*ag_Yield_kgm2_R_Pasture_AEZ_repR_Y[,2:19])

#Use this "potential production" to disaggregate actual pastureland production to AEZs
ag_potentialProd_Mt_R_Pasture_Y_AEZ$total<-apply(ag_potentialProd_Mt_R_Pasture_Y_AEZ[,4:21],1,sum)
ag_PastureProdfrac_R_Y_AEZ<-cbind(ag_potentialProd_Mt_R_Pasture_Y_AEZ[,1:3],ag_potentialProd_Mt_R_Pasture_Y_AEZ[,4:21]/ag_potentialProd_Mt_R_Pasture_Y_AEZ$total)
ag_Prod_Mt_R_Pasture_Y_AEZ<-cbind(ag_PastureProdfrac_R_Y_AEZ[,1:3],ag_PastureProdfrac_R_Y_AEZ[,4:21]*ag_Feed_Mt_R_Pasture_Y$Feed_Mt)
ag_Prod_Mt_R_Pasture_Y_AEZ$ID_R_LT_Y<-paste(ag_Prod_Mt_R_Pasture_Y_AEZ$GCAM_region_ID,ag_Prod_Mt_R_Pasture_Y_AEZ$Land_Type,ag_Prod_Mt_R_Pasture_Y_AEZ$year,sep="")

write.table(ag_Prod_Mt_R_Pasture_Y_AEZ,file="Rdata_out/ag_Prod_Mt_R_Pasture_Y_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Calculate land requirements.
LC_bm2_R_MgdPasture_Y_AEZ<-cbind(ag_Prod_Mt_R_Pasture_Y_AEZ[,1:3],ag_Prod_Mt_R_Pasture_Y_AEZ[,4:21]/(ag_Yield_kgm2_R_Pasture_AEZ_repR_Y[,2:19]+1e-9))
#Where managed pasture is greater than 85% of total pasture, reduce the managed pasture land. Output is unaffected so these regions have higher yields.
LC_MgdPastureFrac_Y_AEZ<-cbind(LC_bm2_R_MgdPasture_Y_AEZ[,1:3],LC_bm2_R_MgdPasture_Y_AEZ[,4:21]/(LC_bm2_R_Pasture_Y_AEZ[,4:21]+1e-9))
LC_MgdPastureFrac_Y_AEZ_adj<-cbind(LC_MgdPastureFrac_Y_AEZ[,1:3],apply(LC_MgdPastureFrac_Y_AEZ[,4:21],2,function(x){x[x>0.85]=0.85;x}))
LC_bm2_R_MgdPasture_Y_AEZ_adj<-cbind(LC_bm2_R_MgdPasture_Y_AEZ[,1:3],LC_bm2_R_Pasture_Y_AEZ[,4:21]*LC_MgdPastureFrac_Y_AEZ_adj[,4:21])
#Calculate pasture yield
ag_Yield_kgm2_R_Pasture_Y_AEZ<-cbind(ag_Prod_Mt_R_Pasture_Y_AEZ[,1:3],ag_Prod_Mt_R_Pasture_Y_AEZ[,4:21]/(LC_bm2_R_MgdPasture_Y_AEZ_adj[4:21]+1e-9))

write.table(LC_bm2_R_MgdPasture_Y_AEZ_adj,file="Rdata_out/LC_bm2_R_MgdPasture_Y_AEZ_adj_23.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Yield_kgm2_R_Pasture_Y_AEZ,file="Rdata_out/ag_Yield_kgm2_R_Pasture_Y_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Multiply "managed" shares by historical pathway to get historical managed pasture
LC_bm2_R_Pasture_Yh1975_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Pasture" & LC_bm2_R_LT_Yh_AEZ$year<=1975,]
LC_MgdPastureFrac_Yh1975_AEZ_adj<-LC_MgdPastureFrac_Y_AEZ_adj[LC_MgdPastureFrac_Y_AEZ_adj$year==1990,]
LC_MgdPastureFrac_Yh1975_AEZ_adj<-LC_MgdPastureFrac_Yh1975_AEZ_adj[rep(1:nrow(LC_MgdPastureFrac_Yh1975_AEZ_adj), times=(nrow(LC_bm2_R_Pasture_Yh1975_AEZ)/nrow(LC_MgdPastureFrac_Yh1975_AEZ_adj))), ]
LC_bm2_R_MgdPasture_Yh1975_AEZ<-cbind(LC_bm2_R_Pasture_Yh1975_AEZ[,1:3],LC_bm2_R_Pasture_Yh1975_AEZ[,4:21]*LC_MgdPastureFrac_Yh1975_AEZ_adj[,4:21])

#combine this with managed pasture in model years and write out
LC_bm2_R_MgdPasture_Yh_AEZ<-rbind(LC_bm2_R_MgdPasture_Yh1975_AEZ,LC_bm2_R_MgdPasture_Y_AEZ_adj)
LC_bm2_R_MgdPasture_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_MgdPasture_Yh_AEZ$GCAM_region_ID,LC_bm2_R_MgdPasture_Yh_AEZ$Land_Type,LC_bm2_R_MgdPasture_Yh_AEZ$year,sep="")
write.table(LC_bm2_R_MgdPasture_Yh_AEZ,file="Rdata_out/LC_bm2_R_MgdPasture_Yh_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)

#FORESTRY
#Carbon densities are divided by mature age to get net primary productivity, and used to derive exogenous yields for separating managed/unmanaged forest
#calculate veg mass of each AEZ based on above-ground carbon content of each AEZ. Assume 288 kgC/m3 of wood.
veg_kgCm2_R_LTnatveg_AEZ<-read.table("Inventory Data/Various/Rdata_in/veg_kgCm2_R_LTnatveg_AEZ.csv",header=T,sep=',')
mature_age_R_LT_AEZ<-read.table("Inventory Data/Various/Rdata_in/mature_age_R_LT_AEZ.csv",header=T,sep=',')
For_VegMass_m3m2_R_AEZ<-cbind(veg_kgCm2_R_LTnatveg_AEZ[veg_kgCm2_R_LTnatveg_AEZ$Land_Type=="AllForestLand",1:2],veg_kgCm2_R_LTnatveg_AEZ[veg_kgCm2_R_LTnatveg_AEZ$Land_Type=="AllForestLand",3:20]/288)
#calculate primary productivity (yield) as veg mass divided by the mature age
For_MatureAge_R_AEZ<-mature_age_R_LT_AEZ[mature_age_R_LT_AEZ$Land_Type=="AllForestLand",1:20]
For_Yield_m3m2_R_AEZ<-cbind(For_VegMass_m3m2_R_AEZ[,1:2],For_VegMass_m3m2_R_AEZ[,3:20]/For_MatureAge_R_AEZ[,3:20])
#Replace na's with 0
For_Yield_m3m2_R_AEZ[,3:20]<-apply(For_Yield_m3m2_R_AEZ[,3:20],2,function(x){x[is.na(x)]=0;x})

#disaggregate logging to AEZ on the basis of biomass production rates.
LC_bm2_R_Forest_Y_AEZ<-LC_bm2_R_LT_Yh_AEZ[LC_bm2_R_LT_Yh_AEZ$Land_Type=="Forest" & LC_bm2_R_LT_Yh_AEZ$year %in% For_ALL_bm3_R_Y$year,]
#step assumes two model base years
For_Yield_m3m2_R_AEZx2<-rbind(For_Yield_m3m2_R_AEZ,For_Yield_m3m2_R_AEZ)
For_potentialProd_bm3_R_Y_AEZ<-cbind(LC_bm2_R_Forest_Y_AEZ[,1:3],LC_bm2_R_Forest_Y_AEZ[,4:21]*For_Yield_m3m2_R_AEZx2[,3:20])
For_potentialProd_bm3_R_Y_AEZ$total<-apply(For_potentialProd_bm3_R_Y_AEZ[,4:21],1,sum)
For_Prodfrac_R_Y_AEZ<-cbind(For_potentialProd_bm3_R_Y_AEZ[,1:3],For_potentialProd_bm3_R_Y_AEZ[,4:21]/For_potentialProd_bm3_R_Y_AEZ$total)
For_Prod_bm3_R_Y_AEZ<-cbind(For_Prodfrac_R_Y_AEZ[,1:3],For_Prodfrac_R_Y_AEZ[,4:21]*For_ALL_bm3_R_Y$Prod_bm3)
#Get rid of values less than 1e-6
For_Prod_bm3_R_Y_AEZ<-cbind(For_Prod_bm3_R_Y_AEZ[,1:3],apply(For_Prod_bm3_R_Y_AEZ[,4:21],2,function(x){x[x<1e-6]=0;x}))
For_Prod_bm3_R_Y_AEZ$ID_R_LT_Y<-paste(For_Prod_bm3_R_Y_AEZ$GCAM_region_ID,For_Prod_bm3_R_Y_AEZ$Land_Type,For_Prod_bm3_R_Y_AEZ$year,sep="")

write.table(For_Yield_m3m2_R_AEZ,file="Rdata_out/For_Yield_m3m2_R_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(For_Prod_bm3_R_Y_AEZ,file="Rdata_out/For_Prod_bm3_R_Y_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Calculate land cover of "managed" forest as the output divided by the yield (net primary production), in each AEZ and region
LC_bm2_R_MgdForest_Y_AEZ<-cbind(For_Prod_bm3_R_Y_AEZ[,1:3],For_Prod_bm3_R_Y_AEZ[,4:21]/(For_Yield_m3m2_R_AEZx2[,3:20]+1e-9))
write.table(LC_bm2_R_MgdForest_Y_AEZ,file="Rdata_out/LC_bm2_R_MgdForest_Y_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Use historical population ratios to scale back managed forest over time
pop_1700_2005<-read.table("Inventory Data/Various/Rdata_in/pop_ratio_2000.csv",header=T,sep=',')

#subset 1990 from managed forest table
LC_bm2_R_MgdForest_1990_AEZ<-LC_bm2_R_MgdForest_Y_AEZ[LC_bm2_R_MgdForest_Y_AEZ$year==1990,]

LC_bm2_R_MgdForest_1975_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1975])
LC_bm2_R_MgdForest_1975_AEZ$year<-1975
LC_bm2_R_MgdForest_1950_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1950])
LC_bm2_R_MgdForest_1950_AEZ$year<-1950
LC_bm2_R_MgdForest_1900_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1900])
LC_bm2_R_MgdForest_1900_AEZ$year<-1900
LC_bm2_R_MgdForest_1850_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1850])
LC_bm2_R_MgdForest_1850_AEZ$year<-1850
LC_bm2_R_MgdForest_1800_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1800])
LC_bm2_R_MgdForest_1800_AEZ$year<-1800
LC_bm2_R_MgdForest_1750_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1750])
LC_bm2_R_MgdForest_1750_AEZ$year<-1750
LC_bm2_R_MgdForest_1700_AEZ<-cbind(LC_bm2_R_MgdForest_1990_AEZ[,1:3],LC_bm2_R_MgdForest_1990_AEZ[,4:21]*pop_1700_2005$pop_ratio_2000[pop_1700_2005$year==1700])
LC_bm2_R_MgdForest_1700_AEZ$year<-1700

#Build table of managed forest land use history
LC_bm2_R_MgdForest_Yh_AEZ<-rbind(LC_bm2_R_MgdForest_1700_AEZ,LC_bm2_R_MgdForest_1750_AEZ,LC_bm2_R_MgdForest_1800_AEZ,LC_bm2_R_MgdForest_1850_AEZ,LC_bm2_R_MgdForest_1900_AEZ,LC_bm2_R_MgdForest_1950_AEZ,LC_bm2_R_MgdForest_1975_AEZ,LC_bm2_R_MgdForest_Y_AEZ)
LC_bm2_R_MgdForest_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_MgdForest_Yh_AEZ$GCAM_region_ID,LC_bm2_R_MgdForest_Yh_AEZ$Land_Type,LC_bm2_R_MgdForest_Yh_AEZ$year,sep="")
write.table(LC_bm2_R_MgdForest_Yh_AEZ,file="Rdata_out/LC_bm2_R_MgdForest_Yh_AEZ_23.csv",sep=",",col.names=TRUE,row.names=FALSE)




