#Read in table of residue biomass parameters for FAO crop types
ag_resbio_data<-read.table("Inventory Data/Various/Rdata_in/ag_resbio_data.csv",header=T,sep=',')
ag_items_resbio<-read.table("Inventory Data/Various/Rdata_in/ag_items_resbio.csv",header=T,sep=',')

#Compute weighted averages of each parameter (HarvestIndex, ErosionControl, and ResidueEnergyContent) for each crop type in each GCAM region
#Note that cotton is not kept separate here (mapped to oilcrop)
ag_resbio_Prod<-ag_Prod_t_PRODSTAT[,1:2]
ag_resbio_Prod$Prod<-ag_Prod_t_PRODSTAT$X2005
ag_resbio_Prod$HarvestIndex<-ag_resbio_data$HarvestIndex[match(ag_resbio_Prod$item,ag_resbio_data$item)]
ag_resbio_Prod$ErosCtrl_tHa<-ag_resbio_data$ErosCtrl_tHa[match(ag_resbio_Prod$item,ag_resbio_data$item)]
ag_resbio_Prod$ResEnergy_GJt<-ag_resbio_data$ResEnergy_GJt[match(ag_resbio_Prod$item,ag_resbio_data$item)]
ag_resbio_Prod$Root_Shoot<-ag_resbio_data$Root_Shoot[match(ag_resbio_Prod$item,ag_resbio_data$item)]
ag_resbio_Prod$WaterContent<-ag_resbio_data$WaterContent[match(ag_resbio_Prod$item,ag_resbio_data$item)]

#Drop rows with NA values (dropping commodities with no residue content)
ag_resbio_Prod<-ag_resbio_Prod[!is.na(ag_resbio_Prod$HarvestIndex),]

#Multiply by production to get weights
ag_resbio_Prod$ProdxHarvestIndex<-ag_resbio_Prod$Prod*ag_resbio_Prod$HarvestIndex
ag_resbio_Prod$ProdxErosCtrl_tHa<-ag_resbio_Prod$Prod*ag_resbio_Prod$ErosCtrl_tHa
ag_resbio_Prod$ProdxResEnergy_GJt<-ag_resbio_Prod$Prod*ag_resbio_Prod$ResEnergy_GJt
ag_resbio_Prod$ProdxRoot_Shoot<-ag_resbio_Prod$Prod*ag_resbio_Prod$Root_Shoot
ag_resbio_Prod$ProdxWaterContent<-ag_resbio_Prod$Prod*ag_resbio_Prod$WaterContent

#Add vectors for GCAM regions and commodities, collapse, and divide by production to get residue biomass values
ag_resbio_Prod$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_resbio_Prod$countries,ctry_reg_FAO$countries)]
ag_resbio_Prod$GCAM_commodity<-ag_resbio_data$GCAM_commodity[match(ag_resbio_Prod$item,ag_resbio_data$item)]
ag_resbio_Prod_R_C<-aggregate(ag_resbio_Prod[,c(3,9:13)],list(GCAM_region_ID=ag_resbio_Prod$GCAM_region_ID,GCAM_commodity=ag_resbio_Prod$GCAM_commodity),sum)
ag_resbio_Prod_R_C<-ag_resbio_Prod_R_C[ag_resbio_Prod_R_C$GCAM_commodity!="na",]

ag_resbio_R_C<-ag_resbio_Prod_R_C[,1:2]
ag_resbio_R_C$HarvestIndex<-ag_resbio_Prod_R_C$ProdxHarvestIndex/(ag_resbio_Prod_R_C$Prod+1e-6)
ag_resbio_R_C$ErosCtrl_tHa<-ag_resbio_Prod_R_C$ProdxErosCtrl_tHa/(ag_resbio_Prod_R_C$Prod+1e-6)
ag_resbio_R_C$ResEnergy_GJt<-ag_resbio_Prod_R_C$ProdxResEnergy_GJt/(ag_resbio_Prod_R_C$Prod+1e-6)
ag_resbio_R_C$Root_Shoot<-ag_resbio_Prod_R_C$ProdxRoot_Shoot/(ag_resbio_Prod_R_C$Prod+1e-6)
ag_resbio_R_C$WaterContent<-ag_resbio_Prod_R_C$ProdxWaterContent/(ag_resbio_Prod_R_C$Prod+1e-6)

#write out table of ag residue biomass production
write.table(ag_resbio_R_C,file="Rdata_out/ag_resbio_R_C_11.csv",sep=",",col.names=TRUE,row.names=FALSE)







