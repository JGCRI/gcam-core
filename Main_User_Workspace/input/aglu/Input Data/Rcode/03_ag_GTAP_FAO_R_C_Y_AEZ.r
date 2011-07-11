#disaggregate FAO cotton land into seed and fiber based on production. Calculate fractions first
ag_Prod_Mt_R_cotfib_Y<-ag_Prod_Mt_R_Cc_Y[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='Cotton_Fiber',]
ag_Prod_Mt_R_cottot_Y<-ag_Prod_Mt_R_Cc_Y[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='Cotton_Total',]
ag_Prod_Mt_R_cotseed_Y<-ag_Prod_Mt_R_cottot_Y[,1:3]
ag_Prod_Mt_R_cotseed_Y$Prod_Mt<-ag_Prod_Mt_R_cottot_Y[,4]-ag_Prod_Mt_R_cotfib_Y[,4]
ag_Prod_frac_R_cotfib_Y<-ag_Prod_Mt_R_cotfib_Y[,1:3]
#add small number to denominator to guard against dividing by zero
ag_Prod_frac_R_cotfib_Y$frac<-ag_Prod_Mt_R_cotfib_Y$Prod_Mt/(ag_Prod_Mt_R_cottot_Y$Prod_Mt+0.000001)
ag_Prod_frac_R_cotseed_Y<-ag_Prod_Mt_R_cotseed_Y[,1:3]
ag_Prod_frac_R_cotseed_Y$frac<-ag_Prod_Mt_R_cotseed_Y$Prod_Mt/(ag_Prod_Mt_R_cottot_Y$Prod_Mt+0.000001)

#Map cotton seed to oilcrop and cotton fiber to fibercrop.
ag_Prod_Mt_R_fibercrop_Y<-ag_Prod_Mt_R_Cc_Y[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='FiberCrop',1:3]
ag_Prod_Mt_R_fibercrop_Y$Prod_Mt<-ag_Prod_Mt_R_cotfib_Y$Prod_Mt+ag_Prod_Mt_R_Cc_Y$Prod_Mt[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='FiberCrop']
ag_Prod_Mt_R_oilcrop_Y<-ag_Prod_Mt_R_Cc_Y[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='OilCrop',1:3]
ag_Prod_Mt_R_oilcrop_Y$Prod_Mt<-ag_Prod_Mt_R_cotseed_Y$Prod_Mt+ag_Prod_Mt_R_Cc_Y$Prod_Mt[ag_Prod_Mt_R_Cc_Y$GCAM_commodity=='OilCrop']
ag_Prod_Mt_R_C_Y<-rbind(ag_Prod_Mt_R_Cc_Y[ag_Prod_Mt_R_Cc_Y$GCAM_commodity!='Cotton_Fiber' & ag_Prod_Mt_R_Cc_Y$GCAM_commodity!='Cotton_Total' & ag_Prod_Mt_R_Cc_Y$GCAM_commodity!='FiberCrop' & ag_Prod_Mt_R_Cc_Y$GCAM_commodity!='OilCrop',],ag_Prod_Mt_R_fibercrop_Y,ag_Prod_Mt_R_oilcrop_Y)
#sort by year, crop name, and region ID
ag_Prod_Mt_R_C_Y<-ag_Prod_Mt_R_C_Y[order(ag_Prod_Mt_R_C_Y$year,ag_Prod_Mt_R_C_Y$GCAM_commodity,ag_Prod_Mt_R_C_Y$GCAM_region_ID),]

#For harvested area, multiply cotton seed  and cotton fiber areas by respective production portions, and then add to fibercrop and oilcrop
ag_HA_bm2_R_cotfib_Y<-ag_HA_bm2_R_Cc_Y[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='Cotton_Total',1:3]
ag_HA_bm2_R_cotfib_Y$HA_bm2<-ag_HA_bm2_R_Cc_Y$HA_bm2[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='Cotton_Total']*ag_Prod_frac_R_cotfib_Y$frac
ag_HA_bm2_R_cotseed_Y<-ag_HA_bm2_R_Cc_Y[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='Cotton_Total',1:3]
ag_HA_bm2_R_cotseed_Y$HA_bm2<-ag_HA_bm2_R_Cc_Y$HA_bm2[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='Cotton_Total']*ag_Prod_frac_R_cotseed_Y$frac
ag_HA_bm2_R_fibercrop_Y<-ag_HA_bm2_R_Cc_Y[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='FiberCrop',1:3]
ag_HA_bm2_R_fibercrop_Y$HA_bm2<-ag_HA_bm2_R_Cc_Y$HA_bm2[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='FiberCrop']+ag_HA_bm2_R_cotfib_Y$HA_bm2
ag_HA_bm2_R_oilcrop_Y<-ag_HA_bm2_R_Cc_Y[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='OilCrop',1:3]
ag_HA_bm2_R_oilcrop_Y$HA_bm2<-ag_HA_bm2_R_Cc_Y$HA_bm2[ag_HA_bm2_R_Cc_Y$GCAM_commodity=='OilCrop']+ag_HA_bm2_R_cotseed_Y$HA_bm2
ag_HA_bm2_R_C_Y<-rbind(ag_HA_bm2_R_Cc_Y[ag_HA_bm2_R_Cc_Y$GCAM_commodity!='Cotton_Total' & ag_HA_bm2_R_Cc_Y$GCAM_commodity!='FiberCrop' & ag_HA_bm2_R_Cc_Y$GCAM_commodity!='OilCrop',],ag_HA_bm2_R_fibercrop_Y,ag_HA_bm2_R_oilcrop_Y)   
#sort by year, crop name, region ID
ag_HA_bm2_R_C_Y<-ag_HA_bm2_R_C_Y[order(ag_HA_bm2_R_C_Y$year,ag_HA_bm2_R_C_Y$GCAM_commodity,ag_HA_bm2_R_C_Y$GCAM_region_ID),]

#Disaggregate cotton to fibercrop and oilcrop in the GTAP data.
#GTAP ag_Production - mass
ag_Prod_Mt_R_cotfib_AEZ<-cbind(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='Cotton',1:2],(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='Cotton',3:20]*ag_Prod_frac_R_cotfib_Y[,ncol(ag_Prod_frac_R_cotfib_Y)]))
ag_Prod_Mt_R_cotseed_AEZ<-cbind(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='Cotton',1:2],(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='Cotton',3:20]*ag_Prod_frac_R_cotseed_Y[,ncol(ag_Prod_frac_R_cotseed_Y)]))
ag_Prod_Mt_R_fibercrop_AEZ<-cbind(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='FiberCrop',1:2],(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='FiberCrop',3:20]+ag_Prod_Mt_R_cotfib_AEZ[3:20]))
ag_Prod_Mt_R_oilcrop_AEZ<-cbind(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='OilCrop',1:2],(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity=='OilCrop',3:20]+ag_Prod_Mt_R_cotseed_AEZ[3:20]))
ag_Prod_Mt_R_C_AEZ<-rbind(ag_Prod_Mt_R_Cc_AEZ[ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity!='Cotton' & ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity!='FiberCrop' & ag_Prod_Mt_R_Cc_AEZ$GCAM_commodity!='OilCrop',],ag_Prod_Mt_R_fibercrop_AEZ,ag_Prod_Mt_R_oilcrop_AEZ)
#Sort by crop name
ag_Prod_Mt_R_C_AEZ<-ag_Prod_Mt_R_C_AEZ[order(ag_Prod_Mt_R_C_AEZ$GCAM_commodity,ag_Prod_Mt_R_C_AEZ$GCAM_region_ID),]

#GTAP Harvested Area
ag_HA_bm2_R_cotfib_AEZ<-cbind(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='Cotton',1:2],(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='Cotton',3:20]*ag_Prod_frac_R_cotfib_Y[,ncol(ag_Prod_frac_R_cotfib_Y)]))
ag_HA_bm2_R_cotseed_AEZ<-cbind(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='Cotton',1:2],(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='Cotton',3:20]*ag_Prod_frac_R_cotseed_Y[,ncol(ag_Prod_frac_R_cotseed_Y)]))
ag_HA_bm2_R_fibercrop_AEZ<-cbind(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='FiberCrop',1:2],(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='FiberCrop',3:20]+ag_HA_bm2_R_cotfib_AEZ[3:20]))
ag_HA_bm2_R_oilcrop_AEZ<-cbind(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='OilCrop',1:2],(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity=='OilCrop',3:20]+ag_HA_bm2_R_cotseed_AEZ[3:20]))
ag_HA_bm2_R_C_AEZ<-rbind(ag_HA_bm2_R_Cc_AEZ[ag_HA_bm2_R_Cc_AEZ$GCAM_commodity!='Cotton' & ag_HA_bm2_R_Cc_AEZ$GCAM_commodity!='FiberCrop' & ag_HA_bm2_R_Cc_AEZ$GCAM_commodity!='OilCrop',],ag_HA_bm2_R_fibercrop_AEZ,ag_HA_bm2_R_oilcrop_AEZ)
#sort by crop name
ag_HA_bm2_R_C_AEZ<-ag_HA_bm2_R_C_AEZ[order(ag_HA_bm2_R_C_AEZ$GCAM_commodity,ag_HA_bm2_R_C_AEZ$GCAM_region_ID),]

#Print out tables with cotton aggregated into fibercrop and oilcrop
write.table(ag_HA_bm2_R_C_Y,file="Rdata_out/ag_HA_bm2_R_C_Y_03.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Prod_Mt_R_C_Y,file="Rdata_out/ag_Prod_Mt_R_C_Y_03.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_HA_bm2_R_C_AEZ,file="Rdata_out/ag_HA_bm2_R_C_AEZ_03.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Prod_Mt_R_C_AEZ,file="Rdata_out/ag_Prod_Mt_R_C_AEZ_03.csv",sep=",",col.names=TRUE,row.names=FALSE)

#create tables with 1990 and 2005 crop production and harvested area by AEZ.
#production (Mt)
ag_Prod_Mt_R_C_AEZ_sum<-apply(ag_Prod_Mt_R_C_AEZ[,3:20],1,sum)
ag_Prod_frac_R_C_AEZ<-cbind(ag_Prod_Mt_R_C_AEZ[,1:2],(ag_Prod_Mt_R_C_AEZ[,3:20]/ag_Prod_Mt_R_C_AEZ_sum))
#NOTE: STEP IS SET FOR TWO BASE YEARS.
ag_Prod_frac_R_C_AEZx2<-rbind(ag_Prod_frac_R_C_AEZ,ag_Prod_frac_R_C_AEZ)
ag_Prod_Mt_R_C_Y_AEZ<-cbind(ag_Prod_Mt_R_C_Y[,1:3],ag_Prod_Mt_R_C_Y$Prod_Mt*ag_Prod_frac_R_C_AEZx2[,3:20])

#harvested area (bm2)
ag_HA_bm2_R_C_AEZ_sum<-apply(ag_HA_bm2_R_C_AEZ[,3:ncol(ag_HA_bm2_R_C_AEZ)],1,sum)
ag_HA_frac_R_C_AEZ<-cbind(ag_HA_bm2_R_C_AEZ[,1:2],(ag_HA_bm2_R_C_AEZ[,3:ncol(ag_HA_bm2_R_C_AEZ)]/ag_HA_bm2_R_C_AEZ_sum))
#NOTE: STEP IS SET FOR TWO BASE YEARS.
ag_HA_frac_R_C_AEZx2<-rbind(ag_HA_frac_R_C_AEZ,ag_HA_frac_R_C_AEZ)
ag_HA_bm2_R_C_Y_AEZ<-cbind(ag_HA_bm2_R_C_Y[,1:3],ag_HA_bm2_R_C_Y$HA_bm2*ag_HA_frac_R_C_AEZx2[,3:20])

#calculate yield in tonnes per hectare
ag_Yield_kgm2_R_C_Y_AEZ<-cbind(ag_Prod_Mt_R_C_Y_AEZ[,1:3],(ag_Prod_Mt_R_C_Y_AEZ[,4:21]/(ag_HA_bm2_R_C_Y_AEZ[,4:21]+1e-9)))

#replace all NaN values with 0 in all tables to be written out
ag_Prod_Mt_R_C_Y_AEZ<-cbind(ag_Prod_Mt_R_C_Y_AEZ[,1:3],apply(ag_Prod_Mt_R_C_Y_AEZ[,4:21],2,function(x){x[x=="NaN"]=0;x}))
ag_HA_bm2_R_C_Y_AEZ<-cbind(ag_HA_bm2_R_C_Y_AEZ[,1:3],apply(ag_HA_bm2_R_C_Y_AEZ[,4:21],2,function(x){x[x=="NaN"]=0;x}))
ag_Yield_kgm2_R_C_Y_AEZ<-cbind(ag_Yield_kgm2_R_C_Y_AEZ[,1:3],apply(ag_Yield_kgm2_R_C_Y_AEZ[,4:21],2,function(x){x[x=="NaN"]=0;x}))

#Add ID vector to HA table
ag_HA_bm2_R_C_Y_AEZ$ID_R_C_Y<-paste(ag_HA_bm2_R_C_Y_AEZ$GCAM_region_ID,ag_HA_bm2_R_C_Y_AEZ$GCAM_commodity,ag_HA_bm2_R_C_Y_AEZ$year,sep="")

#write out final tables
write.table(ag_Prod_Mt_R_C_Y_AEZ,file="Rdata_out/ag_Prod_Mt_R_C_Y_AEZ_03.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_HA_bm2_R_C_Y_AEZ,file="Rdata_out/ag_HA_bm2_R_C_Y_AEZ_03.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Yield_kgm2_R_C_Y_AEZ,file="Rdata_out/ag_Yield_kgm2_R_C_Y_AEZ_03.csv",sep=",",col.names=TRUE,row.names=FALSE)

