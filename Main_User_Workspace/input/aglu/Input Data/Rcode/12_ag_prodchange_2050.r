ag_CROSIT_2005_2050<-read.table("Inventory Data/FAO/Rdata_in/ag_CROSIT_2005_2050.csv",header=T,sep=',')
ctry_reg_CROSIT<-read.table("Inventory Data/FAO/Rdata_in/ctry_reg_CROSIT.csv",header=T,sep=',')
crop_commodity_CROSIT<-read.table("Inventory Data/FAO/Rdata_in/crop_commodity_CROSIT.csv",header=T,sep=',')

#add identifier vector for region and GCAM commodity
ag_CROSIT_2005_2050$GCAM_region_ID<-ctry_reg_CROSIT$GCAM_region_ID[match(ag_CROSIT_2005_2050$country_ID,ctry_reg_CROSIT$country_ID)]
ag_CROSIT_2005_2050$GCAM_commodity<-crop_commodity_CROSIT$GCAM_commodity[match(ag_CROSIT_2005_2050$crop_ID,crop_commodity_CROSIT$crop_ID)]

#aggregate by GCAM commodity and re-name the new columns
ag_GCAM_2005_2050<-aggregate(ag_CROSIT_2005_2050[,c(4,6:7,9:10,12)],by=list(ag_CROSIT_2005_2050$GCAM_region_ID,ag_CROSIT_2005_2050$GCAM_commodity,ag_CROSIT_2005_2050$year),FUN=sum)
names(ag_GCAM_2005_2050)[names(ag_GCAM_2005_2050)=="Group.1"]<-"GCAM_region_ID"
names(ag_GCAM_2005_2050)[names(ag_GCAM_2005_2050)=="Group.2"]<-"GCAM_commodity"
names(ag_GCAM_2005_2050)[names(ag_GCAM_2005_2050)=="Group.3"]<-"year"
#Remove "na" crops
ag_GCAM_2005_2050<-ag_GCAM_2005_2050[ag_GCAM_2005_2050$GCAM_commodity!="na",]

#recalculate the yields as production divided by harvested area
ag_GCAM_2005_2050$Yield_tHa_rainfed<-ag_GCAM_2005_2050$Prod_kt_rainfed/ag_GCAM_2005_2050$HA_kHa_rainfed
ag_GCAM_2005_2050$Yield_tHa_irrigated<-ag_GCAM_2005_2050$Prod_kt_irrigated/ag_GCAM_2005_2050$HA_kHa_irrigated
ag_GCAM_2005_2050$Yield_tHa<-ag_GCAM_2005_2050$Prod_kt/ag_GCAM_2005_2050$HA_kHa

#subset the yields
ag_Yield_tHa_2005<-ag_GCAM_2005_2050[ag_GCAM_2005_2050$year==2005,c(1:3,10:12)]
ag_Yield_tHa_2030<-ag_GCAM_2005_2050[ag_GCAM_2005_2050$year==2030,c(1:3,10:12)]
ag_Yield_tHa_2050<-ag_GCAM_2005_2050[ag_GCAM_2005_2050$year==2050,c(1:3,10:12)]

#calculate yield improvement rates. set "NA" values to 0
ag_Yield_rate_2030<-cbind(ag_Yield_tHa_2030[,1:3],(ag_Yield_tHa_2030[,4:6]/ag_Yield_tHa_2005[,4:6])^(1/25)-1)
ag_Yield_rate_2030<-cbind(ag_Yield_rate_2030[,1:3],apply(ag_Yield_rate_2030[,4:6],2,function(x){x[x=="NaN"]=0;x}))
ag_Yield_rate_2030<-cbind(ag_Yield_rate_2030[,1:3],apply(ag_Yield_rate_2030[,4:6],2,function(x){x[x==-1]=0;x}))
names(ag_Yield_rate_2030)[names(ag_Yield_rate_2030)=="Yield_tHa_rainfed"]<-"Yield_rainfed"
names(ag_Yield_rate_2030)[names(ag_Yield_rate_2030)=="Yield_tHa_irrigated"]<-"Yield_irrigated"
names(ag_Yield_rate_2030)[names(ag_Yield_rate_2030)=="Yield_tHa"]<-"Yield_total"

ag_Yield_rate_2050<-cbind(ag_Yield_tHa_2050[,1:3],(ag_Yield_tHa_2050[,4:6]/ag_Yield_tHa_2030[,4:6])^(1/20)-1)
ag_Yield_rate_2050<-cbind(ag_Yield_rate_2050[,1:3],apply(ag_Yield_rate_2050[,4:6],2,function(x){x[x=="NaN"]=0;x}))
ag_Yield_rate_2050<-cbind(ag_Yield_rate_2050[,1:3],apply(ag_Yield_rate_2050[,4:6],2,function(x){x[x==-1]=0;x}))
names(ag_Yield_rate_2050)[names(ag_Yield_rate_2050)=="Yield_tHa_rainfed"]<-"Yield_rainfed"
names(ag_Yield_rate_2050)[names(ag_Yield_rate_2050)=="Yield_tHa_irrigated"]<-"Yield_irrigated"
names(ag_Yield_rate_2050)[names(ag_Yield_rate_2050)=="Yield_tHa"]<-"Yield_total"

write.table(ag_Yield_rate_2030,file="Rdata_out/ag_Yield_rate_2030.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Yield_rate_2050,file="Rdata_out/ag_Yield_rate_2050.csv",sep=",",col.names=TRUE,row.names=FALSE)

write.table(ag_GCAM_2005_2050,file="Rdata_out/ag_GCAM_2005_2050.csv",sep=",",col.names=TRUE,row.names=FALSE)

