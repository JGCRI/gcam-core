#import data from CSV files
ctry_reg_FAO<-read.table("Inventory Data/FAO/Rdata_in/ctry_reg_FAO.csv",header=T,sep=',')
ag_items_PRODSTAT<-read.table("Inventory Data/FAO/Rdata_in/ag_items_PRODSTAT.csv",header=T,sep=',')
ag_HA_Ha_PRODSTAT<-read.table("Inventory Data/FAO/Rdata_in/ag_HA_Ha_PRODSTAT.csv",header=T,sep=',')
ag_Prod_t_PRODSTAT<-read.table("Inventory Data/FAO/Rdata_in/ag_Prod_t_PRODSTAT.csv",header=T,sep=',')
ag_items_cal_SUA<-read.table("Inventory Data/FAO/Rdata_in/ag_items_cal_SUA.csv",header=T,sep=',')
ag_Food_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/ag_Food_t_SUA.csv",header=T,sep=',')

#Divide USA alfalfa by 4 to correct for FAO's accounting error
ag_Prod_t_PRODSTAT[ag_Prod_t_PRODSTAT$countries=="United States of America"&ag_Prod_t_PRODSTAT$item=="Alfalfa for forage and silage",3:ncol(ag_Prod_t_PRODSTAT)]<-ag_Prod_t_PRODSTAT[ag_Prod_t_PRODSTAT$countries=="United States of America"&ag_Prod_t_PRODSTAT$item=="Alfalfa for forage and silage",3:ncol(ag_Prod_t_PRODSTAT)]/4

#add lookup vector for GCAM regions
ag_Food_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_Food_t_SUA$countries,ctry_reg_FAO$countries)]
ag_HA_Ha_PRODSTAT$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_HA_Ha_PRODSTAT$countries,ctry_reg_FAO$countries)]
ag_Prod_t_PRODSTAT$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_Prod_t_PRODSTAT$countries,ctry_reg_FAO$countries)]

#add lookup vectors for GCAM crop names
ag_Food_t_SUA$GCAM_commodity<-ag_items_cal_SUA$GCAM_commodity[match(ag_Food_t_SUA$item,ag_items_cal_SUA$item)]
ag_HA_Ha_PRODSTAT$GCAM_commodity<-ag_items_PRODSTAT$GCAM_commodity[match(ag_HA_Ha_PRODSTAT$item,ag_items_PRODSTAT$item)]
ag_Prod_t_PRODSTAT$GCAM_commodity<-ag_items_PRODSTAT$GCAM_commodity[match(ag_Prod_t_PRODSTAT$item,ag_items_PRODSTAT$item)]

#add lookup vectors for Mcal conversions for consumption tables
ag_Food_t_SUA$Mcal_t<-ag_items_cal_SUA$Mcal_t[match(ag_Food_t_SUA$item,ag_items_cal_SUA$item)]

#build tables for consumption in terms of Mcal
ag_Food_Mcal_SUA<-ag_Food_t_SUA$Mcal_t * ag_Food_t_SUA[,3:12]
ag_Food_Mcal_SUA<-cbind(ag_Food_t_SUA[,1:2],ag_Food_Mcal_SUA,ag_Food_t_SUA[,13:14])

#build tables collapsed by GCAM region and crop name
ag_Food_t_R_C_year<-aggregate(ag_Food_t_SUA[,3:12],by=list(ag_Food_t_SUA$GCAM_region_ID,ag_Food_t_SUA$GCAM_commodity),FUN=sum)
names(ag_Food_t_R_C_year)[names(ag_Food_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Food_t_R_C_year)[names(ag_Food_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
ag_Food_t_R_C_year<-ag_Food_t_R_C_year[ag_Food_t_R_C_year$GCAM_commodity!="na",]
ag_Food_Mcal_R_C_year<-aggregate(ag_Food_Mcal_SUA[,3:12],by=list(ag_Food_Mcal_SUA$GCAM_region_ID,ag_Food_Mcal_SUA$GCAM_commodity),FUN=sum)
names(ag_Food_Mcal_R_C_year)[names(ag_Food_Mcal_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Food_Mcal_R_C_year)[names(ag_Food_Mcal_R_C_year)=="Group.2"]<-"GCAM_commodity"
ag_Food_Mcal_R_C_year<-ag_Food_Mcal_R_C_year[ag_Food_Mcal_R_C_year$GCAM_commodity!="na",]
ag_HA_Ha_R_Cc_year<-aggregate(ag_HA_Ha_PRODSTAT[,3:12],by=list(ag_HA_Ha_PRODSTAT$GCAM_region_ID,ag_HA_Ha_PRODSTAT$GCAM_commodity),FUN=sum)
names(ag_HA_Ha_R_Cc_year)[names(ag_HA_Ha_R_Cc_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_HA_Ha_R_Cc_year)[names(ag_HA_Ha_R_Cc_year)=="Group.2"]<-"GCAM_commodity"
ag_HA_Ha_R_Cc_year<-ag_HA_Ha_R_Cc_year[ag_HA_Ha_R_Cc_year$GCAM_commodity!="na",]
ag_Prod_t_R_Cc_year<-aggregate(ag_Prod_t_PRODSTAT[,3:12],by=list(ag_Prod_t_PRODSTAT$GCAM_region_ID,ag_Prod_t_PRODSTAT$GCAM_commodity),FUN=sum)
names(ag_Prod_t_R_Cc_year)[names(ag_Prod_t_R_Cc_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Prod_t_R_Cc_year)[names(ag_Prod_t_R_Cc_year)=="Group.2"]<-"GCAM_commodity"
ag_Prod_t_R_Cc_year<-ag_Prod_t_R_Cc_year[ag_Prod_t_R_Cc_year$GCAM_commodity!="na",]

#Average five year blocks around the 1990 and 2005 model time periods
#NOTE: THIS STEP ASSUMES TWO MODEL BASE YEARS
ag_Food_t_R_C_1990<-ag_Food_t_R_C_year[,1:2]
ag_Food_t_R_C_1990$year<-1990
ag_Food_t_R_C_1990$Food_t<-apply(ag_Food_t_R_C_year[,3:7],1,mean)
ag_Food_t_R_C_2005<-ag_Food_t_R_C_year[,1:2]
ag_Food_t_R_C_2005$year<-2005
ag_Food_t_R_C_2005$Food_t<-apply(ag_Food_t_R_C_year[,8:12],1,mean)
ag_Food_t_R_C_Y<-rbind(ag_Food_t_R_C_1990,ag_Food_t_R_C_2005)

ag_Food_Mcal_R_C_1990<-ag_Food_Mcal_R_C_year[,1:2]
ag_Food_Mcal_R_C_1990$year<-1990
ag_Food_Mcal_R_C_1990$Food_Mcal<-apply(ag_Food_Mcal_R_C_year[,3:7],1,mean)
ag_Food_Mcal_R_C_2005<-ag_Food_Mcal_R_C_year[,1:2]
ag_Food_Mcal_R_C_2005$year<-2005
ag_Food_Mcal_R_C_2005$Food_Mcal<-apply(ag_Food_Mcal_R_C_year[,8:12],1,mean)
ag_Food_Mcal_R_C_Y<-rbind(ag_Food_Mcal_R_C_1990,ag_Food_Mcal_R_C_2005)

ag_HA_Ha_R_Cc_1990<-ag_HA_Ha_R_Cc_year[,1:2]
ag_HA_Ha_R_Cc_1990$year<-1990
ag_HA_Ha_R_Cc_1990$HA_Ha<-apply(ag_HA_Ha_R_Cc_year[,3:7],1,mean)
ag_HA_Ha_R_Cc_2005<-ag_HA_Ha_R_Cc_year[,1:2]
ag_HA_Ha_R_Cc_2005$year<-2005
ag_HA_Ha_R_Cc_2005$HA_Ha<-apply(ag_HA_Ha_R_Cc_year[,8:12],1,mean)
ag_HA_Ha_R_Cc_Y<-rbind(ag_HA_Ha_R_Cc_1990,ag_HA_Ha_R_Cc_2005)

ag_Prod_t_R_Cc_1990<-ag_Prod_t_R_Cc_year[,1:2]
ag_Prod_t_R_Cc_1990$year<-1990
ag_Prod_t_R_Cc_1990$Prod_t<-apply(ag_Prod_t_R_Cc_year[,3:7],1,mean)
ag_Prod_t_R_Cc_2005<-ag_Prod_t_R_Cc_year[,1:2]
ag_Prod_t_R_Cc_2005$year<-2005
ag_Prod_t_R_Cc_2005$Prod_t<-apply(ag_Prod_t_R_Cc_year[,8:12],1,mean)
ag_Prod_t_R_Cc_Y<-rbind(ag_Prod_t_R_Cc_1990,ag_Prod_t_R_Cc_2005)

#Convert to desired units (Mt, Pcal, and bm2)
ag_Food_Mt_R_C_Y<-ag_Food_t_R_C_Y[,1:3]
ag_Food_Mt_R_C_Y$Food_Mt<-ag_Food_t_R_C_Y$Food_t/1000000
ag_Food_Pcal_R_C_Y<-ag_Food_Mcal_R_C_Y[,1:3]
ag_Food_Pcal_R_C_Y$Food_Pcal<-ag_Food_Mcal_R_C_Y$Food_Mcal/1000000000
ag_HA_bm2_R_Cc_Y<-ag_HA_Ha_R_Cc_Y[,1:3]
ag_HA_bm2_R_Cc_Y$HA_bm2<-ag_HA_Ha_R_Cc_Y$HA_Ha/100000
ag_Prod_Mt_R_Cc_Y<-ag_Prod_t_R_Cc_Y[,1:3]
ag_Prod_Mt_R_Cc_Y$Prod_Mt<-ag_Prod_t_R_Cc_Y$Prod_t/1000000

#Calculate average caloric content of consumed commodities (kcal/g)
ag_kcalg_R_C_Y<-ag_Food_Pcal_R_C_Y[,1:3]
ag_kcalg_R_C_Y$kcalg<-ag_Food_Pcal_R_C_Y$Food_Pcal / ag_Food_Mt_R_C_Y$Food_Mt

#Add identifier vector to food tables
ag_Food_Mt_R_C_Y$ID_R_C_Y<-paste(ag_Food_Mt_R_C_Y$GCAM_region_ID,ag_Food_Mt_R_C_Y$GCAM_commodity,ag_Food_Mt_R_C_Y$year,sep="")
ag_Food_Pcal_R_C_Y$ID_R_C_Y<-paste(ag_Food_Pcal_R_C_Y$GCAM_region_ID,ag_Food_Pcal_R_C_Y$GCAM_commodity,ag_Food_Pcal_R_C_Y$year,sep="")
ag_kcalg_R_C_Y$ID_R_C_Y<-paste(ag_kcalg_R_C_Y$GCAM_region_ID,ag_kcalg_R_C_Y$GCAM_commodity,ag_kcalg_R_C_Y$year,sep="")

#write tables as CSV files
write.table(ag_Food_Mt_R_C_Y,file="Rdata_out/ag_Food_Mt_R_C_Y_01.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Food_Pcal_R_C_Y,file="Rdata_out/ag_Food_Pcal_R_C_Y_01.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_kcalg_R_C_Y,file="Rdata_out/ag_kcalg_R_C_Y_01.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_HA_bm2_R_Cc_Y,file="Rdata_out/ag_HA_bm2_R_Cc_Y_01.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Prod_Mt_R_Cc_Y,file="Rdata_out/ag_Prod_Mt_R_Cc_Y_01.csv",sep=",",col.names=TRUE,row.names=FALSE)
