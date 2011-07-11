#import data from CSV files
an_items_cal_SUA<-read.table("Inventory Data/FAO/Rdata_in/an_items_cal_SUA.csv",header=T,sep=',')
an_items_PRODSTAT<-read.table("Inventory Data/FAO/Rdata_in/an_items_PRODSTAT.csv",header=T,sep=',')
an_Food_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/an_Food_t_SUA.csv",header=T,sep=',')
an_Prod_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/an_Prod_t_SUA.csv",header=T,sep=',')

#add lookup vectors for GCAM regions
an_Food_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(an_Food_t_SUA$countries,ctry_reg_FAO$countries)]
an_Prod_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(an_Prod_t_SUA$countries,ctry_reg_FAO$countries)]

#add lookup vector for GCAM commodity names
an_Food_t_SUA$GCAM_commodity<-an_items_cal_SUA$GCAM_commodity[match(an_Food_t_SUA$item,an_items_cal_SUA$item)]
an_Prod_t_SUA$GCAM_commodity<-an_items_cal_SUA$GCAM_commodity[match(an_Prod_t_SUA$item,an_items_cal_SUA$item)]

#add lookup vector for calorie conversions
an_Food_t_SUA$Mcal_t<-an_items_cal_SUA$Mcal_t[match(an_Food_t_SUA$item,an_items_cal_SUA$item)]

#build table for consumption in terms of calories
#Need to add 0.0 so that R treats numbers as doubles rather than integers (would exceed max integer value otherwise)
an_Food_Mcal_SUA<-an_Food_t_SUA$Mcal_t * (an_Food_t_SUA[,3:12]+0.0)
an_Food_Mcal_SUA<-cbind(an_Food_t_SUA[,1:2],an_Food_Mcal_SUA,an_Food_t_SUA[,13:14])

#build tables collapsed by GCAM region and nonfood item name
an_Food_t_R_C_year<-aggregate(an_Food_t_SUA[,3:12],by=list(an_Food_t_SUA$GCAM_region_ID,an_Food_t_SUA$GCAM_commodity),FUN=sum)
names(an_Food_t_R_C_year)[names(an_Food_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(an_Food_t_R_C_year)[names(an_Food_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
an_Food_t_R_C_year<-an_Food_t_R_C_year[an_Food_t_R_C_year$GCAM_commodity!="na",]
an_Food_Mcal_R_C_year<-aggregate(an_Food_Mcal_SUA[,3:12],by=list(an_Food_Mcal_SUA$GCAM_region_ID,an_Food_Mcal_SUA$GCAM_commodity),FUN=sum)
names(an_Food_Mcal_R_C_year)[names(an_Food_Mcal_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(an_Food_Mcal_R_C_year)[names(an_Food_Mcal_R_C_year)=="Group.2"]<-"GCAM_commodity"
an_Food_Mcal_R_C_year<-an_Food_Mcal_R_C_year[an_Food_Mcal_R_C_year$GCAM_commodity!="na",]
an_Prod_t_R_C_year<-aggregate(an_Prod_t_SUA[,3:12],by=list(an_Prod_t_SUA$GCAM_region_ID,an_Prod_t_SUA$GCAM_commodity),FUN=sum)
names(an_Prod_t_R_C_year)[names(an_Prod_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(an_Prod_t_R_C_year)[names(an_Prod_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
an_Prod_t_R_C_year<-an_Prod_t_R_C_year[an_Prod_t_R_C_year$GCAM_commodity!="na",]

#Average first five years and last five years, and convert years to rows
#NOTE: THIS STEP ASSUMES TWO MODEL BASE YEARS
an_Food_t_R_C_1990<-an_Food_t_R_C_year[,1:2]
an_Food_t_R_C_1990$year<-1990
an_Food_t_R_C_1990$Food_t<-apply(an_Food_t_R_C_year[,3:7],1,mean)
an_Food_t_R_C_2005<-an_Food_t_R_C_year[,1:2]
an_Food_t_R_C_2005$year<-2005
an_Food_t_R_C_2005$Food_t<-apply(an_Food_t_R_C_year[,8:12],1,mean)
an_Food_t_R_C_Y<-rbind(an_Food_t_R_C_1990,an_Food_t_R_C_2005)

an_Food_Mcal_R_C_1990<-an_Food_Mcal_R_C_year[,1:2]
an_Food_Mcal_R_C_1990$year<-1990
an_Food_Mcal_R_C_1990$Food_Mcal<-apply(an_Food_Mcal_R_C_year[,3:7],1,mean)
an_Food_Mcal_R_C_2005<-an_Food_Mcal_R_C_year[,1:2]
an_Food_Mcal_R_C_2005$year<-2005
an_Food_Mcal_R_C_2005$Food_Mcal<-apply(an_Food_Mcal_R_C_year[,8:12],1,mean)
an_Food_Mcal_R_C_Y<-rbind(an_Food_Mcal_R_C_1990,an_Food_Mcal_R_C_2005)

an_Prod_t_R_C_1990<-an_Prod_t_R_C_year[,1:2]
an_Prod_t_R_C_1990$year<-1990
an_Prod_t_R_C_1990$Prod_t<-apply(an_Prod_t_R_C_year[,3:7],1,mean)
an_Prod_t_R_C_2005<-an_Prod_t_R_C_year[,1:2]
an_Prod_t_R_C_2005$year<-2005
an_Prod_t_R_C_2005$Prod_t<-apply(an_Prod_t_R_C_year[,8:12],1,mean)
an_Prod_t_R_C_Y<-rbind(an_Prod_t_R_C_1990,an_Prod_t_R_C_2005)

#Convert to desired units and add identifier vectors (Mt, Pcal)
an_Food_Mt_R_C_Y<-an_Food_t_R_C_Y[,1:3]
an_Food_Mt_R_C_Y$Food_Mt<-an_Food_t_R_C_Y$Food_t/1000000
an_Food_Mt_R_C_Y$ID_R_C_Y<-paste(an_Food_Mt_R_C_Y$GCAM_region_ID,an_Food_Mt_R_C_Y$GCAM_commodity,an_Food_Mt_R_C_Y$year,sep="")
an_Food_Pcal_R_C_Y<-an_Food_Mcal_R_C_Y[,1:3]
an_Food_Pcal_R_C_Y$Food_Pcal<-an_Food_Mcal_R_C_Y$Food_Mcal/1000000000
an_Food_Pcal_R_C_Y$ID_R_C_Y<-paste(an_Food_Pcal_R_C_Y$GCAM_region_ID,an_Food_Pcal_R_C_Y$GCAM_commodity,an_Food_Pcal_R_C_Y$year,sep="")
an_Prod_Mt_R_C_Y<-an_Prod_t_R_C_Y[,1:3]
an_Prod_Mt_R_C_Y$Prod_Mt<-an_Prod_t_R_C_Y$Prod_t/1000000
an_Prod_Mt_R_C_Y$ID_R_C_Y<-paste(an_Prod_Mt_R_C_Y$GCAM_region_ID,an_Prod_Mt_R_C_Y$GCAM_commodity,an_Prod_Mt_R_C_Y$year,sep="")

#Calculate Mt to Pcal conversion for each region and animal type
an_kcalg_R_C_Y<-an_Food_Mt_R_C_Y[,1:3]
an_kcalg_R_C_Y$kcalg<-an_Food_Pcal_R_C_Y$Food_Pcal/an_Food_Mt_R_C_Y$Food_Mt
an_kcalg_R_C_Y$ID_R_C_Y<-paste(an_kcalg_R_C_Y$GCAM_region_ID,an_kcalg_R_C_Y$GCAM_commodity,an_kcalg_R_C_Y$year,sep="")

#write tables as CSV files
write.table(an_Food_Mt_R_C_Y,file="Rdata_out/an_Food_Mt_R_C_Y_05.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(an_Food_Pcal_R_C_Y,file="Rdata_out/an_Food_Pcal_R_C_Y_05.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(an_kcalg_R_C_Y,file="Rdata_out/an_kcalg_R_C_Y_05.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(an_Prod_Mt_R_C_Y,file="Rdata_out/an_Prod_Mt_R_C_Y_05.csv",sep=",",col.names=TRUE,row.names=FALSE)
           