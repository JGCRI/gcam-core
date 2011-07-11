#Methods are the same for ag and animal products.
#AGRICULTURAL ITEMS
#import data from CSV files
ag_Exp_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/ag_Exp_t_SUA.csv",header=T,sep=',')
ag_Imp_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/ag_Imp_t_SUA.csv",header=T,sep=',')

#add lookup vectors for GCAM regions
ag_Exp_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_Exp_t_SUA$countries,ctry_reg_FAO$countries)]
ag_Imp_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_Imp_t_SUA$countries,ctry_reg_FAO$countries)]
#add lookup vectors for GCAM commodities
ag_Exp_t_SUA$GCAM_commodity<-ag_items_cal_SUA$GCAM_commodity[match(ag_Exp_t_SUA$item,ag_items_cal_SUA$item)]
ag_Imp_t_SUA$GCAM_commodity<-ag_items_cal_SUA$GCAM_commodity[match(ag_Imp_t_SUA$item,ag_items_cal_SUA$item)]

#build tables collapsed by GCAM region and commodity
# exports
ag_Exp_t_R_C_year<-aggregate(ag_Exp_t_SUA[,3:12],by=list(ag_Exp_t_SUA$GCAM_region_ID,ag_Exp_t_SUA$GCAM_commodity),FUN=sum)
names(ag_Exp_t_R_C_year)[names(ag_Exp_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Exp_t_R_C_year)[names(ag_Exp_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
ag_Exp_t_R_C_year<-ag_Exp_t_R_C_year[ag_Exp_t_R_C_year$GCAM_commodity!="na",]
# imports
ag_Imp_t_R_C_year<-aggregate(ag_Imp_t_SUA[,3:12],by=list(ag_Imp_t_SUA$GCAM_region_ID,ag_Imp_t_SUA$GCAM_commodity),FUN=sum)
names(ag_Imp_t_R_C_year)[names(ag_Imp_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Imp_t_R_C_year)[names(ag_Imp_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
ag_Imp_t_R_C_year<-ag_Imp_t_R_C_year[ag_Imp_t_R_C_year$GCAM_commodity!="na",]

#Average first five years and last five years, convert years to rows, and put imports and exports into the same tables
#NOTE: THIS STEP ASSUMES TWO MODEL BASE YEARS
ag_NetExp_t_R_C_1990<-ag_Exp_t_R_C_year[,1:2]
ag_NetExp_t_R_C_1990$year<-1990
ag_NetExp_t_R_C_1990$Exp_t<-apply(ag_Exp_t_R_C_year[,3:7],1,mean)
ag_NetExp_t_R_C_1990$Imp_t<-apply(ag_Imp_t_R_C_year[,3:7],1,mean)
ag_NetExp_t_R_C_1990$NetExp_t<-ag_NetExp_t_R_C_1990$Exp_t - ag_NetExp_t_R_C_1990$Imp_t
ag_NetExp_t_R_C_2005<-ag_Exp_t_R_C_year[,1:2]
ag_NetExp_t_R_C_2005$year<-2005
ag_NetExp_t_R_C_2005$Exp_t<-apply(ag_Exp_t_R_C_year[,8:12],1,mean)
ag_NetExp_t_R_C_2005$Imp_t<-apply(ag_Imp_t_R_C_year[,8:12],1,mean)
ag_NetExp_t_R_C_2005$NetExp_t<-ag_NetExp_t_R_C_2005$Exp_t - ag_NetExp_t_R_C_2005$Imp_t
ag_NetExp_t_R_C_Y<-rbind(ag_NetExp_t_R_C_1990,ag_NetExp_t_R_C_2005)

#Net exports must add to zero globally. Sum by year and crop type, and calculate global scalers for exports of each crop.
#NOTE: giving precedence to imports (rather than exports) of each crop. This is arbitrary but of little consequence, and generally reduces amount of trade.
ag_NetExp_t_C_Y<-aggregate(ag_NetExp_t_R_C_Y[,4:6],by=list(ag_NetExp_t_R_C_Y$GCAM_commodity,ag_NetExp_t_R_C_Y$year),FUN=sum)
names(ag_NetExp_t_C_Y)[names(ag_NetExp_t_C_Y)=="Group.1"]<-"GCAM_commodity"
names(ag_NetExp_t_C_Y)[names(ag_NetExp_t_C_Y)=="Group.2"]<-"year"
ag_NetExp_t_C_Y$Exp_scaler<-(ag_NetExp_t_C_Y$Exp_t-ag_NetExp_t_C_Y$NetExp_t)/ag_NetExp_t_C_Y$Exp_t
ag_NetExp_t_C_Y$ID_C_Y<-paste(ag_NetExp_t_C_Y$GCAM_commodity,ag_NetExp_t_C_Y$year,sep="")

#Adjust exports and recompile table
ag_NetExp_t_R_C_Y$ID_C_Y<-paste(ag_NetExp_t_R_C_Y$GCAM_commodity,ag_NetExp_t_R_C_Y$year,sep="")
ag_NetExp_t_R_C_Y$Exp_scaler<-ag_NetExp_t_C_Y$Exp_scaler[match(ag_NetExp_t_R_C_Y$ID_C_Y,ag_NetExp_t_C_Y$ID_C_Y)]
ag_NetExp_t_R_C_Y$Exp_t_adj<-ag_NetExp_t_R_C_Y$Exp_t * ag_NetExp_t_R_C_Y$Exp_scaler
ag_NetExp_t_R_C_Y$NetExp_t_adj<-ag_NetExp_t_R_C_Y$Exp_t_adj - ag_NetExp_t_R_C_Y$Imp_t
ag_NetExp_t_R_C_Y_adj<-ag_NetExp_t_R_C_Y[,1:3]
ag_NetExp_t_R_C_Y_adj$NetExp_t<-ag_NetExp_t_R_C_Y$NetExp_t_adj

#Convert to desired units (Mt) and add identifier vector 
ag_NetExp_Mt_R_C_Y_adj<-ag_NetExp_t_R_C_Y_adj[,1:3]
ag_NetExp_Mt_R_C_Y_adj$NetExp_Mt<-ag_NetExp_t_R_C_Y_adj$NetExp_t/1000000
ag_NetExp_Mt_R_C_Y_adj$ID_R_C_Y<-paste(ag_NetExp_Mt_R_C_Y_adj$GCAM_region_ID,ag_NetExp_Mt_R_C_Y_adj$GCAM_commodity,ag_NetExp_Mt_R_C_Y_adj$year,sep="")

#write table as CSV file
write.table(ag_NetExp_Mt_R_C_Y_adj,file="Rdata_out/ag_NetExp_Mt_R_C_Y_adj_06.csv",sep=",",col.names=TRUE,row.names=FALSE)

#ANIMAL ITEMS
#import data from CSV files
an_Exp_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/an_Exp_t_SUA.csv",header=T,sep=',')
an_Imp_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/an_Imp_t_SUA.csv",header=T,sep=',')

#add lookup vectors for GCAM regions
an_Exp_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(an_Exp_t_SUA$countries,ctry_reg_FAO$countries)]
an_Imp_t_SUA$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(an_Imp_t_SUA$countries,ctry_reg_FAO$countries)]
#add lookup vectors for GCAM commodities
an_Exp_t_SUA$GCAM_commodity<-an_items_cal_SUA$GCAM_commodity[match(an_Exp_t_SUA$item,an_items_cal_SUA$item)]
an_Imp_t_SUA$GCAM_commodity<-an_items_cal_SUA$GCAM_commodity[match(an_Imp_t_SUA$item,an_items_cal_SUA$item)]

#build tables collapsed by GCAM region and commodity
# exports
an_Exp_t_R_C_year<-aggregate(an_Exp_t_SUA[,3:12],by=list(an_Exp_t_SUA$GCAM_region_ID,an_Exp_t_SUA$GCAM_commodity),FUN=sum)
names(an_Exp_t_R_C_year)[names(an_Exp_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(an_Exp_t_R_C_year)[names(an_Exp_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
an_Exp_t_R_C_year<-an_Exp_t_R_C_year[an_Exp_t_R_C_year$GCAM_commodity!="na",]
# imports
an_Imp_t_R_C_year<-aggregate(an_Imp_t_SUA[,3:12],by=list(an_Imp_t_SUA$GCAM_region_ID,an_Imp_t_SUA$GCAM_commodity),FUN=sum)
names(an_Imp_t_R_C_year)[names(an_Imp_t_R_C_year)=="Group.1"]<-"GCAM_region_ID"
names(an_Imp_t_R_C_year)[names(an_Imp_t_R_C_year)=="Group.2"]<-"GCAM_commodity"
an_Imp_t_R_C_year<-an_Imp_t_R_C_year[an_Imp_t_R_C_year$GCAM_commodity!="na",]

#Average first five years and last five years, convert years to rows, and put imports and exports into the same tables
#NOTE: THIS STEP ASSUMES TWO MODEL BASE YEARS
an_NetExp_t_R_C_1990<-an_Exp_t_R_C_year[,1:2]
an_NetExp_t_R_C_1990$year<-1990
an_NetExp_t_R_C_1990$Exp_t<-apply(an_Exp_t_R_C_year[,3:7],1,mean)
an_NetExp_t_R_C_1990$Imp_t<-apply(an_Imp_t_R_C_year[,3:7],1,mean)
an_NetExp_t_R_C_1990$NetExp_t<-an_NetExp_t_R_C_1990$Exp_t - an_NetExp_t_R_C_1990$Imp_t
an_NetExp_t_R_C_2005<-an_Exp_t_R_C_year[,1:2]
an_NetExp_t_R_C_2005$year<-2005
an_NetExp_t_R_C_2005$Exp_t<-apply(an_Exp_t_R_C_year[,8:12],1,mean)
an_NetExp_t_R_C_2005$Imp_t<-apply(an_Imp_t_R_C_year[,8:12],1,mean)
an_NetExp_t_R_C_2005$NetExp_t<-an_NetExp_t_R_C_2005$Exp_t - an_NetExp_t_R_C_2005$Imp_t
an_NetExp_t_R_C_Y<-rbind(an_NetExp_t_R_C_1990,an_NetExp_t_R_C_2005)

#Calculate scalers for exports
an_NetExp_t_C_Y<-aggregate(an_NetExp_t_R_C_Y[,4:6],by=list(an_NetExp_t_R_C_Y$GCAM_commodity,an_NetExp_t_R_C_Y$year),FUN=sum)
names(an_NetExp_t_C_Y)[names(an_NetExp_t_C_Y)=="Group.1"]<-"GCAM_commodity"
names(an_NetExp_t_C_Y)[names(an_NetExp_t_C_Y)=="Group.2"]<-"year"
an_NetExp_t_C_Y$Exp_scaler<-(an_NetExp_t_C_Y$Exp_t-an_NetExp_t_C_Y$NetExp_t)/an_NetExp_t_C_Y$Exp_t
an_NetExp_t_C_Y$ID_C_Y<-paste(an_NetExp_t_C_Y$GCAM_commodity,an_NetExp_t_C_Y$year,sep="")

#Adjust exports and recompile table
an_NetExp_t_R_C_Y$ID_C_Y<-paste(an_NetExp_t_R_C_Y$GCAM_commodity,an_NetExp_t_R_C_Y$year,sep="")
an_NetExp_t_R_C_Y$Exp_scaler<-an_NetExp_t_C_Y$Exp_scaler[match(an_NetExp_t_R_C_Y$ID_C_Y,an_NetExp_t_C_Y$ID_C_Y)]
an_NetExp_t_R_C_Y$Exp_t_adj<-an_NetExp_t_R_C_Y$Exp_t * an_NetExp_t_R_C_Y$Exp_scaler
an_NetExp_t_R_C_Y$NetExp_t_adj<-an_NetExp_t_R_C_Y$Exp_t_adj - an_NetExp_t_R_C_Y$Imp_t
an_NetExp_t_R_C_Y_adj<-an_NetExp_t_R_C_Y[,1:3]
an_NetExp_t_R_C_Y_adj$NetExp_t<-an_NetExp_t_R_C_Y$NetExp_t_adj

#Convert to desired units (Mt) and add identifier vector 
an_NetExp_Mt_R_C_Y_adj<-an_NetExp_t_R_C_Y_adj[,1:3]
an_NetExp_Mt_R_C_Y_adj$NetExp_Mt<-an_NetExp_t_R_C_Y_adj$NetExp_t/1000000
an_NetExp_Mt_R_C_Y_adj$ID_R_C_Y<-paste(an_NetExp_Mt_R_C_Y_adj$GCAM_region_ID,an_NetExp_Mt_R_C_Y_adj$GCAM_commodity,an_NetExp_Mt_R_C_Y_adj$year,sep="")

#write table as CSV file
write.table(an_NetExp_Mt_R_C_Y_adj,file="Rdata_out/an_NetExp_Mt_R_C_Y_adj_06.csv",sep=",",col.names=TRUE,row.names=FALSE)
