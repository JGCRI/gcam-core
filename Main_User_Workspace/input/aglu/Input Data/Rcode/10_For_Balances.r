#Read in tables of production, consumption, and imports
For_Prod_m3_FORESTAT<-read.table("Inventory Data/FAO/Rdata_in/For_Prod_m3_FORESTAT.csv",header=T,sep=',')
For_Imp_m3_FORESTAT<-read.table("Inventory Data/FAO/Rdata_in/For_Imp_m3_FORESTAT.csv",header=T,sep=',')
For_Exp_m3_FORESTAT<-read.table("Inventory Data/FAO/Rdata_in/For_Exp_m3_FORESTAT.csv",header=T,sep=',')

#Add regionID lookup vectors and aggregate
For_Prod_m3_FORESTAT$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(For_Prod_m3_FORESTAT$countries,ctry_reg_FAO$countries)]
For_Imp_m3_FORESTAT$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(For_Imp_m3_FORESTAT$countries,ctry_reg_FAO$countries)]
For_Exp_m3_FORESTAT$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(For_Exp_m3_FORESTAT$countries,ctry_reg_FAO$countries)]
For_Prod_m3_FORESTAT$GCAM_commodity<-"Forest"
For_Imp_m3_FORESTAT$GCAM_commodity<-"Forest"
For_Exp_m3_FORESTAT$GCAM_commodity<-"Forest"

For_Prod_m3_R_year<-aggregate(For_Prod_m3_FORESTAT[,3:12],by=list(For_Prod_m3_FORESTAT$GCAM_region_ID,For_Prod_m3_FORESTAT$GCAM_commodity),FUN=sum)
names(For_Prod_m3_R_year)[names(For_Prod_m3_R_year)=="Group.1"]<-"GCAM_region_ID"
names(For_Prod_m3_R_year)[names(For_Prod_m3_R_year)=="Group.2"]<-"GCAM_commodity"
For_Imp_m3_R_year<-aggregate(For_Imp_m3_FORESTAT[,3:12],by=list(For_Imp_m3_FORESTAT$GCAM_region_ID,For_Imp_m3_FORESTAT$GCAM_commodity),FUN=sum)
names(For_Imp_m3_R_year)[names(For_Imp_m3_R_year)=="Group.1"]<-"GCAM_region_ID"
names(For_Imp_m3_R_year)[names(For_Imp_m3_R_year)=="Group.2"]<-"GCAM_commodity"
For_Exp_m3_R_year<-aggregate(For_Exp_m3_FORESTAT[,3:12],by=list(For_Exp_m3_FORESTAT$GCAM_region_ID,For_Exp_m3_FORESTAT$GCAM_commodity),FUN=sum)
names(For_Exp_m3_R_year)[names(For_Exp_m3_R_year)=="Group.1"]<-"GCAM_region_ID"
names(For_Exp_m3_R_year)[names(For_Exp_m3_R_year)=="Group.2"]<-"GCAM_commodity"

#Calculate model time periods as the average of the first five years and the last five years, and build balance. Convert to million cubic meters.
For_ALL_bm3_R_1990<-For_Prod_m3_R_year[,1:2]
For_ALL_bm3_R_1990$year<-1990
For_ALL_bm3_R_1990$Prod_bm3<-apply(For_Prod_m3_R_year[,3:7],1,mean)/1e9
For_ALL_bm3_R_1990$Imp_bm3<-apply(For_Imp_m3_R_year[,3:7],1,mean)/1e9
For_ALL_bm3_R_1990$Exp_bm3<-apply(For_Exp_m3_R_year[,3:7],1,mean)/1e9
For_ALL_bm3_R_2005<-For_Prod_m3_R_year[,1:2]
For_ALL_bm3_R_2005$year<-2005
For_ALL_bm3_R_2005$Prod_bm3<-apply(For_Prod_m3_R_year[,8:12],1,mean)/1e9
For_ALL_bm3_R_2005$Imp_bm3<-apply(For_Imp_m3_R_year[,8:12],1,mean)/1e9
For_ALL_bm3_R_2005$Exp_bm3<-apply(For_Exp_m3_R_year[,8:12],1,mean)/1e9
For_ALL_bm3_R_Y<-rbind(For_ALL_bm3_R_1990,For_ALL_bm3_R_2005)

write.table(For_ALL_bm3_R_Y,file="Rdata_out/For_ALL_bm3_R_Y_09.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Calculate consumption, and scale consumption to equal production
For_ALL_bm3_R_Y$Cons_bm3<-For_ALL_bm3_R_Y$Prod_bm3+For_ALL_bm3_R_Y$Imp_bm3-For_ALL_bm3_R_Y$Exp_bm3
For_ALL_bm3_R_Y$Cons_adj<-1
For_ALL_bm3_R_Y$Cons_adj[For_ALL_bm3_R_Y$year==1990]<-sum(For_ALL_bm3_R_Y$Prod_bm3[For_ALL_bm3_R_Y$year==1990])/sum(For_ALL_bm3_R_Y$Cons_bm3[For_ALL_bm3_R_Y$year==1990])
For_ALL_bm3_R_Y$Cons_adj[For_ALL_bm3_R_Y$year==2005]<-sum(For_ALL_bm3_R_Y$Prod_bm3[For_ALL_bm3_R_Y$year==2005])/sum(For_ALL_bm3_R_Y$Cons_bm3[For_ALL_bm3_R_Y$year==2005])
For_ALL_bm3_R_Y_adj<-For_ALL_bm3_R_Y[,1:4]
For_ALL_bm3_R_Y_adj$NetExp_bm3<-0
For_ALL_bm3_R_Y_adj$Cons_bm3<-For_ALL_bm3_R_Y$Cons_bm3*For_ALL_bm3_R_Y$Cons_adj
For_ALL_bm3_R_Y_adj$NetExp_bm3<-For_ALL_bm3_R_Y_adj$Prod_bm3-For_ALL_bm3_R_Y_adj$Cons_bm3

#Add lookup vector (region and year)
For_ALL_bm3_R_Y_adj$ID_R_Y<-paste(For_ALL_bm3_R_Y_adj$GCAM_region_ID,For_ALL_bm3_R_Y_adj$year,sep="")

write.table(For_ALL_bm3_R_Y_adj,file="Rdata_out/For_ALL_bm3_R_Y_adj_09.csv",sep=",",col.names=TRUE,row.names=FALSE)


