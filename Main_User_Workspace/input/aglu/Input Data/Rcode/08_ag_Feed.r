#FODDERHERB
#Calculate FodderHerb production by region
ag_Prod_Mt_R_FodderHerb_Y<-ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity=="FodderHerb",1:3]
ag_Prod_Mt_R_FodderHerb_Y$Prod_Mt<-apply(ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity=="FodderHerb",4:21],1,sum)

#Calculate regional demands of FodderHerb as feed
an_Feed_Mt_R_F_Y<-aggregate(an_FeedIO_Mt_R_C_S_F_Y$Feed_Mt,by=list(an_FeedIO_Mt_R_C_S_F_Y$GCAM_region_ID,an_FeedIO_Mt_R_C_S_F_Y$feed,an_FeedIO_Mt_R_C_S_F_Y$year),FUN=sum)
names(an_Feed_Mt_R_F_Y)[names(an_Feed_Mt_R_F_Y)=="Group.1"]<-"GCAM_region_ID"
names(an_Feed_Mt_R_F_Y)[names(an_Feed_Mt_R_F_Y)=="Group.2"]<-"feed"
names(an_Feed_Mt_R_F_Y)[names(an_Feed_Mt_R_F_Y)=="Group.3"]<-"year"
names(an_Feed_Mt_R_F_Y)[names(an_Feed_Mt_R_F_Y)=="x"]<-"Feed_Mt"
an_Feed_Mt_R_FodderHerbResidue_Y<-an_Feed_Mt_R_F_Y[an_Feed_Mt_R_F_Y$feed=="FodderHerb_Residue",]

#Calculate residual: FodderHerb demand as feed minus FodderHerb production.
#NOTE: Foddercrop is assumed to be a global market, but actual trade is not tracked in this version (not in FAO's SUA balances)
ag_ALL_Mt_R_FodderHerb_Y<-ag_Prod_Mt_R_FodderHerb_Y[,1:2]
ag_ALL_Mt_R_FodderHerb_Y$GCAM_commodity<-"FodderHerb"
ag_ALL_Mt_R_FodderHerb_Y$year<-ag_Prod_Mt_R_FodderHerb_Y$year
ag_ALL_Mt_R_FodderHerb_Y$Prod_Mt<-ag_Prod_Mt_R_FodderHerb_Y$Prod_Mt
ag_ALL_Mt_R_FodderHerb_Y$Feed_Mt<-an_Feed_Mt_R_FodderHerbResidue_Y$Feed_Mt
ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt<-ag_ALL_Mt_R_FodderHerb_Y$Feed_Mt-ag_ALL_Mt_R_FodderHerb_Y$Prod_Mt

#First calculate difference between global fodderherb supply and demand
ag_ALL_Mt_R_FodderHerb_Y$GlobalProd_Mt<-1
ag_ALL_Mt_R_FodderHerb_Y$GlobalProd_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Prod_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990])
ag_ALL_Mt_R_FodderHerb_Y$GlobalProd_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Prod_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005])
ag_ALL_Mt_R_FodderHerb_Y$GlobalFeed_Mt<-1
ag_ALL_Mt_R_FodderHerb_Y$GlobalFeed_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Feed_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990])
ag_ALL_Mt_R_FodderHerb_Y$GlobalFeed_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Feed_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005])

#Where Prod > Feed, map difference to "other net uses" demand. Where Prod < Feed, map difference to "Crop residue" production
ag_ALL_Mt_R_FodderHerb_Y$GlobalResidual_Mt<-ag_ALL_Mt_R_FodderHerb_Y$GlobalProd_Mt - ag_ALL_Mt_R_FodderHerb_Y$GlobalFeed_Mt
ag_ALL_Mt_R_FodderHerb_Y$GlobalOtherUses_Mt<-ifelse(ag_ALL_Mt_R_FodderHerb_Y$GlobalResidual_Mt>0,ag_ALL_Mt_R_FodderHerb_Y$GlobalResidual_Mt,0)
ag_ALL_Mt_R_FodderHerb_Y$GlobalResidue_Mt<-ifelse(ag_ALL_Mt_R_FodderHerb_Y$GlobalResidual_Mt<0,-1 * ag_ALL_Mt_R_FodderHerb_Y$GlobalResidual_Mt,0)

#Assign "other net use" demand to regions according to discrepancy
ag_ALL_Mt_R_FodderHerb_Y$GlobalNegResidual_Mt<-1
ag_ALL_Mt_R_FodderHerb_Y$GlobalNegResidual_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt[ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt<0 & ag_ALL_Mt_R_FodderHerb_Y$year==1990])
ag_ALL_Mt_R_FodderHerb_Y$GlobalNegResidual_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt[ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt<0 & ag_ALL_Mt_R_FodderHerb_Y$year==2005])
ag_ALL_Mt_R_FodderHerb_Y$NegResidualFrac<-ifelse(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt < 0, ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt / ag_ALL_Mt_R_FodderHerb_Y$GlobalNegResidual_Mt,0)
ag_ALL_Mt_R_FodderHerb_Y$OtherUses_Mt<-ag_ALL_Mt_R_FodderHerb_Y$GlobalOtherUses_Mt * ag_ALL_Mt_R_FodderHerb_Y$NegResidualFrac

#Assign residue production to regions according to discrepancy
ag_ALL_Mt_R_FodderHerb_Y$GlobalPosResidual_Mt<-1
ag_ALL_Mt_R_FodderHerb_Y$GlobalPosResidual_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==1990]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt[ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt>0 & ag_ALL_Mt_R_FodderHerb_Y$year==1990])
ag_ALL_Mt_R_FodderHerb_Y$GlobalPosResidual_Mt[ag_ALL_Mt_R_FodderHerb_Y$year==2005]<-sum(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt[ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt>0 & ag_ALL_Mt_R_FodderHerb_Y$year==2005])
ag_ALL_Mt_R_FodderHerb_Y$PosResidualFrac<-ifelse(ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt > 0, ag_ALL_Mt_R_FodderHerb_Y$Residual_Mt / ag_ALL_Mt_R_FodderHerb_Y$GlobalPosResidual_Mt,0)
ag_ALL_Mt_R_FodderHerb_Y$Residue_Mt<-ag_ALL_Mt_R_FodderHerb_Y$GlobalResidue_Mt * ag_ALL_Mt_R_FodderHerb_Y$PosResidualFrac

#Rebuild balance table of fodder herb production, net exports, and all uses (food, feed, other uses).
ag_ALL_Mt_R_FodderHerb_Y_adj<-ag_ALL_Mt_R_FodderHerb_Y[,1:4]
ag_ALL_Mt_R_FodderHerb_Y_adj$NetExp_Mt<-0
ag_ALL_Mt_R_FodderHerb_Y_adj$Supply_Mt<-0
ag_ALL_Mt_R_FodderHerb_Y_adj$Food_Mt<-0
ag_ALL_Mt_R_FodderHerb_Y_adj$Feed_Mt<-ag_ALL_Mt_R_FodderHerb_Y$Feed - ag_ALL_Mt_R_FodderHerb_Y$Residue_Mt
ag_ALL_Mt_R_FodderHerb_Y_adj$OtherUses_Mt<-ag_ALL_Mt_R_FodderHerb_Y$OtherUses_Mt
ag_ALL_Mt_R_FodderHerb_Y_adj$NetExp_Mt<-ag_ALL_Mt_R_FodderHerb_Y_adj$Prod_Mt - ag_ALL_Mt_R_FodderHerb_Y_adj$Feed_Mt - ag_ALL_Mt_R_FodderHerb_Y_adj$OtherUses_Mt
ag_ALL_Mt_R_FodderHerb_Y_adj$Supply_Mt<-ag_ALL_Mt_R_FodderHerb_Y_adj$Prod_Mt - ag_ALL_Mt_R_FodderHerb_Y_adj$NetExp_Mt
ag_ALL_Mt_R_FodderHerb_Y_adj$ID_R_C_Y<-paste(ag_ALL_Mt_R_FodderHerb_Y_adj$GCAM_region_ID,ag_ALL_Mt_R_FodderHerb_Y_adj$GCAM_commodity,ag_ALL_Mt_R_FodderHerb_Y_adj$year,sep="")

#Build table of residue production and consumption
ag_ALL_Mt_R_Residue_Y<-ag_ALL_Mt_R_FodderHerb_Y_adj[,1:3]
ag_ALL_Mt_R_Residue_Y$GCAM_commodity<-"Residue"
ag_ALL_Mt_R_Residue_Y$Prod_Mt<-ag_ALL_Mt_R_FodderHerb_Y$Residue_Mt
ag_ALL_Mt_R_Residue_Y$NetExp_Mt<-0
ag_ALL_Mt_R_Residue_Y$Supply_Mt<-0
ag_ALL_Mt_R_Residue_Y$Food_Mt<-0
ag_ALL_Mt_R_Residue_Y$Feed_Mt<-ag_ALL_Mt_R_FodderHerb_Y$Residue_Mt
ag_ALL_Mt_R_Residue_Y$OtherUses_Mt<-0
ag_ALL_Mt_R_Residue_Y$ID_R_C_Y<-paste(ag_ALL_Mt_R_Residue_Y$GCAM_region_ID,ag_ALL_Mt_R_Residue_Y$GCAM_commodity,ag_ALL_Mt_R_Residue_Y$year,sep="")

#Rbind fodderherb and residue balances into a single table
ag_ALL_Mt_R_FodderHerbResidue_Y<-rbind(ag_ALL_Mt_R_FodderHerb_Y_adj,ag_ALL_Mt_R_Residue_Y)

write.table(ag_ALL_Mt_R_FodderHerbResidue_Y,file="Rdata_out/ag_ALL_Mt_R_FodderHerbResidue_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)
                            
#Subset only the feed and make a table
ag_Feed_Mt_R_FodderHerbResidue_Y<-ag_ALL_Mt_R_FodderHerbResidue_Y[,1:3]
ag_Feed_Mt_R_FodderHerbResidue_Y$Feed_Mt<-ag_ALL_Mt_R_FodderHerbResidue_Y$Feed_Mt
ag_Feed_Mt_R_FodderHerbResidue_Y$ID_R_C_Y<-paste(ag_Feed_Mt_R_FodderHerbResidue_Y$GCAM_region_ID,ag_Feed_Mt_R_FodderHerbResidue_Y$GCAM_commodity,ag_Feed_Mt_R_FodderHerbResidue_Y$year,sep="")

write.table(ag_Feed_Mt_R_FodderHerbResidue_Y,file="Rdata_out/ag_Feed_Mt_R_FodderHerbResidue_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

#PASTURE & FODDERGRASS
#Calculate regional FodderGrass production
ag_Prod_Mt_R_FodderGrass_Y<-ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity=="FodderGrass",1:3]
ag_Prod_Mt_R_FodderGrass_Y$Prod_Mt<-apply(ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity=="FodderGrass",4:21],1,sum)

#Calculate regional demands of FodderGrass+Pasture
an_Feed_Mt_R_PastureFodderGrass_Y<-an_Feed_Mt_R_F_Y[an_Feed_Mt_R_F_Y$feed=="Pasture_FodderGrass",]

#Pasture demand is equal to FodderGrass+Pasture demand minus FodderGrass production within each region
#NOTE: ADJUST FEED FRACTIONS IN IMAGE DATABASE TO ENSURE THAT NO REGION HAS NEGATIVE PASTURE DEMAND. Japan and Korea adjusted.
ag_Feed_Mt_R_FodderGrass_Y<-ag_Prod_Mt_R_FodderGrass_Y[,1:3]
ag_Feed_Mt_R_FodderGrass_Y$Feed_Mt<-ag_Prod_Mt_R_FodderGrass_Y$Prod_Mt
ag_Feed_Mt_R_Pasture_Y<-ag_Prod_Mt_R_FodderGrass_Y[,1:3]
ag_Feed_Mt_R_Pasture_Y$GCAM_commodity<-"Pasture"
ag_Feed_Mt_R_Pasture_Y$Feed_Mt<-an_Feed_Mt_R_PastureFodderGrass_Y$Feed_Mt-ag_Feed_Mt_R_FodderGrass_Y$Feed_Mt

#Build table and write it out
ag_Feed_Mt_R_PastureFodderGrass_Y<-rbind(ag_Feed_Mt_R_FodderGrass_Y,ag_Feed_Mt_R_Pasture_Y)
ag_Feed_Mt_R_PastureFodderGrass_Y$ID_R_C_Y<-paste(ag_Feed_Mt_R_PastureFodderGrass_Y$GCAM_region_ID,ag_Feed_Mt_R_PastureFodderGrass_Y$GCAM_commodity,ag_Feed_Mt_R_PastureFodderGrass_Y$year,sep="")

#write out table of pasture and foddergrass consumption
write.table(ag_Feed_Mt_R_PastureFodderGrass_Y,file="Rdata_out/ag_Feed_Mt_R_PastureFodderGrass_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Build balance table of foddergrass production, net exports, and all uses (food, feed, other uses).
ag_ALL_Mt_R_FodderGrass_Y<-ag_Feed_Mt_R_FodderGrass_Y[,1:3]
ag_ALL_Mt_R_FodderGrass_Y$Prod_Mt<-ag_Feed_Mt_R_FodderGrass_Y$Feed_Mt
ag_ALL_Mt_R_FodderGrass_Y$NetExp_Mt<-0
ag_ALL_Mt_R_FodderGrass_Y$Supply_Mt<-ag_ALL_Mt_R_FodderGrass_Y$Prod_Mt
ag_ALL_Mt_R_FodderGrass_Y$Food_Mt<-0
ag_ALL_Mt_R_FodderGrass_Y$Feed_Mt<-ag_Feed_Mt_R_FodderGrass_Y$Feed_Mt
ag_ALL_Mt_R_FodderGrass_Y$OtherUses_Mt<-0
ag_ALL_Mt_R_FodderGrass_Y$ID_R_C_Y<-paste(ag_ALL_Mt_R_FodderGrass_Y$GCAM_region_ID,ag_ALL_Mt_R_FodderGrass_Y$GCAM_commodity,ag_ALL_Mt_R_FodderGrass_Y$year,sep="")

#Write this table out
write.table(ag_ALL_Mt_R_FodderGrass_Y,file="Rdata_out/ag_ALL_Mt_R_FodderGrass_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

#SCAVENGING & OTHER
#regional demands of scavenging_other determine the supplies; no calculations are needed here
ag_Feed_Mt_R_ScvgOthr_Y<-an_Feed_Mt_R_F_Y[an_Feed_Mt_R_F_Y$feed=="Scavenging_Other",]
names(ag_Feed_Mt_R_ScvgOthr_Y)[names(ag_Feed_Mt_R_ScvgOthr_Y)=="feed"]<-"GCAM_commodity"
ag_Feed_Mt_R_ScvgOthr_Y$ID_R_C_Y<-paste(ag_Feed_Mt_R_ScvgOthr_Y$GCAM_region_ID,ag_Feed_Mt_R_ScvgOthr_Y$GCAM_commodity,ag_Feed_Mt_R_ScvgOthr_Y$year,sep="")

#write out table of scavenging/other production
write.table(ag_Feed_Mt_R_ScvgOthr_Y,file="Rdata_out/ag_Feed_Mt_R_ScvgOthr_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

#FEEDCROPS
#Compile regional feedcrop demands by crop type from FAO balances. Import data, add identifier vectors, and aggregate.
ag_Feed_t_SUA<-read.table("Inventory Data/FAO/Rdata_in/ag_Feed_t_SUA.csv",header=T,sep=',')
ag_Feed_t_SUA_missing<-read.table("Inventory Data/FAO/Rdata_in/ag_Feed_t_SUA_missing.csv",header=T,sep=',')
ag_Feed_t_SUA_full<-rbind(ag_Feed_t_SUA,ag_Feed_t_SUA_missing)
ag_Feed_t_SUA_full$GCAM_region_ID<-ctry_reg_FAO$GCAM_region_ID[match(ag_Feed_t_SUA_full$countries,ctry_reg_FAO$countries)]
ag_Feed_t_SUA_full$GCAM_commodity<-ag_items_cal_SUA$GCAM_commodity[match(ag_Feed_t_SUA_full$item,ag_items_cal_SUA$item)]
ag_Feed_t_R_Cnff_year<-aggregate(ag_Feed_t_SUA_full[,3:12],by=list(ag_Feed_t_SUA_full$GCAM_region_ID,ag_Feed_t_SUA_full$GCAM_commodity),FUN=sum)
names(ag_Feed_t_R_Cnff_year)[names(ag_Feed_t_R_Cnff_year)=="Group.1"]<-"GCAM_region_ID"
names(ag_Feed_t_R_Cnff_year)[names(ag_Feed_t_R_Cnff_year)=="Group.2"]<-"GCAM_commodity"
#Drop na's and fibercrop
ag_Feed_t_R_Cnff_year<-ag_Feed_t_R_Cnff_year[ag_Feed_t_R_Cnff_year$GCAM_commodity!="na" & ag_Feed_t_R_Cnff_year$GCAM_commodity!="FiberCrop",]

#Take five-year rolling averages
#NOTE: Step assumes two model base periods
ag_Feed_t_R_Cnff_1990<-ag_Feed_t_R_Cnff_year[,1:2]
ag_Feed_t_R_Cnff_1990$year<-1990
ag_Feed_t_R_Cnff_1990$Feed_t<-apply(ag_Feed_t_R_Cnff_year[,3:7],1,mean)
ag_Feed_t_R_Cnff_2005<-ag_Feed_t_R_Cnff_year[,1:2]
ag_Feed_t_R_Cnff_2005$year<-2005
ag_Feed_t_R_Cnff_2005$Feed_t<-apply(ag_Feed_t_R_Cnff_year[,8:12],1,mean)
ag_Feed_t_R_Cnff_Y<-rbind(ag_Feed_t_R_Cnff_1990,ag_Feed_t_R_Cnff_2005)

#Convert to desired units (Mt)
ag_Feed_Mt_R_Cnff_Y<-ag_Feed_t_R_Cnff_Y[,1:3]
ag_Feed_Mt_R_Cnff_Y$Feed_Mt<-ag_Feed_t_R_Cnff_Y$Feed_t/1000000

#Aggregate FAO feed data by region in order to calculate proportions to assign to each crop
ag_Feed_Mt_R_Y<-aggregate(ag_Feed_Mt_R_Cnff_Y[,4],by=list(ag_Feed_Mt_R_Cnff_Y$GCAM_region_ID,ag_Feed_Mt_R_Cnff_Y$year),FUN=sum)
names(ag_Feed_Mt_R_Y)[names(ag_Feed_Mt_R_Y)=="Group.1"]<-"GCAM_region_ID"
names(ag_Feed_Mt_R_Y)[names(ag_Feed_Mt_R_Y)=="Group.2"]<-"year"
names(ag_Feed_Mt_R_Y)[names(ag_Feed_Mt_R_Y)=="x"]<-"Feed_Mt"
ag_Feed_Mt_R_Y$ID_R_Y<-paste(ag_Feed_Mt_R_Y$GCAM_region_ID,ag_Feed_Mt_R_Y$year,sep="")

#Calculate proportions by crop type and region
ag_Feed_Mt_R_Cnff_Y$ID_R_Y<-paste(ag_Feed_Mt_R_Cnff_Y$GCAM_region_ID,ag_Feed_Mt_R_Cnff_Y$year,sep="")
ag_Feed_Mt_R_Cnff_Y$TotalFeed_Mt<-ag_Feed_Mt_R_Y$Feed_Mt[match(ag_Feed_Mt_R_Cnff_Y$ID_R_Y,ag_Feed_Mt_R_Y$ID_R_Y)]
ag_Feed_Mt_R_Cnff_Y$Feedfrac<-ag_Feed_Mt_R_Cnff_Y$Feed_Mt / ag_Feed_Mt_R_Cnff_Y$TotalFeed_Mt

#Compile regional total feedcrop demands from IMAGE data and add ID vector
an_Feed_Mt_R_FeedCrop_Y<-an_Feed_Mt_R_F_Y[an_Feed_Mt_R_F_Y$feed=="FeedCrops",]
an_Feed_Mt_R_FeedCrop_Y$ID_R_Y<-paste(an_Feed_Mt_R_FeedCrop_Y$GCAM_region_ID,an_Feed_Mt_R_FeedCrop_Y$year,sep="")

#Add this vector to the FAO-based table
ag_Feed_Mt_R_Cnff_Y$TotalFeed_Mt_adj<-an_Feed_Mt_R_FeedCrop_Y$Feed_Mt[match(ag_Feed_Mt_R_Cnff_Y$ID_R_Y,an_Feed_Mt_R_FeedCrop_Y$ID_R_Y)]
ag_Feed_Mt_R_Cnff_Y$Feed_Mt_adj<-ag_Feed_Mt_R_Cnff_Y$TotalFeed_Mt_adj * ag_Feed_Mt_R_Cnff_Y$Feedfrac

#Create final feedcrop table and export
ag_Feed_Mt_R_Cnff_Y_adj<-ag_Feed_Mt_R_Cnff_Y[,1:3]
ag_Feed_Mt_R_Cnff_Y_adj$Feed_Mt<-ag_Feed_Mt_R_Cnff_Y$Feed_Mt_adj
ag_Feed_Mt_R_Cnff_Y_adj$ID_R_C_Y<-paste(ag_Feed_Mt_R_Cnff_Y_adj$GCAM_region_ID,ag_Feed_Mt_R_Cnff_Y_adj$GCAM_commodity,ag_Feed_Mt_R_Cnff_Y_adj$year,sep="")

write.table(ag_Feed_Mt_R_Cnff_Y_adj,file="Rdata_out/ag_Feed_Mt_R_Cnff_Y_adj_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Merge all feed sources into a single table, sort, zero out small values, and write it out
ag_Feed_Mt_R_C_Y<-rbind(ag_Feed_Mt_R_Cnff_Y_adj,ag_Feed_Mt_R_FodderHerbResidue_Y,ag_Feed_Mt_R_PastureFodderGrass_Y,ag_Feed_Mt_R_ScvgOthr_Y)
ag_Feed_Mt_R_C_Y<-ag_Feed_Mt_R_C_Y[order(ag_Feed_Mt_R_C_Y$year,ag_Feed_Mt_R_C_Y$GCAM_commodity,ag_Feed_Mt_R_C_Y$GCAM_region_ID),]
ag_Feed_Mt_R_C_Y$Feed_Mt[ag_Feed_Mt_R_C_Y$Feed_Mt<1e-6]<-0

write.table(ag_Feed_Mt_R_C_Y,file="Rdata_out/ag_Feed_Mt_R_C_Y_08.csv",sep=",",col.names=TRUE,row.names=FALSE)

