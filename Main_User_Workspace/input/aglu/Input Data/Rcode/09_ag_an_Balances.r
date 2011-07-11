#Build balances of agricultural crops. Exclude fodder crops at this stage as their balances are already built elsewhere.
ag_ALL_Mt_R_Cnf_Y<-ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity!="FodderGrass" & ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity!="FodderHerb",1:3]
ag_ALL_Mt_R_Cnf_Y$Prod_Mt<-apply(ag_Prod_Mt_R_C_Y_AEZ_adj[ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity!="FodderGrass" & ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity!="FodderHerb",4:21],1,sum)
ag_ALL_Mt_R_Cnf_Y$NetExp_Mt<-0
ag_ALL_Mt_R_Cnf_Y$Supply_Mt<-0
ag_ALL_Mt_R_Cnf_Y$Food_Mt<-0
ag_ALL_Mt_R_Cnf_Y$Feed_Mt<-0
ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt<-0
ag_ALL_Mt_R_Cnf_Y$ID_R_C_Y<-paste(ag_ALL_Mt_R_Cnf_Y$GCAM_region_ID,ag_ALL_Mt_R_Cnf_Y$GCAM_commodity,ag_ALL_Mt_R_Cnf_Y$year,sep="")

#Look up exports in the trade tables from FAO/SUA and calculate domestic supply (production minus net exports)
ag_ALL_Mt_R_Cnf_Y$NetExp_Mt<-ag_NetExp_Mt_R_C_Y_adj$NetExp_Mt[match(ag_ALL_Mt_R_Cnf_Y$ID_R_C_Y,ag_NetExp_Mt_R_C_Y_adj$ID_R_C_Y)]
ag_ALL_Mt_R_Cnf_Y$Supply_Mt<-ag_ALL_Mt_R_Cnf_Y$Prod_Mt - ag_ALL_Mt_R_Cnf_Y$NetExp_Mt

#Look up food and feed. Set food consumption of fibercrop to 0, and set all feed NA's to 0 (original table doesn't have 0's written out).
ag_ALL_Mt_R_Cnf_Y$Food_Mt<-ag_Food_Mt_R_C_Y$Food_Mt[match(ag_ALL_Mt_R_Cnf_Y$ID_R_C_Y,ag_Food_Mt_R_C_Y$ID_R_C_Y)]
ag_ALL_Mt_R_Cnf_Y$Food_Mt[ag_ALL_Mt_R_Cnf_Y$GCAM_commodity=="FiberCrop"]<-0
ag_ALL_Mt_R_Cnf_Y$Feed_Mt<-ag_Feed_Mt_R_C_Y$Feed_Mt[match(ag_ALL_Mt_R_Cnf_Y$ID_R_C_Y,ag_Feed_Mt_R_C_Y$ID_R_C_Y)]
ag_ALL_Mt_R_Cnf_Y$Feed_Mt[is.na(ag_ALL_Mt_R_Cnf_Y$Feed_Mt)]<-0

#Other uses is equal to domestic supply minus food and feed. Any negative numbers for other uses indicate other stuff needs to be adjusted.
ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt<-ag_ALL_Mt_R_Cnf_Y$Supply_Mt - ag_ALL_Mt_R_Cnf_Y$Food_Mt - ag_ALL_Mt_R_Cnf_Y$Feed_Mt

write.table(ag_ALL_Mt_R_Cnf_Y,file="Rdata_out/ag_ALL_Mt_R_Cnf_Y_09.csv",sep=",",col.names=TRUE,row.names=FALSE)

#At present 1990 wheat in the USA isn't balanced due to negative stock changes being greater than sum of all unmodeled forms of consumption (non-food, non-feed).
# Stock changes are normally considered as an "other net use" but where they are negative and exceed other net uses, subtract from net exports.
#Subset negative and positive "other uses" separately. Positive will be the new adjusted "Other uses"
ag_ALL_Mt_R_Cnf_Y$NegOtherUses_Mt<-ifelse(ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt<0,ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt,0)
ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt_adj<-ifelse(ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt>0,ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt,0)
ag_ALL_Mt_R_Cnf_Y$NetExp_Mt_adj<-ag_ALL_Mt_R_Cnf_Y$NetExp_Mt + ag_ALL_Mt_R_Cnf_Y$NegOtherUses_Mt
ag_ALL_Mt_R_Cnf_Y$ID_C_Y<-paste(ag_ALL_Mt_R_Cnf_Y$GCAM_commodity,ag_ALL_Mt_R_Cnf_Y$year,sep="")

#Aggregate totals
ag_ALL_Mt_Cnf_Y<-aggregate(ag_ALL_Mt_R_Cnf_Y[,c(11:13)],by=list(ag_ALL_Mt_R_Cnf_Y$GCAM_commodity,ag_ALL_Mt_R_Cnf_Y$year),FUN=sum)
names(ag_ALL_Mt_Cnf_Y)[names(ag_ALL_Mt_Cnf_Y)=="Group.1"]<-"GCAM_commodity"
names(ag_ALL_Mt_Cnf_Y)[names(ag_ALL_Mt_Cnf_Y)=="Group.2"]<-"year"
ag_ALL_Mt_Cnf_Y$ID_C_Y<-paste(ag_ALL_Mt_Cnf_Y$GCAM_commodity,ag_ALL_Mt_Cnf_Y$year,sep="")

#Paste in totals and calculate regional shares of positive "other net uses" for each crop
ag_ALL_Mt_R_Cnf_Y$GlobalOtherUses_Mt<-ag_ALL_Mt_Cnf_Y$OtherUses_Mt_adj[match(ag_ALL_Mt_R_Cnf_Y$ID_C_Y,ag_ALL_Mt_Cnf_Y$ID_C_Y)]
ag_ALL_Mt_R_Cnf_Y$NetExpAdjFrac<-ag_ALL_Mt_R_Cnf_Y$OtherUses_Mt_adj/ag_ALL_Mt_R_Cnf_Y$GlobalOtherUses_Mt
ag_ALL_Mt_R_Cnf_Y$GlobalNetExpAdj<-ag_ALL_Mt_Cnf_Y$NegOtherUses_Mt[match(ag_ALL_Mt_R_Cnf_Y$ID_C_Y,ag_ALL_Mt_Cnf_Y$ID_C_Y)]
ag_ALL_Mt_R_Cnf_Y$NetExp_Mt_adj2<-ag_ALL_Mt_R_Cnf_Y$NetExp_Mt_adj - ag_ALL_Mt_R_Cnf_Y$NetExpAdjFrac * ag_ALL_Mt_R_Cnf_Y$GlobalNetExpAdj

#Rebuild balance table and write it out
ag_ALL_Mt_R_Cnf_Y_adj<-ag_ALL_Mt_R_Cnf_Y[,1:10]
ag_ALL_Mt_R_Cnf_Y_adj$NetExp_Mt<-ag_ALL_Mt_R_Cnf_Y$NetExp_Mt_adj2
ag_ALL_Mt_R_Cnf_Y_adj$Supply_Mt<-ag_ALL_Mt_R_Cnf_Y_adj$Prod_Mt - ag_ALL_Mt_R_Cnf_Y_adj$NetExp_Mt
ag_ALL_Mt_R_Cnf_Y_adj$OtherUses_Mt<-ag_ALL_Mt_R_Cnf_Y_adj$Supply_Mt - ag_ALL_Mt_R_Cnf_Y_adj$Food_Mt - ag_ALL_Mt_R_Cnf_Y_adj$Feed_Mt

write.table(ag_ALL_Mt_R_Cnf_Y_adj,file="Rdata_out/ag_ALL_Mt_R_Cnf_Y_adj_09.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Combine with foddercrop balances to create a complete balance table, sort, and get rid of values < 1e-6
ag_ALL_Mt_R_C_Y<-rbind(ag_ALL_Mt_R_Cnf_Y_adj,ag_ALL_Mt_R_FodderHerb_Y_adj,ag_ALL_Mt_R_FodderGrass_Y)
ag_ALL_Mt_R_C_Y<-ag_ALL_Mt_R_C_Y[order(ag_ALL_Mt_R_C_Y$year,ag_ALL_Mt_R_C_Y$GCAM_commodity,ag_ALL_Mt_R_C_Y$GCAM_region_ID),]
ag_ALL_Mt_R_C_Y<-cbind(ag_ALL_Mt_R_C_Y[,1:3],apply(ag_ALL_Mt_R_C_Y[,4:9],2,function(x){x[x<1e-6 & x > -1e-6]=0;x}))
ag_ALL_Mt_R_C_Y$ID_R_C_Y<-paste(ag_ALL_Mt_R_C_Y$GCAM_region_ID,ag_ALL_Mt_R_C_Y$GCAM_commodity,ag_ALL_Mt_R_C_Y$year,sep="")

write.table(ag_ALL_Mt_R_C_Y,file="Rdata_out/ag_ALL_Mt_R_C_Y_09.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Build table of animal commodities
an_ALL_Mt_R_C_Y<-an_Prod_Mt_R_C_Y[,1:4]
an_ALL_Mt_R_C_Y$NetExp_Mt<-an_NetExp_Mt_R_C_Y_adj$NetExp_Mt
an_ALL_Mt_R_C_Y$Supply_Mt<-an_ALL_Mt_R_C_Y$Prod_Mt - an_ALL_Mt_R_C_Y$NetExp_Mt
an_ALL_Mt_R_C_Y$Food_Mt<-an_Food_Mt_R_C_Y$Food_Mt
an_ALL_Mt_R_C_Y$OtherUses_Mt<-an_ALL_Mt_R_C_Y$Supply_Mt - an_ALL_Mt_R_C_Y$Food_Mt
an_ALL_Mt_R_C_Y$ID_R_C_Y<-paste(an_ALL_Mt_R_C_Y$GCAM_region_ID,an_ALL_Mt_R_C_Y$GCAM_commodity,an_ALL_Mt_R_C_Y$year,sep="")

write.table(an_ALL_Mt_R_C_Y,file="Rdata_out/an_ALL_Mt_R_C_Y_09.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Several commodities have slightly negative other uses, either due to rounding errors, statistical abnormalities, or scaling adjustments to make net exports sum to 0
#Subset negative and positive "other uses" separately. Positive will be the new adjusted "Other uses"
an_ALL_Mt_R_C_Y$NegOtherUses_Mt<-ifelse(an_ALL_Mt_R_C_Y$OtherUses_Mt<0,an_ALL_Mt_R_C_Y$OtherUses_Mt,0)
an_ALL_Mt_R_C_Y$OtherUses_Mt_adj<-ifelse(an_ALL_Mt_R_C_Y$OtherUses_Mt>0,an_ALL_Mt_R_C_Y$OtherUses_Mt,0)
an_ALL_Mt_R_C_Y$NetExp_Mt_adj<-an_ALL_Mt_R_C_Y$NetExp_Mt + an_ALL_Mt_R_C_Y$NegOtherUses_Mt
an_ALL_Mt_R_C_Y$ID_C_Y<-paste(an_ALL_Mt_R_C_Y$GCAM_commodity,an_ALL_Mt_R_C_Y$year,sep="")

#Aggregate totals
an_ALL_Mt_C_Y<-aggregate(an_ALL_Mt_R_C_Y[,c(10:12)],by=list(an_ALL_Mt_R_C_Y$GCAM_commodity,an_ALL_Mt_R_C_Y$year),FUN=sum)
names(an_ALL_Mt_C_Y)[names(an_ALL_Mt_C_Y)=="Group.1"]<-"GCAM_commodity"
names(an_ALL_Mt_C_Y)[names(an_ALL_Mt_C_Y)=="Group.2"]<-"year"
an_ALL_Mt_C_Y$ID_C_Y<-paste(an_ALL_Mt_C_Y$GCAM_commodity,an_ALL_Mt_C_Y$year,sep="")

#Paste in totals and calculate regional shares of positive "other net uses" for each animal commodity
an_ALL_Mt_R_C_Y$GlobalOtherUses_Mt<-an_ALL_Mt_C_Y$OtherUses_Mt_adj[match(an_ALL_Mt_R_C_Y$ID_C_Y,an_ALL_Mt_C_Y$ID_C_Y)]
an_ALL_Mt_R_C_Y$NetExpAdjFrac<-an_ALL_Mt_R_C_Y$OtherUses_Mt_adj/an_ALL_Mt_R_C_Y$GlobalOtherUses_Mt
an_ALL_Mt_R_C_Y$GlobalNetExpAdj<-an_ALL_Mt_C_Y$NegOtherUses_Mt[match(an_ALL_Mt_R_C_Y$ID_C_Y,an_ALL_Mt_C_Y$ID_C_Y)]
an_ALL_Mt_R_C_Y$NetExp_Mt_adj2<-an_ALL_Mt_R_C_Y$NetExp_Mt_adj - an_ALL_Mt_R_C_Y$NetExpAdjFrac * an_ALL_Mt_R_C_Y$GlobalNetExpAdj

#Rebuild balance table
an_ALL_Mt_R_C_Y_adj<-an_ALL_Mt_R_C_Y[,1:9]
an_ALL_Mt_R_C_Y_adj$NetExp_Mt<-an_ALL_Mt_R_C_Y$NetExp_Mt_adj2
an_ALL_Mt_R_C_Y_adj$Supply_Mt<-an_ALL_Mt_R_C_Y_adj$Prod_Mt - an_ALL_Mt_R_C_Y_adj$NetExp_Mt
an_ALL_Mt_R_C_Y_adj$OtherUses_Mt<-an_ALL_Mt_R_C_Y_adj$Supply_Mt - an_ALL_Mt_R_C_Y_adj$Food_Mt

#Zero out small values and write it out
an_ALL_Mt_R_C_Y_adj<-cbind(an_ALL_Mt_R_C_Y_adj[,1:3],apply(an_ALL_Mt_R_C_Y_adj[,4:8],2,function(x){x[x<1e-6 & x > -1e-6]=0;x}))
an_ALL_Mt_R_C_Y_adj$ID_R_C_Y<-paste(an_ALL_Mt_R_C_Y_adj$GCAM_region_ID,an_ALL_Mt_R_C_Y_adj$GCAM_commodity,an_ALL_Mt_R_C_Y_adj$year,sep="")

write.table(an_ALL_Mt_R_C_Y_adj,file="Rdata_out/an_ALL_Mt_R_C_Y_adj_09.csv",sep=",",col.names=TRUE,row.names=FALSE)


