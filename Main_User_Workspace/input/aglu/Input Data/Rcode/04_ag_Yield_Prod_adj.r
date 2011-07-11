#Generate vectors for assessment of yields
maxyieldUSA<-apply(ag_Yield_kgm2_R_C_Y_AEZ[ag_Yield_kgm2_R_C_Y_AEZ$GCAM_region_ID==1&ag_Yield_kgm2_R_C_Y_AEZ$year==2005,4:21],1,max)
ag_maxyieldUSA_C_2005<-cbind(ag_Yield_kgm2_R_C_Y_AEZ[ag_Yield_kgm2_R_C_Y_AEZ$GCAM_region_ID==1&ag_Yield_kgm2_R_C_Y_AEZ$year==2005,1:3],maxyieldUSA)
#The USA doesn't grow palmfruit, so hard-wiring a maximum yield for palm fruit that is equal to the global max
ag_maxyieldUSA_C_2005$maxyieldUSA[ag_maxyieldUSA_C_2005$GCAM_commodity=="PalmFruit"]<-max(ag_Yield_kgm2_R_C_Y_AEZ[ag_Yield_kgm2_R_C_Y_AEZ$GCAM_commodity=="PalmFruit",4:21])
ag_Yield_kgm2_R_C_Y_AEZ$max<-ag_maxyieldUSA_C_2005$maxyieldUSA[match(ag_Yield_kgm2_R_C_Y_AEZ$GCAM_commodity,ag_maxyieldUSA_C_2005$GCAM_commodity)]

#Calculate the regional average yield for each region and crop
ag_Prod_Mt_R_C_Y_AEZ$total<-apply(ag_Prod_Mt_R_C_Y_AEZ[,4:21],1,sum)
ag_HA_bm2_R_C_Y_AEZ$total<-apply(ag_HA_bm2_R_C_Y_AEZ[,4:21],1,sum)
ag_Yield_kgm2_R_C_Y_AEZ$avg<-ag_Prod_Mt_R_C_Y_AEZ$total/ag_HA_bm2_R_C_Y_AEZ$total

#Set rule: if yield in AEZ is greater than the USA max yield for that crop, and that AEZ accounts for less than 1.5%
#of total regional production of that crop, set the yield in that AEZ equal to the regional average for that crop.
ag_Yield_kgm2_R_C_Y_AEZ_adj<-ag_Yield_kgm2_R_C_Y_AEZ[,1:3]
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ1<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ1>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ1<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ1)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ2<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ2>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ2<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ2)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ3<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ3>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ3<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ3)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ4<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ4>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ4<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ4)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ5<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ5>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ5<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ5)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ6<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ6>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ6<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ6)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ7<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ7>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ7<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ7)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ8<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ8>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ8<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ8)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ9<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ9>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ9<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ9)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ10<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ10>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ10<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ10)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ11<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ11>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ11<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ11)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ12<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ12>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ12<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ12)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ13<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ13>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ13<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ13)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ14<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ14>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ14<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ14)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ15<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ15>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ15<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ15)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ16<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ16>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ16<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ16)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ17<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ17>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ17<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ17)
ag_Yield_kgm2_R_C_Y_AEZ_adj$AEZ18<-ifelse(ag_Yield_kgm2_R_C_Y_AEZ$AEZ18>ag_Yield_kgm2_R_C_Y_AEZ$max & ag_Prod_frac_R_C_AEZ$AEZ18<0.015,ag_Yield_kgm2_R_C_Y_AEZ$avg,ag_Yield_kgm2_R_C_Y_AEZ$AEZ18)

#calculate adjusted production as harvested area times adjusted yields
ag_Prod_Mt_R_C_Y_AEZ_adj<-cbind(ag_Yield_kgm2_R_C_Y_AEZ_adj[,1:3],ag_Yield_kgm2_R_C_Y_AEZ_adj[,4:21]*ag_HA_bm2_R_C_Y_AEZ[,4:21])
ag_Prod_Mt_R_C_Y_AEZ_adj$ID_R_C_Y<-paste(ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_region_ID,ag_Prod_Mt_R_C_Y_AEZ_adj$GCAM_commodity,ag_Prod_Mt_R_C_Y_AEZ_adj$year,sep="")

write.table(ag_Yield_kgm2_R_C_Y_AEZ_adj,file="Rdata_out/ag_Yield_kgm2_R_C_Y_AEZ_adj_04.csv",sep=",",col.names=TRUE,row.names=FALSE)
write.table(ag_Prod_Mt_R_C_Y_AEZ_adj,file="Rdata_out/ag_Prod_Mt_R_C_Y_AEZ_adj_04.csv",sep=",",col.names=TRUE,row.names=FALSE)


