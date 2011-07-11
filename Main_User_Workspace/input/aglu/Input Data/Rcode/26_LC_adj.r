#Total land cover by region
#NOTE: If protected lands are included, paste this in before the terminal parenthesis below:  + LC_bm2_R_Protected_Yh_AEZ[,4:21]
LC_bm2_R_Yh_AEZ<-cbind(LC_bm2_R_HarvCropLand_Yh_AEZ[,1:3],LC_bm2_R_HarvCropLand_Yh_AEZ[,4:21]+LC_bm2_R_OtherArableLand_Yh_AEZ[,4:21]+LC_bm2_R_MgdPasture_Yh_AEZ[,4:21]
  +LC_bm2_R_MgdForest_Yh_AEZ[,4:21]+LC_bm2_R_Shrub_Yh_AEZ_adj[,4:21] + LC_bm2_R_Grass_Yh_AEZ_adj[,4:21] + LC_bm2_R_UnMgdPasture_Yh_AEZ_adj[,4:21] + LC_bm2_R_UnMgdForest_Yh_AEZ_adj[,4:21]
  +LC_bm2_R_UrbanLand_Yh_AEZ[,4:21] + LC_bm2_R_Tundra_Yh_AEZ[,4:21] + LC_bm2_R_RckIceDsrt_Yh_AEZ[,4:21])
LC_bm2_R_Yh_AEZ$Land_Type<-"Total"

#Land cover should not change between periods. Check to make sure that this is the case
LC_bm2_R_1700_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1700,]
LC_bm2_R_1750_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1750,]
LC_bm2_R_1800_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1800,]
LC_bm2_R_1850_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1850,]
LC_bm2_R_1900_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1900,]
LC_bm2_R_1950_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1950,]
LC_bm2_R_1975_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1975,]
LC_bm2_R_1990_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==1990,]
LC_bm2_R_2005_AEZ<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==2005,]

LC_check_1700<-cbind(LC_bm2_R_1700_AEZ[,1:3],LC_bm2_R_1700_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1750<-cbind(LC_bm2_R_1750_AEZ[,1:3],LC_bm2_R_1750_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1800<-cbind(LC_bm2_R_1800_AEZ[,1:3],LC_bm2_R_1800_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1850<-cbind(LC_bm2_R_1850_AEZ[,1:3],LC_bm2_R_1850_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1900<-cbind(LC_bm2_R_1900_AEZ[,1:3],LC_bm2_R_1900_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1950<-cbind(LC_bm2_R_1950_AEZ[,1:3],LC_bm2_R_1950_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1975<-cbind(LC_bm2_R_1975_AEZ[,1:3],LC_bm2_R_1975_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])
LC_check_1990<-cbind(LC_bm2_R_1990_AEZ[,1:3],LC_bm2_R_1990_AEZ[,4:21] / LC_bm2_R_2005_AEZ[,4:21])

LC_check<-rbind(LC_check_1700,LC_check_1750,LC_check_1800,LC_check_1850,LC_check_1900,LC_check_1950,LC_check_1975,LC_check_1990)
LC_check[is.na(LC_check)]<-1
print(ifelse(any(LC_check[,4:21]<0.99|LC_check[,4:21]>1.01),"Land cover changes over time in some regions/AEZs","Land cover is constant over time for all regions and AEZs"))

#Write out the totals
LC_bm2_R<-LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==2005,c(1,3)] 
LC_bm2_R$Land_Type<-"Total"
LC_bm2_R$Total<-apply(LC_bm2_R_Yh_AEZ[LC_bm2_R_Yh_AEZ$year==2005,4:21],1,sum)

write.table(LC_bm2_R,file="Rdata_out/LC_bm2_R_26.csv",sep=",",col.names=TRUE,row.names=FALSE)







