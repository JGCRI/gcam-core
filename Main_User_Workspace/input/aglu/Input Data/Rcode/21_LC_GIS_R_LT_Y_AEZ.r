#import data from CSV files
LC_km2_GIS<-read.table("Inventory Data/GIS/Rdata_in/LC_km2_GIS.csv",header=T,sep=',')
ctry_reg_AEZ_GIS<-read.table("Inventory Data/GIS/Rdata_in/ctry_reg_AEZ_GIS.csv",header=T,sep=',')
Land_Types_GIS<-read.table("Inventory Data/GIS/Rdata_in/Land_Types_GIS.csv",header=T,sep=',')

#Add vectors for GCAM region ID and AEZ
LC_km2_GIS$GCAM_region_ID<-ctry_reg_AEZ_GIS$GCAM_region_ID[match(LC_km2_GIS$AEZ_ID,ctry_reg_AEZ_GIS$AEZ_ID)]
LC_km2_GIS$AEZ<-ctry_reg_AEZ_GIS$AEZ[match(LC_km2_GIS$AEZ_ID,ctry_reg_AEZ_GIS$AEZ_ID)]

#Add vectors for land type (SAGE, HYDE, and WDPA)
LC_km2_GIS$LT_SAGE<-Land_Types_GIS$LT_SAGE[match(LC_km2_GIS$Category,Land_Types_GIS$Category)]
LC_km2_GIS$LT_HYDE<-Land_Types_GIS$LT_HYDE[match(LC_km2_GIS$Category,Land_Types_GIS$Category)]
LC_km2_GIS$LT_WDPA<-Land_Types_GIS$LT_WDPA[match(LC_km2_GIS$Category,Land_Types_GIS$Category)]

#Drop all rows with missing values (bodies of water)
LC_km2_GIS<-na.omit(LC_km2_GIS)

#Reset WDPA classification to "Non-protected" where HYDE classification is cropland, pasture, or urban land
LC_km2_GIS$LT_WDPA[LC_km2_GIS$LT_HYDE=="Cropland" | LC_km2_GIS$LT_HYDE=="Pasture" | LC_km2_GIS$LT_HYDE=="Urbanland"]<-"Non-protected"

#These multi-tiered classifications will be used for C contents, but for all land cover processing, collapse into GCAM land types
LC_km2_GIS$Land_Type<-LC_km2_GIS$LT_SAGE
#Need to add levels to new variable Land_Type
#NOTE: To include protected lands: Add a category called "Protected" to the list below
levels(LC_km2_GIS$Land_Type)<-c(levels(LC_km2_GIS$LT_SAGE),"Cropland","Pasture","Urbanland")
LC_km2_GIS$Land_Type[LC_km2_GIS$LT_HYDE=="Cropland"]<-"Cropland"
LC_km2_GIS$Land_Type[LC_km2_GIS$LT_HYDE=="Pasture"]<-"Pasture"
LC_km2_GIS$Land_Type[LC_km2_GIS$LT_HYDE=="Urbanland"]<-"Urbanland"
#NOTE: To include protected lands, uncomment the line below
#LC_km2_GIS$Land_Type[LC_km2_GIS$LT_WDPA=="Protected"]<-"Protected"

#Add vector for area in thousand square kilometers (bm2)
LC_km2_GIS$Area_bm2<-LC_km2_GIS$Area_km2/1000

#Aggregate into GCAM regions and land types. This table is incomplete (missing combinations not written out), indicated by LCi
LCi_bm2_R_LT_year_AEZ<-aggregate(LC_km2_GIS$Area_bm2,list(GCAM_region_ID=LC_km2_GIS$GCAM_region_ID,Land_Type=LC_km2_GIS$Land_Type,year=LC_km2_GIS$Year,AEZ=LC_km2_GIS$AEZ),sum)
names(LCi_bm2_R_LT_year_AEZ)[names(LCi_bm2_R_LT_year_AEZ)=="x"]<-"Area_bm2"

#Calculate 1975
#First match 1980 values to 1970 values
LCi_bm2_R_LT_1970_AEZ<-LCi_bm2_R_LT_year_AEZ[LCi_bm2_R_LT_year_AEZ$year==1970,]
LCi_bm2_R_LT_1970_AEZ$ID_R_LT_AEZ<-paste(LCi_bm2_R_LT_1970_AEZ$GCAM_region_ID,LCi_bm2_R_LT_1970_AEZ$Land_Type,LCi_bm2_R_LT_1970_AEZ$AEZ,sep="")
LCi_bm2_R_LT_1980_AEZ<-LCi_bm2_R_LT_year_AEZ[LCi_bm2_R_LT_year_AEZ$year==1980,]
LCi_bm2_R_LT_1980_AEZ$ID_R_LT_AEZ<-paste(LCi_bm2_R_LT_1980_AEZ$GCAM_region_ID,LCi_bm2_R_LT_1980_AEZ$Land_Type,LCi_bm2_R_LT_1980_AEZ$AEZ,sep="")
LCi_bm2_R_LT_1970_AEZ$Area_bm2_1980<-LCi_bm2_R_LT_1980_AEZ$Area_bm2[match(LCi_bm2_R_LT_1970_AEZ$ID_R_LT_AEZ,LCi_bm2_R_LT_1980_AEZ$ID_R_LT_AEZ)]

#Replace missing values with 0, and take the average
LCi_bm2_R_LT_1970_AEZ$Area_bm2_1980[is.na(LCi_bm2_R_LT_1970_AEZ$Area_bm2_1980)]<-0
LCi_bm2_R_LT_1970_AEZ$Area_bm2_1975<-(LCi_bm2_R_LT_1970_AEZ$Area_bm2 + LCi_bm2_R_LT_1970_AEZ$Area_bm2_1980) / 2

LCi_bm2_R_LT_1975_AEZ<-LCi_bm2_R_LT_1970_AEZ[,1:5]
LCi_bm2_R_LT_1975_AEZ$year<-1975
LCi_bm2_R_LT_1975_AEZ$Area_bm2<-LCi_bm2_R_LT_1970_AEZ$Area_bm2_1975

#Subset only the years that are relevant for the model read-in (1700, 1750, 1800, 1850, 1900, 1950, 1975, 1990, 2005)
LCi_bm2_R_LT_Yh1950_AEZ<-LCi_bm2_R_LT_year_AEZ[LCi_bm2_R_LT_year_AEZ$year %in% c(1700, 1750, 1800, 1850, 1900, 1950),]
LC_bm2_R_LT_Yh1975_AEZ<-rbind(LCi_bm2_R_LT_Yh1950_AEZ,LCi_bm2_R_LT_1975_AEZ)
LCi_bm2_R_LT_Yh_AEZ<-rbind(LC_bm2_R_LT_Yh1975_AEZ,LCi_bm2_R_LT_year_AEZ[LCi_bm2_R_LT_year_AEZ$year %in% c(1990, 2005),])

#Add ID vector that will allow the table to be transposed so that AEZs are read as columns
LCi_bm2_R_LT_Yh_AEZ$ID_R_LT_Y<-paste(LCi_bm2_R_LT_Yh_AEZ$GCAM_region_ID,LCi_bm2_R_LT_Yh_AEZ$Land_Type,LCi_bm2_R_LT_Yh_AEZ$year,sep="")

#Create a modified table with AEZs as columns and rows are regions x land types x years
#Start with regions x land types, with region vector sorted
#NOTE: THIS STEP ASSUMES 14 GCAM REGIONS
R_LT_GCAM<-data.frame(GCAM_region_ID=rep(c(1:14),times=nlevels(LCi_bm2_R_LT_Yh_AEZ$Land_Type)),Land_Type=rep(levels(LCi_bm2_R_LT_Yh_AEZ$Land_Type),times=14))
R_LT_GCAM$GCAM_region_ID<-sort(R_LT_GCAM$GCAM_region_ID)

LC_bm2_R_LT_Yh_AEZ<-R_LT_GCAM[rep(1:nrow(R_LT_GCAM),times=nlevels(as.factor(LCi_bm2_R_LT_Yh_AEZ$year))),]
LC_bm2_R_LT_Yh_AEZ$year<-levels(as.factor(LCi_bm2_R_LT_Yh_AEZ$year))
LC_bm2_R_LT_Yh_AEZ$year<-sort(LC_bm2_R_LT_Yh_AEZ$year)
LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y<-paste(LC_bm2_R_LT_Yh_AEZ$GCAM_region_ID,LC_bm2_R_LT_Yh_AEZ$Land_Type,LC_bm2_R_LT_Yh_AEZ$year,sep="")

#Divide main land cover table into separate tables, one for each AEZ
LCi_bm2_R_LT_Yh_AEZ1<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==1,]
LCi_bm2_R_LT_Yh_AEZ2<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==2,]
LCi_bm2_R_LT_Yh_AEZ3<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==3,]
LCi_bm2_R_LT_Yh_AEZ4<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==4,]
LCi_bm2_R_LT_Yh_AEZ5<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==5,]
LCi_bm2_R_LT_Yh_AEZ6<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==6,]
LCi_bm2_R_LT_Yh_AEZ7<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==7,]
LCi_bm2_R_LT_Yh_AEZ8<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==8,]
LCi_bm2_R_LT_Yh_AEZ9<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==9,]
LCi_bm2_R_LT_Yh_AEZ10<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==10,]
LCi_bm2_R_LT_Yh_AEZ11<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==11,]
LCi_bm2_R_LT_Yh_AEZ12<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==12,]
LCi_bm2_R_LT_Yh_AEZ13<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==13,]
LCi_bm2_R_LT_Yh_AEZ14<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==14,]
LCi_bm2_R_LT_Yh_AEZ15<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==15,]
LCi_bm2_R_LT_Yh_AEZ16<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==16,]
LCi_bm2_R_LT_Yh_AEZ17<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==17,]
LCi_bm2_R_LT_Yh_AEZ18<-LCi_bm2_R_LT_Yh_AEZ[LCi_bm2_R_LT_Yh_AEZ$AEZ==18,]

#Paste in land cover estimates
LC_bm2_R_LT_Yh_AEZ$AEZ1<-LCi_bm2_R_LT_Yh_AEZ1$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ1$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ2<-LCi_bm2_R_LT_Yh_AEZ2$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ2$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ3<-LCi_bm2_R_LT_Yh_AEZ3$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ3$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ4<-LCi_bm2_R_LT_Yh_AEZ4$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ4$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ5<-LCi_bm2_R_LT_Yh_AEZ5$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ5$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ6<-LCi_bm2_R_LT_Yh_AEZ6$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ6$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ7<-LCi_bm2_R_LT_Yh_AEZ7$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ7$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ8<-LCi_bm2_R_LT_Yh_AEZ8$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ8$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ9<-LCi_bm2_R_LT_Yh_AEZ9$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ9$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ10<-LCi_bm2_R_LT_Yh_AEZ10$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ10$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ11<-LCi_bm2_R_LT_Yh_AEZ11$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ11$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ12<-LCi_bm2_R_LT_Yh_AEZ12$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ12$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ13<-LCi_bm2_R_LT_Yh_AEZ13$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ13$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ14<-LCi_bm2_R_LT_Yh_AEZ14$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ14$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ15<-LCi_bm2_R_LT_Yh_AEZ15$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ15$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ16<-LCi_bm2_R_LT_Yh_AEZ16$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ16$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ17<-LCi_bm2_R_LT_Yh_AEZ17$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ17$ID_R_LT_Y)]
LC_bm2_R_LT_Yh_AEZ$AEZ18<-LCi_bm2_R_LT_Yh_AEZ18$Area_bm2[match(LC_bm2_R_LT_Yh_AEZ$ID_R_LT_Y,LCi_bm2_R_LT_Yh_AEZ18$ID_R_LT_Y)]

#Sort, move ID vector to last column, and replace NA's with 0
LC_bm2_R_LT_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[order(LC_bm2_R_LT_Yh_AEZ$year,LC_bm2_R_LT_Yh_AEZ$Land_Type,LC_bm2_R_LT_Yh_AEZ$GCAM_region_ID),]
LC_bm2_R_LT_Yh_AEZ<-LC_bm2_R_LT_Yh_AEZ[,c(1:3,5:22,4)]
LC_bm2_R_LT_Yh_AEZ[is.na(LC_bm2_R_LT_Yh_AEZ)]<-0

#Write it out
write.table(LC_bm2_R_LT_Yh_AEZ,file="Rdata_out/LC_bm2_R_LT_Yh_AEZ_21.csv",sep=",",col.names=TRUE,row.names=FALSE)
