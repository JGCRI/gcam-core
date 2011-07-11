#NOTE: THIS METHOD WILL NEED TO BE CHANGED IF BASE YEARS OTHER THAN 1990 AND 2005 ARE TO BE READ IN
#import data from CSV files
reg_IMAGE<-read.table("Inventory Data/IMAGE/Rdata_in/reg_IMAGE.csv",header=T,sep=',')
an_Prod_Rimg_C_Y<-read.table("Inventory Data/IMAGE/Rdata_in/an_Prod_Rimg_C_Y.csv",header=T,sep=',')
an_Prodmixfrac_Rimg_C_Y<-read.table("Inventory Data/IMAGE/Rdata_in/an_Prodmixfrac_Rimg_C_Y.csv",header=T,sep=',')
an_Feedfrac_Rimg_C_S_F_Y<-read.table("Inventory Data/IMAGE/Rdata_in/an_Feedfrac_Rimg_C_S_F_Y.csv",header=T,sep=',')

#calculate mixed production as total prod times mixed fraction. Use this to build a table disaggregated by system.
#Note that the IMAGE production data is only used as a weighting factor. The actual amounts are not used, so the units are not labeled.
an_Prod_Rimg_C_Y<-an_Prod_Rimg_C_Y[order(an_Prod_Rimg_C_Y$year,an_Prod_Rimg_C_Y$commodity,an_Prod_Rimg_C_Y$IMAGE_region_ID),]
an_Prodmixfrac_Rimg_C_Y<-an_Prodmixfrac_Rimg_C_Y[order(an_Prodmixfrac_Rimg_C_Y$year,an_Prodmixfrac_Rimg_C_Y$commodity,an_Prodmixfrac_Rimg_C_Y$IMAGE_region_ID),]
an_Prod_Rimg_C_mix_Y<-an_Prod_Rimg_C_Y[,1:2]
an_Prod_Rimg_C_mix_Y$system<-"Mixed"
an_Prod_Rimg_C_mix_Y$year<-an_Prod_Rimg_C_Y[,3]
an_Prod_Rimg_C_mix_Y$Prod<-an_Prod_Rimg_C_Y$Prod*an_Prodmixfrac_Rimg_C_Y$Prodmixfrac
an_Prod_Rimg_C_past_Y<-an_Prod_Rimg_C_Y[,1:2]
an_Prod_Rimg_C_past_Y$system<-"Pastoral"
an_Prod_Rimg_C_past_Y$year<-an_Prod_Rimg_C_Y[,3]
an_Prod_Rimg_C_past_Y$Prod<-an_Prod_Rimg_C_Y$Prod-an_Prod_Rimg_C_mix_Y$Prod
an_Prod_Rimg_C_S_Y<-rbind(an_Prod_Rimg_C_mix_Y,an_Prod_Rimg_C_past_Y)
an_Prod_Rimg_C_S_Y<-an_Prod_Rimg_C_S_Y[order(an_Prod_Rimg_C_S_Y$year,an_Prod_Rimg_C_S_Y$system,an_Prod_Rimg_C_S_Y$commodity,an_Prod_Rimg_C_S_Y$IMAGE_region_ID),]

#Disaggregate this by feed type
#First, order feedfrac table by year, feed, system, commodity, and region
an_Feedfrac_Rimg_C_S_F_Y<-an_Feedfrac_Rimg_C_S_F_Y[order(an_Feedfrac_Rimg_C_S_F_Y$year,an_Feedfrac_Rimg_C_S_F_Y$feed,an_Feedfrac_Rimg_C_S_F_Y$system,an_Feedfrac_Rimg_C_S_F_Y$commodity,an_Feedfrac_Rimg_C_S_F_Y$IMAGE_region_ID),]

#Add identifier vectors (region, commodity, system, and year) to tables. Calculate production by feed type, system, commodity, and IMAGE region.
an_Prod_Rimg_C_S_Y$ID_Rimg_C_S_Y<-paste(an_Prod_Rimg_C_S_Y$IMAGE_region_ID,an_Prod_Rimg_C_S_Y$commodity,an_Prod_Rimg_C_S_Y$system,an_Prod_Rimg_C_S_Y$year,sep="")
an_Feedfrac_Rimg_C_S_F_Y$ID_Rimg_C_S_Y<-paste(an_Feedfrac_Rimg_C_S_F_Y$IMAGE_region_ID,an_Feedfrac_Rimg_C_S_F_Y$commodity,an_Feedfrac_Rimg_C_S_F_Y$system,an_Feedfrac_Rimg_C_S_F_Y$year,sep="")
an_Feedfrac_Rimg_C_S_F_Y$Prod_S<-an_Prod_Rimg_C_S_Y$Prod[match(an_Feedfrac_Rimg_C_S_F_Y$ID_Rimg_C_S_Y,an_Prod_Rimg_C_S_Y$ID_Rimg_C_S_Y)]
an_Prod_Rimg_C_S_F_Y<-an_Feedfrac_Rimg_C_S_F_Y[,1:5]
an_Prod_Rimg_C_S_F_Y$Prod<-an_Feedfrac_Rimg_C_S_F_Y$Feedfrac*an_Feedfrac_Rimg_C_S_F_Y$Prod_S

#Add vector for GCAM region, and aggregate by GCAM region, commodity, system, feed, and year
an_Prod_Rimg_C_S_F_Y$GCAM_region_ID<-reg_IMAGE$GCAM_region_ID[match(an_Prod_Rimg_C_S_F_Y$IMAGE_region_ID,reg_IMAGE$IMAGE_region_ID)]
an_Prod_R_C_S_F_Y<-aggregate(an_Prod_Rimg_C_S_F_Y$Prod,by=list(an_Prod_Rimg_C_S_F_Y$GCAM_region_ID,an_Prod_Rimg_C_S_F_Y$commodity,an_Prod_Rimg_C_S_F_Y$system,an_Prod_Rimg_C_S_F_Y$feed,an_Prod_Rimg_C_S_F_Y$year),FUN=sum)
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="Group.1"]<-"GCAM_region_ID"
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="Group.2"]<-"commodity"
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="Group.3"]<-"system"
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="Group.4"]<-"feed"
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="Group.5"]<-"year"
names(an_Prod_R_C_S_F_Y)[names(an_Prod_R_C_S_F_Y)=="x"]<-"Prod"

#Aggregate by GCAM region, commodity, and year, and assign identifier vectors to table by region, commodity, system, and feed
an_Prod_R_C_Y<-aggregate(an_Prod_R_C_S_F_Y$Prod,by=list(an_Prod_R_C_S_F_Y$GCAM_region_ID,an_Prod_R_C_S_F_Y$commodity,an_Prod_R_C_S_F_Y$year),FUN=sum)
names(an_Prod_R_C_Y)[names(an_Prod_R_C_Y)=="Group.1"]<-"GCAM_region_ID"
names(an_Prod_R_C_Y)[names(an_Prod_R_C_Y)=="Group.2"]<-"commodity"
names(an_Prod_R_C_Y)[names(an_Prod_R_C_Y)=="Group.3"]<-"year"
names(an_Prod_R_C_Y)[names(an_Prod_R_C_Y)=="x"]<-"Prod"
an_Prod_R_C_S_F_Y$ID_R_C_Y<-paste(an_Prod_R_C_S_F_Y$GCAM_region_ID,an_Prod_R_C_S_F_Y$commodity,an_Prod_R_C_S_F_Y$year,sep="")
an_Prod_R_C_Y$ID_R_C_Y<-paste(an_Prod_R_C_Y$GCAM_region_ID,an_Prod_R_C_Y$commodity,an_Prod_R_C_Y$year,sep="")
an_Prod_R_C_S_F_Y$Prod_C<-an_Prod_R_C_Y$Prod[match(an_Prod_R_C_S_F_Y$ID_R_C_Y,an_Prod_R_C_Y$ID_R_C_Y)]
an_Prodfrac_R_C_S_F_Y<-an_Prod_R_C_S_F_Y[,1:5]
an_Prodfrac_R_C_S_F_Y$Prodfrac<-an_Prod_R_C_S_F_Y$Prod/an_Prod_R_C_S_F_Y$Prod_C

#These fractions are used to disaggregate FAO production data into system AND feed
#First, remove other meat and fish from FAO animal production table. Add identifier vectors.
an_Prod_Mt_R_Cpl_Y<-an_Prod_Mt_R_C_Y[an_Prod_Mt_R_C_Y$GCAM_commodity!="OtherMeat_Fish",]
an_Prod_Mt_R_Cpl_Y$ID_R_C_Y<-paste(an_Prod_Mt_R_Cpl_Y$GCAM_region_ID,an_Prod_Mt_R_Cpl_Y$GCAM_commodity,an_Prod_Mt_R_Cpl_Y$year,sep="")

#Add identifier vector to prodfrac table and map output from FAO data to this table.
an_Prodfrac_R_C_S_F_Y$ID_R_C_Y<-paste(an_Prodfrac_R_C_S_F_Y$GCAM_region_ID,an_Prodfrac_R_C_S_F_Y$commodity,an_Prodfrac_R_C_S_F_Y$year,sep="")
an_Prodfrac_R_C_S_F_Y$Prod_Mt_C<-an_Prod_Mt_R_Cpl_Y$Prod_Mt[match(an_Prodfrac_R_C_S_F_Y$ID_R_C_Y,an_Prod_Mt_R_Cpl_Y$ID_R_C_Y)]

#Multiply through to get production in Mt (from FAO) by GCAM region, commodity, system, and feed
an_Prod_Mt_R_C_S_F_Y<-an_Prodfrac_R_C_S_F_Y[,1:5]
an_Prod_Mt_R_C_S_F_Y$Prod_Mt<-an_Prodfrac_R_C_S_F_Y$Prodfrac*an_Prodfrac_R_C_S_F_Y$Prod_Mt_C
#Replace NaN values with 0
an_Prod_Mt_R_C_S_F_Y$Prod_Mt[an_Prod_Mt_R_C_S_F_Y$Prod_Mt=="NaN"]<-0

#add lookup vector
an_Prod_Mt_R_C_S_F_Y$ID_R_C_S_F_Y<-paste(an_Prod_Mt_R_C_S_F_Y$GCAM_region_ID,an_Prod_Mt_R_C_S_F_Y$commodity,an_Prod_Mt_R_C_S_F_Y$system,an_Prod_Mt_R_C_S_F_Y$feed,an_Prod_Mt_R_C_S_F_Y$year,sep="")

#Write out table of production by region, commodity, system, and feed
write.table(an_Prod_Mt_R_C_S_F_Y,file="Rdata_out/an_Prod_Mt_R_C_S_F_Y_07.csv",sep=",",col.names=TRUE,row.names=FALSE)

#Calculation of feed amounts to production systems
an_FeedIO_Rimg_C_S_Y<-read.table("Inventory Data/IMAGE/Rdata_in/an_FeedIO_Rimg_C_S_Y.csv",header=T,sep=',')

#Add production by system to IO table, in order to calculate total feed inputs, by IMAGE region, commodity, system, and year
an_FeedIO_Rimg_C_S_Y$ID_Rimg_C_S_Y<-paste(an_FeedIO_Rimg_C_S_Y$IMAGE_region_ID,an_FeedIO_Rimg_C_S_Y$commodity,an_FeedIO_Rimg_C_S_Y$system,an_FeedIO_Rimg_C_S_Y$year,sep="")
an_FeedIO_Rimg_C_S_Y$Prod_C_S_Y<-an_Prod_Rimg_C_S_Y$Prod[match(an_FeedIO_Rimg_C_S_Y$ID_Rimg_C_S_Y,an_Prod_Rimg_C_S_Y$ID_Rimg_C_S_Y)]
an_FeedIO_Rimg_C_S_Y$Feed_C_S_Y<-an_FeedIO_Rimg_C_S_Y$FeedIO*an_FeedIO_Rimg_C_S_Y$Prod_C_S_Y
an_FeedIO_Rimg_C_S_Y$GCAM_region_ID<-reg_IMAGE$GCAM_region_ID[match(an_FeedIO_Rimg_C_S_Y$IMAGE_region_ID,reg_IMAGE$IMAGE_region_ID)]

#Aggregate by GCAM region, and derive IO coefficients by GCAM region, commodity, and system
an_FeedIO_R_C_S_Y<-aggregate(an_FeedIO_Rimg_C_S_Y[,7:8],by=list(an_FeedIO_Rimg_C_S_Y$GCAM_region_ID,an_FeedIO_Rimg_C_S_Y$commodity,an_FeedIO_Rimg_C_S_Y$system,an_FeedIO_Rimg_C_S_Y$year),FUN=sum)
names(an_FeedIO_R_C_S_Y)[names(an_FeedIO_R_C_S_Y)=="Group.1"]<-"GCAM_region_ID"
names(an_FeedIO_R_C_S_Y)[names(an_FeedIO_R_C_S_Y)=="Group.2"]<-"commodity"
names(an_FeedIO_R_C_S_Y)[names(an_FeedIO_R_C_S_Y)=="Group.3"]<-"system"
names(an_FeedIO_R_C_S_Y)[names(an_FeedIO_R_C_S_Y)=="Group.4"]<-"year"
an_FeedIO_R_C_S_Y$IO_C_S_Y<-an_FeedIO_R_C_S_Y$Feed_C_S_Y/an_FeedIO_R_C_S_Y$Prod_C_S_Y
an_FeedIO_R_C_S_Y$ID_R_C_S_Y<-paste(an_FeedIO_R_C_S_Y$GCAM_region_ID,an_FeedIO_R_C_S_Y$commodity,an_FeedIO_R_C_S_Y$system,an_FeedIO_R_C_S_Y$year,sep="")

#Calculate input requirements by GCAM region, commodity, system, feed, and year
an_ProdFeed_Mt_R_C_S_F_Y<-an_Prod_Mt_R_C_S_F_Y
an_ProdFeed_Mt_R_C_S_F_Y$ID_R_C_S_Y<-paste(an_ProdFeed_Mt_R_C_S_F_Y$GCAM_region_ID,an_ProdFeed_Mt_R_C_S_F_Y$commodity,an_ProdFeed_Mt_R_C_S_F_Y$system,an_ProdFeed_Mt_R_C_S_F_Y$year,sep="")
an_ProdFeed_Mt_R_C_S_F_Y$IO<-an_FeedIO_R_C_S_Y$IO_C_S_Y[match(an_ProdFeed_Mt_R_C_S_F_Y$ID_R_C_S_Y,an_FeedIO_R_C_S_Y$ID_R_C_S_Y)]
an_FeedIO_Mt_R_C_S_F_Y<-an_ProdFeed_Mt_R_C_S_F_Y[,1:5]
an_FeedIO_Mt_R_C_S_F_Y$Feed_Mt<-an_ProdFeed_Mt_R_C_S_F_Y$Prod_Mt*an_ProdFeed_Mt_R_C_S_F_Y$IO
#Add IO coefs
an_FeedIO_Mt_R_C_S_F_Y$IO<-an_ProdFeed_Mt_R_C_S_F_Y$IO
#Replace all NaN values with 0 for feed, and with 100 (arbitrarily high number) for IO
an_FeedIO_Mt_R_C_S_F_Y$Feed_Mt[an_FeedIO_Mt_R_C_S_F_Y$Feed_Mt=="NaN"]<-0
an_FeedIO_Mt_R_C_S_F_Y$IO[an_FeedIO_Mt_R_C_S_F_Y$IO=="NaN"]<-100
#identifier vector
an_FeedIO_Mt_R_C_S_F_Y$ID_R_C_S_F_Y<-paste(an_FeedIO_Mt_R_C_S_F_Y$GCAM_region_ID,an_FeedIO_Mt_R_C_S_F_Y$commodity,an_FeedIO_Mt_R_C_S_F_Y$system,an_FeedIO_Mt_R_C_S_F_Y$feed,an_FeedIO_Mt_R_C_S_F_Y$year,sep="")

#Write out table of feed inputs and IO coefs by region, commodity, system, feed, and year
write.table(an_FeedIO_Mt_R_C_S_F_Y,file="Rdata_out/an_FeedIO_Mt_R_C_S_F_Y_07.csv",sep=",",col.names=TRUE,row.names=FALSE)
