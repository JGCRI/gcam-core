# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L210.resources.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Resource supply information" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_rsrc_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A10.rsrc_info <- readdata( "ENERGY_ASSUMPTIONS", "A10.rsrc_info")
A10.subrsrc_info <- readdata( "ENERGY_ASSUMPTIONS", "A10.subrsrc_info")
A10.TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A10.TechChange")
A15.roofPV_TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A15.roofPV_TechChange" )
L111.RsrcCurves_EJ_R_Ffos <- readdata( "ENERGY_LEVEL1_DATA", "L111.RsrcCurves_EJ_R_Ffos" )
L111.Prod_EJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L111.Prod_EJ_R_F_Yh" )
L112.RsrcCurves_Mt_R_U <- readdata( "ENERGY_LEVEL1_DATA", "L112.RsrcCurves_Mt_R_U" )
L113.RsrcCurves_EJ_R_MSW <- readdata( "ENERGY_LEVEL1_DATA", "L113.RsrcCurves_EJ_R_MSW" )
L114.RsrcCurves_EJ_R_wind <- readdata( "ENERGY_LEVEL1_DATA", "L114.RsrcCurves_EJ_R_wind" )
L115.RsrcCurves_EJ_R_roofPV <- readdata( "ENERGY_LEVEL1_DATA", "L115.RsrcCurves_EJ_R_roofPV" )
L116.RsrcCurves_EJ_R_geo <- readdata( "ENERGY_LEVEL1_DATA", "L116.RsrcCurves_EJ_R_geo" )
L116.RsrcCurves_EJ_R_EGS <- readdata( "ENERGY_LEVEL1_DATA", "L116.RsrcCurves_EJ_R_EGS" )
L117.RsrcCurves_EJ_R_tradbio <- readdata( "ENERGY_LEVEL1_DATA", "L117.RsrcCurves_EJ_R_tradbio" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Output unit, price unit, market
#Interpolate to specified historical years, as necessary
L210.rsrc_info <- gcam_interp( A10.rsrc_info, model_base_years )

#Repeat and add region vector to resource assumptions table (use ID to ensure correct region ordering)
L210.rsrc_info <- repeat_and_add_vector( A10.rsrc_info, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L210.rsrc_info <- add_region_name( L210.rsrc_info )

#Remove traditional biomass from regions where it does not apply
rm_tradbio_regions <- paste( A_regions[[R]][ A_regions$tradbio_region == 0 ], L117.RsrcCurves_EJ_R_tradbio$resource[1] )
L210.rsrc_info <- subset( L210.rsrc_info, !paste( GCAM_region_ID, resource ) %in% rm_tradbio_regions )

#Reset regional markets to the names of the specific regions
L210.rsrc_info$market[ L210.rsrc_info$market == "regional" ] <- L210.rsrc_info$region[ L210.rsrc_info$market == "regional" ]

#Split different types of resources into separate tables
L210.dep_rsrc_info <- subset( L210.rsrc_info, resource_type == "depresource" )
L210.renew_rsrc_info <- subset( L210.rsrc_info, resource_type == "renewresource" )
L210.unlim_rsrc_info <- subset( L210.rsrc_info, resource_type == "unlimited-resource" )

printlog( "L210.DepRsrc: output unit, price unit, and market for depletable resources" )
L210.DepRsrc <- data.frame(
      region = L210.dep_rsrc_info$region,
      depresource = L210.dep_rsrc_info$resource,
      output.unit = L210.dep_rsrc_info$output.unit,
      price.unit = L210.dep_rsrc_info$price.unit,
      market = L210.dep_rsrc_info$market)

printlog( "L210.RenewRsrc: output unit, price unit, and market for renewable resources" )
L210.RenewRsrc <- data.frame(
      region = L210.renew_rsrc_info$region,
      renewresource = L210.renew_rsrc_info$resource,
      output.unit = L210.renew_rsrc_info$output.unit,
      price.unit = L210.renew_rsrc_info$price.unit,
      market = L210.renew_rsrc_info$market)

printlog( "L210.UnlimitRsrc: output unit, price unit, and market for unlimited resources" )
L210.UnlimitRsrc <- data.frame(
      region = L210.unlim_rsrc_info$region,
      unlimited.resource = L210.unlim_rsrc_info$resource,
      output.unit = L210.unlim_rsrc_info$output.unit,
      price.unit = L210.unlim_rsrc_info$price.unit,
      market = L210.unlim_rsrc_info$market,
      capacity.factor = L210.unlim_rsrc_info$capacity.factor )

printlog( "L210.DepRsrcPrice: historical prices for depletable resources" )
L210.dep_rsrc_price.melt <- interpolate_and_melt(
      L210.dep_rsrc_info[ names( L210.dep_rsrc_info ) %in% c( "region", "resource", X_historical_years ) ],
      model_base_years )
L210.DepRsrcPrice <- data.frame(
      region = L210.dep_rsrc_price.melt$region,
      depresource = L210.dep_rsrc_price.melt$resource,
      year = L210.dep_rsrc_price.melt$year,
      price = L210.dep_rsrc_price.melt$value )

printlog( "L210.RenewRsrcPrice: historical prices for renewable resources" )
L210.renew_rsrc_price.melt <- interpolate_and_melt(
      L210.renew_rsrc_info[ names( L210.renew_rsrc_info ) %in% c( "region", "resource", X_historical_years ) ],
      model_base_years )
L210.RenewRsrcPrice <- data.frame(
      region = L210.renew_rsrc_price.melt$region,
      renewresource = L210.renew_rsrc_price.melt$resource,
      year = L210.renew_rsrc_price.melt$year,
      price = L210.renew_rsrc_price.melt$value )

printlog( "L210.UnlimitRsrcPrice: prices for unlimited resources" )
L210.unlimit_rsrc_price.melt <- interpolate_and_melt(
      L210.unlim_rsrc_info[ names( L210.unlim_rsrc_info ) %in% c( "region", "resource", X_historical_years ) ],
      model_base_years )
L210.UnlimitRsrcPrice <- data.frame(
      region = L210.unlimit_rsrc_price.melt$region,
      unlimited.resource = L210.unlimit_rsrc_price.melt$resource,
      year = L210.unlimit_rsrc_price.melt$year,
      price = L210.unlimit_rsrc_price.melt$value )

# 2b. Tech change
# Repeat and add region vector to assumed techchange tables
L210.rsrc_TechChange <- repeat_and_add_vector( A10.TechChange, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L210.rsrc_TechChange <- add_region_name( L210.rsrc_TechChange )

#Retrieve the techChange years from the table column names
X_rsrc_TechChangeYears <- names( A10.TechChange )[ grep( "X[0-9]{4}", names( A10.TechChange ) )]

#Melt
#NOTE: assuming no tech change for unlimited resources
L210.rsrc_TechChange.melt <- melt( L210.rsrc_TechChange[ c( "region", "resource", "subresource", X_rsrc_TechChangeYears ) ],
      id.vars = c( "region", "resource", "subresource" ) )
L210.rsrc_TechChange.melt$subresource_type <- A10.subrsrc_info$subresource_type[ match( L210.rsrc_TechChange.melt$subresource, A10.subrsrc_info$subresource ) ]

#Rooftop PV: follow same steps
L210.roofPV_TechChange <- repeat_and_add_vector( A15.roofPV_TechChange, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L210.roofPV_TechChange <- add_region_name( L210.roofPV_TechChange )
X_roofPV_TechChangeYears <- names( A15.roofPV_TechChange )[ grep( "X[0-9]{4}", names( A15.roofPV_TechChange ) )]
L210.roofPV_TechChange.melt <- melt( L210.roofPV_TechChange[ c( "region", "resource", "subresource", X_roofPV_TechChangeYears ) ],
      id.vars = c( "region", "resource", "subresource" ) )
L210.roofPV_TechChange.melt$subresource_type <- A10.subrsrc_info$subresource_type[ match( L210.roofPV_TechChange.melt$subresource, A10.subrsrc_info$subresource ) ]

#Combine (rbind) these two tables
L210.rsrc_TechChange.melt <- rbind( L210.rsrc_TechChange.melt, L210.roofPV_TechChange.melt )
L210.dep_rsrc_TechChange.melt <- subset( L210.rsrc_TechChange.melt, subresource_type == "subresource" )
L210.renew_rsrc_TechChange.melt <- subset( L210.rsrc_TechChange.melt, subresource_type == "smooth-renewable-subresource" )

printlog( "L210.DepRsrcTechChange: technological change for depletable resources" )
L210.DepRsrcTechChange <- data.frame(
      region = L210.dep_rsrc_TechChange.melt$region,
      depresource = L210.dep_rsrc_TechChange.melt$resource,
      subresource = L210.dep_rsrc_TechChange.melt$subresource,
      year.fillout = substr( L210.dep_rsrc_TechChange.melt$variable, 2, 5 ),
      techChange = L210.dep_rsrc_TechChange.melt$value )

printlog( "L210.SmthRenewRsrcTechChange: technological change for smooth renewable subresources" )
L210.SmthRenewRsrcTechChange <- data.frame(
      region = L210.renew_rsrc_TechChange.melt$region,
      renewresource = L210.renew_rsrc_TechChange.melt$resource,
      smooth.renewable.subresource = L210.renew_rsrc_TechChange.melt$subresource,
      year.fillout = substr( L210.renew_rsrc_TechChange.melt$variable, 2, 5 ),
      techChange = L210.renew_rsrc_TechChange.melt$value )

# 2c. Calibrated production (depletable resources only)
printlog( "L210.DepRsrcCalProd: calibrated production of depletable resources" )
printlog( "NOTE: Assuming only one calibrated subresource per depletable resource" )
printlog( "NOTE: Unconventional oil production is calibrated in the traded unconventional oil technology")
# This is complicated. If the unconventional oil production is calibrated in the resource, then there is
# no way to interpolate the base-year price-adders (calibration parameters) in the future. Regions that do
# not produce in the base years effectively come in with no price wedge in the first future time period,
# and those with the price wedge have their base-year behavior essentially carried forward to all periods.
# Calibrating this in the "traded unconventional oil" sectors allows for shareweight interpolation.
L210.Prod_EJ_R_F_Y <- L111.Prod_EJ_R_F_Yh[
      L111.Prod_EJ_R_F_Yh$fuel != "unconventional oil", c( R_S_F, X_model_base_years ) ]
L210.Prod_EJ_R_F_Y.melt <- melt( L210.Prod_EJ_R_F_Y, id.vars = c( R_S_F ) )
L210.Prod_EJ_R_F_Y.melt <- add_region_name( L210.Prod_EJ_R_F_Y.melt )

L210.DepRsrcCalProd <- data.frame(
      region = L210.Prod_EJ_R_F_Y.melt$region,
      depresource = L210.Prod_EJ_R_F_Y.melt$fuel,
      subresource = A10.TechChange$subresource[ match( L210.Prod_EJ_R_F_Y.melt$fuel, A10.TechChange$resource ) ],
      year = substr( L210.Prod_EJ_R_F_Y.melt$variable, 2, 5 ),
      cal.production = round( L210.Prod_EJ_R_F_Y.melt$value, digits_calproduction ) )
      
# 2d. Resource supply curves
printlog( "L210.DepRsrcCurves_fos: supply curves of fossil resources")
L210.RsrcCurves_EJ_R_Ffos <- add_region_name( L111.RsrcCurves_EJ_R_Ffos )
L210.DepRsrcCurves_fos <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_Ffos, "depresource" )

printlog( "L210.DepRsrcCurves_U: supply curves of uranium resources")
L210.RsrcCurves_Mt_R_U <- add_region_name( L112.RsrcCurves_Mt_R_U )
L210.DepRsrcCurves_U <- convert_rsrc_to_L2( L210.RsrcCurves_Mt_R_U, "depresource" )

printlog( "L210.SmthRenewRsrcCurves_MSW: supply curves of waste biomass resources")
L210.RsrcCurves_EJ_R_MSW <- add_region_name( L113.RsrcCurves_EJ_R_MSW )
L210.SmthRenewRsrcCurvesGdpElast_MSW <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_MSW, "renewresource", "smooth-renewable-subresource" )

printlog( "L210.SmthRenewRsrcCurves_wind: supply curves of wind resources")
L210.RsrcCurves_EJ_R_wind <- add_region_name( L114.RsrcCurves_EJ_R_wind )
L210.SmthRenewRsrcCurves_wind <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_wind, "renewresource", "smooth-renewable-subresource" )

printlog( "L210.SmthRenewRsrcCurves_roofPV: supply curves of rooftop PV resources")
L210.RsrcCurves_EJ_R_roofPV <- add_region_name( L115.RsrcCurves_EJ_R_roofPV )
L210.SmthRenewRsrcCurves_roofPV <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_roofPV, "renewresource", "smooth-renewable-subresource" )

printlog( "L210.GrdRenewRsrcCurves_geo: graded supply curves of geothermal (hydrothermal) resources")
L210.RsrcCurves_EJ_R_geo <- add_region_name( L116.RsrcCurves_EJ_R_geo )
L210.GrdRenewRsrcCurves_geo <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_geo, "renewresource", "sub-renewable-resource" )

printlog( "L210.GrdRenewRsrcMax_geo: default max sub resource of geothermal (hydrothermal) resources")
L210.GrdRenewRsrcMax_geo <- subset( L210.GrdRenewRsrcCurves_geo, grade = unique( grade )[1] )
L210.GrdRenewRsrcMax_geo$maxSubResource <- 1
L210.GrdRenewRsrcMax_geo <- L210.GrdRenewRsrcMax_geo[ names_maxSubResource ]

printlog( "L210.GrdRenewRsrcCurves_EGS: graded supply curves of geothermal (EGS) resources")
L210.RsrcCurves_EJ_R_EGS <- add_region_name( L116.RsrcCurves_EJ_R_EGS )
L210.GrdRenewRsrcCurves_EGS <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_EGS, "renewresource", "sub-renewable-resource" )

printlog( "L210.GrdRenewRsrcMax_EGS: default max sub resource of EGS resources")
L210.GrdRenewRsrcMax_EGS <- subset( L210.GrdRenewRsrcCurves_EGS, grade = unique( grade )[1] )
L210.GrdRenewRsrcMax_EGS$maxSubResource <- 1
L210.GrdRenewRsrcMax_EGS <- L210.GrdRenewRsrcMax_EGS[ names_maxSubResource ]

printlog( "L210.GrdRenewRsrcCurves_tradbio: graded supply curves of traditional biomass resources")
L210.RsrcCurves_EJ_R_tradbio <- add_region_name( L117.RsrcCurves_EJ_R_tradbio )
L210.GrdRenewRsrcCurves_tradbio <- convert_rsrc_to_L2( L210.RsrcCurves_EJ_R_tradbio, "renewresource", "sub-renewable-resource" )

printlog( "L210.GrdRenewRsrcMax_tradbio: default max sub resource of tradbio resources")
L210.GrdRenewRsrcMax_tradbio <- subset( L210.GrdRenewRsrcCurves_tradbio, grade == unique( grade )[1] )
L210.GrdRenewRsrcMax_tradbio$maxSubResource <- 1
L210.GrdRenewRsrcMax_tradbio <- L210.GrdRenewRsrcMax_tradbio[ names_maxSubResource ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L210.DepRsrc, IDstring="DepRsrc", domain="ENERGY_LEVEL2_DATA", fn="L210.DepRsrc", batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_resources.xml" ) 
write_mi_data( L210.RenewRsrc, "RenewRsrc", "ENERGY_LEVEL2_DATA", "L210.RenewRsrc", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.UnlimitRsrc, "UnlimitRsrc", "ENERGY_LEVEL2_DATA", "L210.UnlimitRsrc", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.DepRsrcPrice, "DepRsrcPrice", "ENERGY_LEVEL2_DATA", "L210.DepRsrcPrice", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.RenewRsrcPrice, "RenewRsrcPrice", "ENERGY_LEVEL2_DATA", "L210.RenewRsrcPrice", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.UnlimitRsrcPrice, "UnlimitRsrcPrice", "ENERGY_LEVEL2_DATA", "L210.UnlimitRsrcPrice", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.DepRsrcTechChange, "DepRsrcTechChange", "ENERGY_LEVEL2_DATA", "L210.DepRsrcTechChange", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.SmthRenewRsrcTechChange, "SmthRenewRsrcTechChange", "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcTechChange", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.DepRsrcCalProd, "DepRsrcCalProd", "ENERGY_LEVEL2_DATA", "L210.DepRsrcCalProd", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.DepRsrcCurves_fos, "DepRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.DepRsrcCurves_fos", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.DepRsrcCurves_U, "DepRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.DepRsrcCurves_U", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.SmthRenewRsrcCurvesGdpElast_MSW, "SmthRenewRsrcCurvesGdpElast", "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcCurves_MSW", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.SmthRenewRsrcCurves_wind, "SmthRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcCurves_wind", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.SmthRenewRsrcCurves_roofPV, "SmthRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcCurves_roofPV", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.GrdRenewRsrcCurves_geo, "GrdRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcCurves_geo", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.GrdRenewRsrcMax_geo, "GrdRenewRsrcMax", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcMax_geo", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.GrdRenewRsrcCurves_EGS, "GrdRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcCurves_EGS", "ENERGY_XML_BATCH", "batch_geo_adv.xml" ) 
write_mi_data( L210.GrdRenewRsrcMax_EGS, "GrdRenewRsrcMax", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcMax_EGS", "ENERGY_XML_BATCH", "batch_geo_adv.xml" ) 
write_mi_data( L210.GrdRenewRsrcCurves_tradbio, "GrdRenewRsrcCurves", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcCurves_tradbio", "ENERGY_XML_BATCH", "batch_resources.xml" ) 
write_mi_data( L210.GrdRenewRsrcMax_tradbio, "GrdRenewRsrcMax", "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcMax_tradbio", "ENERGY_XML_BATCH", "batch_resources.xml" ) 

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_resources.xml", "ENERGY_XML_FINAL", "resources.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_geo_adv.xml", "ENERGY_XML_FINAL", "geo_adv.xml", "", xml_tag="outFile" )

logstop()
