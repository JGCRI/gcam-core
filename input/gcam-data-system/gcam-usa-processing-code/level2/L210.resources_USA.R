if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L210.resources_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for GCAM USA resources" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" )
us_state_wind <- readdata( "GCAMUSA_GIS_DATA", "us_state_wind" )
L115.rsrc_state_rooftopPV <- readdata( "GCAMUSA_LEVEL1_DATA", "L115.rsrc_state_rooftopPV" )
L1231.out_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.out_EJ_state_elec_F_tech" )
L1321.out_Mt_state_cement_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1321.out_Mt_state_cement_Yh" )
L210.RenewRsrc <- readdata( "ENERGY_LEVEL2_DATA", "L210.RenewRsrc", skip = 4 )
L210.UnlimitRsrc <- readdata( "ENERGY_LEVEL2_DATA", "L210.UnlimitRsrc", skip = 4 )
L210.UnlimitRsrcPrice <- readdata( "ENERGY_LEVEL2_DATA", "L210.UnlimitRsrcPrice", skip = 4 )
L210.SmthRenewRsrcTechChange <- readdata( "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcTechChange", skip = 4 )
L210.SmthRenewRsrcCurves_wind <- readdata( "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcCurves_wind", skip = 4 )
L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV <- readdata( "ENERGY_LEVEL2_DATA", "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV", skip = 4 )
L210.GrdRenewRsrcCurves_geo <- readdata( "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcCurves_geo", skip = 4 )
L210.GrdRenewRsrcMax_geo <- readdata( "ENERGY_LEVEL2_DATA", "L210.GrdRenewRsrcMax_geo", skip = 4 )

# -----------------------------------------------------------------------------
cement_states <- unique( L1321.out_Mt_state_cement_Yh$state )

# 2. Perform computations
printlog( "NOTE: geothermal resource is not created in the states considered to have zero hydrothermal production available" )
NREL_us_re_technical_potential$state <- states_subregions$state[ match( NREL_us_re_technical_potential$State, states_subregions$state_name ) ]
NREL_us_re_technical_potential$geothermal <- NREL_us_re_technical_potential$Geothermal_Hydrothermal_GWh * conv_GWh_EJ / geothermal_default_efficiency
geo_states <- states[ !states %in% NREL_us_re_technical_potential$state[ NREL_us_re_technical_potential$geothermal == 0 ] ]
geo_states_noresource <- paste( states[ !states %in% geo_states ], "geothermal" )

printlog( "NOTE: keeping limestone resources separate, written out to XML batch file for cement" )
printlog( "L210.DeleteRenewRsrc_USArsrc: remove selected renewable resources from the USA region" )
L210.DeleteRenewRsrc_USArsrc <- subset( L210.RenewRsrc[ c( reg, "renewresource" ) ],
      region == "USA" & renewresource %in% state_renewable_resources )

printlog( "L210.DeleteUnlimitRsrc_USArsrc: remove selected renewable resources from the USA region" )
L210.DeleteUnlimitRsrc_USArsrc <- subset( L210.UnlimitRsrc[ c( reg, "unlimited.resource" ) ],
      region == "USA" & unlimited.resource %in% state_unlimited_resources )
#Remove limestone, in order to write this out to a different batch xml file
L210.DeleteUnlimitRsrc_USAlimestone <- subset( L210.DeleteUnlimitRsrc_USArsrc, unlimited.resource == "limestone" )
L210.DeleteUnlimitRsrc_USArsrc <- subset( L210.DeleteUnlimitRsrc_USArsrc, unlimited.resource != "limestone" )

printlog( "L210.RenewRsrc_USA: renewable resource info in the states" )
L210.RenewRsrc_USA <- write_to_all_states( subset( L210.RenewRsrc,
      region == "USA" & renewresource %in% state_renewable_resources ), names_RenewRsrc )
#Re-set the market to the states
L210.RenewRsrc_USA$market <- L210.RenewRsrc_USA$region
#Drop geothermal in states where no resources exist
L210.RenewRsrc_USA <- subset( L210.RenewRsrc_USA, !paste( region, renewresource ) %in% geo_states_noresource )

printlog( "L210.UnlimitRsrc_USA: unlimited resource info in the states" )
L210.UnlimitRsrc_USA <- write_to_all_states( subset( L210.UnlimitRsrc,
      region == "USA" & unlimited.resource %in% state_unlimited_resources ), names_UnlimitRsrc )
L210.UnlimitRsrc_limestone_USA <- subset( L210.UnlimitRsrc_USA, unlimited.resource == "limestone" & region %in% cement_states )
L210.UnlimitRsrc_USA <- subset( L210.UnlimitRsrc_USA, unlimited.resource != "limestone" )

printlog( "L210.UnlimitRsrcPrice_USA: unlimited resource prices in the states" )
L210.UnlimitRsrcPrice_USA <- write_to_all_states( subset( L210.UnlimitRsrcPrice,
      region == "USA" & unlimited.resource %in% state_unlimited_resources ), names_UnlimitRsrcPrice )
L210.UnlimitRsrcPrice_limestone_USA <- subset( L210.UnlimitRsrcPrice_USA, unlimited.resource == "limestone" & region %in% cement_states )
L210.UnlimitRsrcPrice_USA <- subset( L210.UnlimitRsrcPrice_USA, unlimited.resource != "limestone" )

printlog( "L210.SmthRenewRsrcTechChange_USA: smooth renewable resource tech change" )
L210.SmthRenewRsrcTechChange_USA <- write_to_all_states( subset( L210.SmthRenewRsrcTechChange,
      region == "USA" & renewresource %in% state_renewable_resources ), names_SmthRenewRsrcTechChange )
#If geothermal is included in this table, remove states that don't exist
L210.SmthRenewRsrcTechChange_USA <- subset( L210.SmthRenewRsrcTechChange_USA, !paste( region, renewresource ) %in% geo_states_noresource )

printlog( "L210.SmthRenewRsrcCurves_wind_USA: wind resource curves in the states" )
# Convert us_state_wind units from 2007$/kWh to 1975$/GJ
us_state_wind$mid_price_75USDGJ <- us_state_wind$mid_price * conv_2007_1975_USD / conv_kwh_GJ
L210.SmthRenewRsrcCurves_wind_USA <- repeat_and_add_vector(
      subset( L210.SmthRenewRsrcCurves_wind, region == "USA" ), reg, states )
L210.SmthRenewRsrcCurves_wind_USA[ c( "maxSubResource", "mid.price", "curve.exponent" ) ] <- us_state_wind[
      match( L210.SmthRenewRsrcCurves_wind_USA$region, us_state_wind$region ),
      c( "maxResource", "mid_price_75USDGJ", "curve_exponent" ) ]

printlog( "L210.GrdRenewRsrcCurves_geo_USA: geothermal resource curves in the states" )
L210.GrdRenewRsrcCurves_geo_USA <- subset( L210.GrdRenewRsrcCurves_geo, region == "USA" )
#Calculate the cost increment between grades 1 and 2
L210.GeoGrade2Increment <- L210.GrdRenewRsrcCurves_geo_USA$extractioncost[ L210.GrdRenewRsrcCurves_geo_USA$grade == "grade 2" ] -
      L210.GrdRenewRsrcCurves_geo_USA$extractioncost[ L210.GrdRenewRsrcCurves_geo_USA$grade == "grade 1" ]

#Calculate the share of the resource to allocate to each grade
printlog( "NOTE: The method here gives precendence to the state-level NREL data in defining the quantities" )
L210.GrdRenewRsrcCurves_geo_USA$grade_share <- L210.GrdRenewRsrcCurves_geo_USA$available / sum( L210.GrdRenewRsrcCurves_geo_USA$available )
L210.GrdRenewRsrcCurves_geo_USA <- repeat_and_add_vector( L210.GrdRenewRsrcCurves_geo_USA, reg, geo_states )
L210.GrdRenewRsrcCurves_geo_USA$available <- round(
      L210.GrdRenewRsrcCurves_geo_USA$grade_share * NREL_us_re_technical_potential$geothermal[
         match( L210.GrdRenewRsrcCurves_geo_USA$region, NREL_us_re_technical_potential$state ) ],
      digits_calOutput )

#This method is problematic in that each state is assigned the same cost points, even though costs are obviously different by state
# We don't have any data indicating the cost of the next geothermal power station by state, so we'll use the historical generation
# to modify the floor of the cost curve in each state. This is ad-hoc and can be improved at some point.
L210.GrdRenewRsrcCurves_geo_USA$offtake <- L1231.out_EJ_state_elec_F_tech[[X_final_historical_year]][
      match( vecpaste( L210.GrdRenewRsrcCurves_geo_USA[ c( reg, "renewresource" ) ] ),
             vecpaste( L1231.out_EJ_state_elec_F_tech[ c( state, "fuel" ) ] ) ) ] /
      geothermal_default_efficiency
L210.GrdRenewRsrcCurves_geo_USA$offtake_share <- L210.GrdRenewRsrcCurves_geo_USA$offtake / L210.GrdRenewRsrcCurves_geo_USA$available
L210.GrdRenewRsrcCurves_geo_USA$offtake_share[ L210.GrdRenewRsrcCurves_geo_USA$available == 0 ] <-
      L210.GrdRenewRsrcCurves_geo_USA$offtake_share[ L210.GrdRenewRsrcCurves_geo_USA$available != 0 ]

#Index everything to the state with the largest share of its resource in use. That will get the floor of the cost curve
L210.GrdRenewRsrcCurves_geo_USA$cost_modifier <- 1 - ( L210.GrdRenewRsrcCurves_geo_USA$offtake_share / max( L210.GrdRenewRsrcCurves_geo_USA$offtake_share ) )
                                                 
# This term sets the maximum quantity that will be added to the grade 1 cost; it is implemented here as a fraction of the increment from grade 1 to grade 2
L210.MaxIncrement <- L210.GeoGrade2Increment * 0.5
L210.GrdRenewRsrcCurves_geo_USA$extractioncost[ L210.GrdRenewRsrcCurves_geo_USA$grade == "grade 1" ] <-
      round( L210.GrdRenewRsrcCurves_geo_USA$extractioncost[ L210.GrdRenewRsrcCurves_geo_USA$grade == "grade 1" ] + L210.MaxIncrement *             
             L210.GrdRenewRsrcCurves_geo_USA$cost_modifier[ L210.GrdRenewRsrcCurves_geo_USA$grade == "grade 1" ],
             digits_cost )
L210.GrdRenewRsrcCurves_geo_USA <- L210.GrdRenewRsrcCurves_geo_USA[ names_RenewRsrcCurves ]

#Maximum resources: currently assuming this is just set to 1, and the resource info is stored in the grades
printlog( "L210.GrdRenewRsrcMax_geo_USA: max sub resource for geothermal (placeholder)")
L210.GrdRenewRsrcMax_geo_USA <- repeat_and_add_vector( subset( L210.GrdRenewRsrcMax_geo, region == "USA" ), reg, geo_states )

printlog( "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA: rooftop PV resource curves in the states" )
L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA <- repeat_and_add_vector( subset(
      L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV, region == "USA" ), reg, states )
L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA[ c( "maxSubResource", "mid.price", "curve.exponent" ) ] <- L115.rsrc_state_rooftopPV[
      match( L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA$region, L115.rsrc_state_rooftopPV$state ),
      c( "generation", "mid_p", "b_exp" ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L210.DeleteRenewRsrc_USArsrc, "DeleteRenewRsrc", "GCAMUSA_LEVEL2_DATA", "L210.DeleteRenewRsrc_USArsrc", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.DeleteUnlimitRsrc_USArsrc, "DeleteUnlimitRsrc", "GCAMUSA_LEVEL2_DATA", "L210.DeleteUnlimitRsrc_USArsrc", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.RenewRsrc_USA, "RenewRsrc", "GCAMUSA_LEVEL2_DATA", "L210.RenewRsrc_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.UnlimitRsrc_USA, "UnlimitRsrc", "GCAMUSA_LEVEL2_DATA", "L210.UnlimitRsrc_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.UnlimitRsrcPrice_USA, "UnlimitRsrcPrice", "GCAMUSA_LEVEL2_DATA", "L210.UnlimitRsrcPrice_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.SmthRenewRsrcTechChange_USA, "SmthRenewRsrcTechChange", "GCAMUSA_LEVEL2_DATA", "L210.SmthRenewRsrcTechChange_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.SmthRenewRsrcCurves_wind_USA, "SmthRenewRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L210.SmthRenewRsrcCurves_wind_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.GrdRenewRsrcCurves_geo_USA, "GrdRenewRsrcCurves", "GCAMUSA_LEVEL2_DATA", "L210.GrdRenewRsrcCurves_geo_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.GrdRenewRsrcMax_geo_USA, "GrdRenewRsrcMax", "GCAMUSA_LEVEL2_DATA", "L210.GrdRenewRsrcMax_geo_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )
write_mi_data( L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA, "SmthRenewRsrcCurvesGdpElast", "GCAMUSA_LEVEL2_DATA",
              "L210.SmthRenewRsrcCurvesGdpElastCapFac_roofPV_USA", "GCAMUSA_XML_BATCH", "batch_resources_USA.xml" )

write_mi_data( L210.DeleteUnlimitRsrc_USAlimestone, "DeleteUnlimitRsrc", "GCAMUSA_LEVEL2_DATA", "L210.DeleteUnlimitRsrc_USAlimestone", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L210.UnlimitRsrc_limestone_USA, "UnlimitRsrc", "GCAMUSA_LEVEL2_DATA", "L210.UnlimitRsrc_limestone_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )
write_mi_data( L210.UnlimitRsrcPrice_limestone_USA, "UnlimitRsrcPrice", "GCAMUSA_LEVEL2_DATA", "L210.UnlimitRsrcPrice_limestone_USA", "GCAMUSA_XML_BATCH", "batch_cement_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_resources_USA.xml", "GCAMUSA_XML_FINAL", "resources_USA.xml", "", xml_tag="outFile" )

logstop()
