# ---------------------------------------------------------------------------
# Program Name: ICE_vehicle_cost_update.R
# Author: Brinda Yarlagadda and Patrick O'Rourke
# Date Last Updated: 11.20.19
# Program Purpose: To update ICE vehicle cost assumptions
# Input Files: UCD_transportation_database.csv,
#              EFS_Technology_Data.xlsx,
#              EFS_70485_figure_data.xlsx,
#              NREL_to_UCD_mapping.xlsx,
#              Transportation_Energy_Data_Book_Ed37_2019.xlsx
# Output Files: UCD-original_data_which_was_updated.csv,
#               UCD_updated_data_with_NREL.csv,
#               UCD_transportation_db_new.csv
# Notes:
# TODO:

# ------------------------------------------------------------------------------

# 0 computer Options
#   Pick other options
    user <- "Brinda"         # Options only defined for "Pat" and "Brinda"
    computer_type <- "PC" # Only needed if user set to "Pat" - Computer type (MAC or PC)

# ------------------------------------------------------------------------------

# 0.5 Pick options and define constants and functions

#   Pick NREL options
    NREL_EFS_case <- "Rapid Advancement"
    NREL_EFS_ICE_case <- "PATHWAYS Reference"
    AEO_CAFE_case <- "AEO 2019"

#   Define constants
    NREL_VEH_LIST <- c( "Buses", "Medium Duty Trucks", "Heavy Duty Trucks",
                        "Light Duty Cars", "Light Duty Trucks" )
    CONV_2016_TO_2005 <- (100/105.935)*(87.421/100)
    NREL_YEARS <- c( 2015, seq( 2020, 2050, 10 ) )
    NREL_YEARS_X <- paste0( "X", NREL_YEARS )
    UCD_YEARS <-  c( 2005, seq( 2020, 2095, 15 ) )
    UCD_YEARS_X <- paste0( "X", UCD_YEARS )
    COMPLETE_YEARS <- c(seq(2005,2100,5))
    COMPLETE_YEARS_X <- paste0 ( "X", COMPLETE_YEARS)
    AEO_CAFE_YEARS <- paste0(c(seq(2015, 2050, 5)))

    FCR_VEH <- 0.1627454 # Fixed charge rate, from A_trn_data
    
    Wh_per_gal <- 33700 #Fuel economy conversion factor from (NREL EFS, Source: DOE 2017a)
    MJ_per_Wh <- 0.0036
    km_per_mi <- 1.60934

    #   Define functions for interpolating between years
    all.na_new  <- function(x) return( all( is.na(x) ) )
    
    interpolate_NAs_new <- function(df) {
      
      if( !is.data.frame( df ) ) {
        
        warning( "interpolate_NAs expects a data frame; attempting to convert" )
        
        df <- as.data.frame( df )
        
      }
      
      #   Convert columns that are all NA to numeric
      df <- dplyr::mutate_if( df, function( x ) all.na_new( x ), as.numeric )
      
      if( length( rle( sapply( df, is.numeric ) )$lengths ) > 2) {
        
        warning( "interpolate_NAs found mixed numeric and non-numeric columns;
                  make sure all value columns are numeric")
        
      }
      
      value.cols <- sapply( df, is.numeric )
      df_flipped <- t( df[value.cols] )
      df[ , value.cols] <- t( na.approx( df_flipped, na.rm = F ) )
      
      return(df)
      
    }
    
    #   USEFUL FUNCTIONS
    #Function "is not an element of" (opposite of %in%)
    '%!in%' <- function( x, y ) !( '%in%'( x, y ) )

# ------------------------------------------------------------------------------

# I. Set WD, Load packages, and Load data

#   Set WD
    if( user == "Brinda" ){

      setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Input" )

    } else if( user == "Pat" ){

      if( computer_type == "MAC" ){

        setwd( "/Users/patrickorourke/Desktop/Vehicle_costs/Input" )

      } else {

        setwd( "C:/Users/orou913/Desktop/Vehicle_costs/Input" )

      }

    } else {

      stop ( paste0( "setwd() Options are not set for this user. See Section 1." ) )

    }

#   Load packages
    library( "dplyr" )
    library( "tidyr" )
    library( "openxlsx" )
    library( "zoo" )

# #   Load UCD transportation data
#     UCD <- read.csv( "UCD_transportation_database.csv",
#                      header = TRUE )

#   Load UCD transportation data
    UCD <- read.csv( "UCD_trn_data_CORE_ref.csv", skip=7 )

#   Load NREL transportation data
    NREL_ICE_LDC_costs <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "4", startRow = 5)
    NREL_ICE_LDT_costs <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "5", startRow = 5)
    NREL_ICE_MDT_costs <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "8", startRow = 4)
    NREL_ICE_HDT_costs <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "9", startRow = 4)
    NREL_ICE_Buses_costs <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "11", startRow = 4)
    
    NREL_ICE_LDC_fuel_eff <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "6", startRow = 4)
    NREL_ICE_LDT_fuel_eff <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "7", startRow = 5)
    NREL_ICE_MDT_HDT_fuel_eff <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "10", startRow = 4)
    NREL_ICE_Buses_fuel_eff <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "12", startRow = 4)
    
    AEO_CAFE_ICE_fuel_eff <- read.xlsx( xlsxFile = "AEO_CAFE_standards.xlsx", sheet = "Sheet1", startRow = 1)

#   Load NREL mapping files
    NREL_var_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Variables" )
    NREL_veh_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Vehicles" )
    NREL_region_tech_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Region_Tech" )
    UCD_Truck_size_class_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Truck_size_class")
    NREL_conv_main_costs <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Conv_main_costs")
    TEDB_vkt_veh_yr <- read.xlsx( xlsxFile = "Transportation_Energy_Data_Book_Ed37_2019.xlsx", sheet = "vkt_veh_yr")

    NREL_ICE_years <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "ICE_years")
# ------------------------------------------------------------------------------

# II. Initial processing of original UCD data and NREL data

#   A. Clean UCD Data - transform identification variables to characters
    UCD_clean <- UCD %>%
      dplyr::mutate( UCD_region = as.character( UCD_region ),
                     UCD_sector = as.character( UCD_sector ),
                     mode = as.character( mode ),
                     size.class = as.character( size.class ),
                     UCD_technology = as.character( UCD_technology ),
                     UCD_fuel = as.character( UCD_fuel ),
                     variable = as.character( variable ),
                     unit = as.character( unit ) )

#   B. Clean NREL Data
    NREL_ICE_LDC_costs_clean <- NREL_ICE_LDC_costs %>%
      dplyr::rename( Case = EFS.Case.or.Source,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Light Duty Cars" )

    NREL_ICE_LDT_costs_clean <- NREL_ICE_LDT_costs %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Light Duty Trucks")

    NREL_ICE_MDT_costs_clean <- NREL_ICE_MDT_costs %>%
      dplyr::rename( Case = Electric.Range.Case,
                     Technology = Battery.Cost.Case,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Medium Duty Trucks") %>%
      dplyr::distinct()

    NREL_ICE_HDT_costs_clean <- NREL_ICE_HDT_costs %>%
      dplyr::rename( Case = Electric.Range.Case,
                     Technology = Battery.Cost.Case,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Heavy Duty Trucks") %>%
      dplyr::distinct()

    NREL_ICE_Buses_costs_clean <- NREL_ICE_Buses_costs %>%
      dplyr::rename( Case = Electric.Range.Case,
                     Technology = Battery.Cost.Case,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Buses" ) %>%
      dplyr::distinct()
    
    NREL_ICE_LDC_fuel_eff_clean <- NREL_ICE_LDC_fuel_eff %>%
      dplyr::filter(Fuel.Efficiency.Type == "Main Efficiency") %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Fuel.Efficiency.(MPGe)" ) %>%
      dplyr::mutate( Subsector = "Light Duty Cars" ) %>%
      dplyr::select(-Fuel.Efficiency.Type)

    NREL_ICE_LDT_fuel_eff_clean <- NREL_ICE_LDT_fuel_eff %>%
      dplyr::filter(Fuel.Efficiency.Type == "Main Efficiency") %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Fuel.Efficiency.(MPGe)" ) %>%
      dplyr::mutate( Subsector = "Light Duty Trucks") %>%
      dplyr::select(-Fuel.Efficiency.Type)

    NREL_ICE_MDT_fuel_eff_clean <- NREL_ICE_MDT_HDT_fuel_eff %>%
      dplyr::filter(Technology == "Medium-Duty ICEV") %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Fuel.Efficiency.(MPGe)" ) %>%
      dplyr::mutate( Subsector = "Medium Duty Trucks",
                     Technology = "ICEV")

    NREL_ICE_HDT_fuel_eff_clean <- NREL_ICE_MDT_HDT_fuel_eff %>%
      dplyr::filter(Technology == "Heavy-Duty ICEV") %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Fuel.Efficiency.(MPGe)" ) %>%
      dplyr::mutate( Subsector = "Heavy Duty Trucks",
                     Technology = "ICEV")

    NREL_ICE_Buses_fuel_eff_clean <- NREL_ICE_Buses_fuel_eff %>%
      dplyr::filter(Technology == "ICEV Bus") %>%
      dplyr::rename( Case = Case.or.Source,
                     Value = "Fuel.Efficiency.(MPGe)" ) %>%
      dplyr::mutate( Subsector = "Buses",
                     Technology = "ICEV")
    
    NREL_ICE_costs <- bind_rows( NREL_ICE_LDC_costs_clean, NREL_ICE_LDT_costs_clean, NREL_ICE_MDT_costs_clean, NREL_ICE_HDT_costs_clean, NREL_ICE_Buses_costs_clean) %>%
      mutate(variable = "Capital costs (purchase)", unit = "2016$")
    
    NREL_ICE_fuel_eff <- bind_rows(NREL_ICE_LDC_fuel_eff_clean, NREL_ICE_LDT_fuel_eff_clean, NREL_ICE_MDT_fuel_eff_clean, NREL_ICE_HDT_fuel_eff_clean, NREL_ICE_Buses_fuel_eff_clean) %>%
      mutate(variable = "intensity", unit = "MPGe")
    
    NREL_ICE_data <- bind_rows(NREL_ICE_costs, NREL_ICE_fuel_eff)
    
     NREL_clean <- NREL_ICE_data %>%
#     Select desired NREL EFs case and NREL technologies (Vehicles)
        dplyr::filter( Case %in% c(NREL_EFS_ICE_case, AEO_CAFE_case),
                       Technology == "ICEV" ) %>%
        dplyr::select( Subsector, Technology, variable, unit, Year, Value )

#     Bind in Operating costs (maintenance)
      NREL_ICE_main_costs <- NREL_conv_main_costs %>%
        dplyr::mutate( Technology = "ICEV", variable = "Operating costs (maintenance)", unit = "2016$/yr" ) %>%
        dplyr::rename( Value = conv_main_cost ) %>%
        dplyr::left_join( NREL_ICE_years, by = "Subsector" ) %>%
        dplyr::select( -Notes)

      NREL_clean_with_main <- dplyr::bind_rows( NREL_clean, NREL_ICE_main_costs )

#     Map to UCD regions
      NREL_region_mapped <- NREL_clean_with_main %>%
        dplyr::left_join( NREL_region_tech_map, by = c(  "Subsector" ) ) %>%
        dplyr::filter( !( is.na( UCD_region ) ) ) %>%
        dplyr::select( UCD_region,  Subsector, variable, unit, Year, Value )
      
#     For Light Duty Cars, need to replace NREL intensity with AEO CAFE intensity
      # Instead of PATHWAYS fuel efficiencies for Light Duty Cars, use AEO CAFE standards
      AEO_ICE_LDC_fuel_eff_clean <- AEO_CAFE_ICE_fuel_eff %>%
        filter(Source %in% c("AEO 2019","AEO 2016"),
               Type %in% c("On-Road New Car (mpg) mpg",
                           "On-Road New Car (mpg)",
                           "On-Road New Light Truck (mpg) mpg",
                           "On-Road New Light Truck (mpg)")) %>%
        gather(key = "Year", value = "Value", AEO_CAFE_YEARS) %>%
        # Take the 2015 data from AEO 2016
        filter((Source == "AEO 2016" & Year == 2015) | (Source == "AEO 2019")) %>%
        mutate(Type = if_else(Type %in% c("On-Road New Car (mpg) mpg", "On-Road New Car (mpg)") , "Midsize Car","Light Truck and SUV")) %>%
        select(-region, -Note, -Source) %>%
        mutate(Year = as.numeric(Year),
               variable = "intensity",
               Units = "MPGe") %>%
        rename(Subsector = Type,
               unit = Units) %>%
        na.omit() %>%
        # need to lag the standards by 5 years
        mutate(Year = Year + 5)
      
      NREL_AEO_ICE_data <- NREL_region_mapped %>%
        filter(Subsector == "Light Duty Cars", variable == "intensity") %>%
        select(-Subsector, -Year, -Value) %>%
        left_join(AEO_ICE_LDC_fuel_eff_clean, by = c("variable", "unit")) %>%
        distinct()
      
      #Bind in with the rest of the NREL data
      NREL_AEO_region_mapped <- bind_rows(filter(NREL_region_mapped, !(Subsector == "Light Duty Cars" & variable == "intensity")),
                                          NREL_AEO_ICE_data)

      
#     Convert energy intensity from MPGe to MJ/vkm
      NREL_intensity <- NREL_AEO_region_mapped %>%
        dplyr::filter( variable == "intensity" ) %>%
        dplyr::mutate( Value = 1/(Value*(1/Wh_per_gal) * (1/MJ_per_Wh) * km_per_mi), unit = "MJ/vkm")
      
#     Bind energy intensity back in with rest of table
      NREL_en_in_converted <- dplyr::bind_rows( dplyr::filter( NREL_AEO_region_mapped, variable != "intensity" ),
                                                NREL_intensity )
      

#     Convert capital costs from $2016/veh to $2016/veh/yr for Trucks and Buses
      NREL_cap_USD_veh_yr <- NREL_en_in_converted %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks", "Buses"), variable == "Capital costs (purchase)" ) %>%
        dplyr::mutate( Value_2016USD_veh_yr = Value * (FCR_VEH), unit = "2016$/veh/yr" ) %>%
        dplyr::select( -Value ) %>%
        dplyr::rename( Value = Value_2016USD_veh_yr )

#     Bind capital costs for Trucks and Buses back into the rest of the cost table
      NREL_2016USD_veh_yr <- dplyr::bind_rows( filter( NREL_en_in_converted,  !(variable == "Capital costs (purchase)"
                                                                           & Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks", "Buses")) ),
                                               NREL_cap_USD_veh_yr )

      #     For NREL Trucks and Buses, Capital costs (purchase) + Capital costs (infrastructure) + Operating costs (maintenance) = CAPEX and non-fuel OPEX.
      #     Convert from $/veh/yr to $/vkt
      #Now, all variables are in the correct form for UCD
      NREL_TrucksBuses_UCD_variables <- NREL_2016USD_veh_yr %>%
        dplyr::filter( variable != "intensity", Subsector %in% c("Buses", "Heavy Duty Trucks", "Medium Duty Trucks", "Light Duty Trucks") ) %>%
        dplyr::group_by( UCD_region, Subsector, Year ) %>%
        dplyr::summarise( Value = sum( Value ) ) %>%
        dplyr::mutate( variable = "CAPEX and non-fuel OPEX", unit = "2016$/veh" ) %>%
        dplyr::left_join( select(TEDB_vkt_veh_yr, Subsector, vkt_veh_yr), by = "Subsector") %>%
        dplyr::mutate( Value = Value / vkt_veh_yr, unit = "2016$/vkt" ) %>%
        dplyr::select(-vkt_veh_yr)
      
#     Bind Truck/Buses costs back into the rest of the cost table
      NREL_2016USD <- dplyr::bind_rows( filter( NREL_2016USD_veh_yr, !(Subsector %in% c("Buses", "Heavy Duty Trucks", "Medium Duty Trucks", "Light Duty Trucks") & variable != "intensity") ),
                                        NREL_TrucksBuses_UCD_variables )
      
#     Convert to $2005/veh or $2005/vkt for cost variables (all variables except intensity):
      NREL_2005USD <- NREL_2016USD %>%
        dplyr::mutate(Value_2005 = if_else(variable == "intensity", Value, Value * CONV_2016_TO_2005)) %>%
        dplyr::mutate(unit = case_when(unit == "MJ/vkm" ~ "MJ/vkm",
                                       unit == "2016$" ~ "2005$/veh",
                                       unit == "2016$/yr" ~ "2005$/veh/yr",
                                       unit == "2016$/veh/yr" ~ "2005$/veh/yr",
                                       unit == "2016$/veh" ~ "2005$/veh",
                                       unit == "2016$/vkt" ~ "2005$/vkt")) %>%
        dplyr::select( -Value ) %>%
        dplyr::rename( Value = Value_2005 )

#     Map to UCD vehicle types
      NREL_vehicle_types_mapped <- NREL_2005USD %>%
        dplyr::left_join( NREL_veh_map, by = "Subsector" ) %>%
        dplyr::select( UCD_region, Subsector, UCD_sector, mode, size.class, variable,
                       unit, Year, Value ) %>%
        dplyr::filter( !( is.na( UCD_sector ) & is.na( mode ) & is.na( size.class) ) )

#     Transform years to be compatible with UCD, add UCD technology and fuel
##     PAST: 2005 values should be non-linearly extrapolated based on average rate of decrease from 2015-2020 or 2017-2020, going back
#     FUTURE: Between 2050 and 2095, the slope halves every decade

#For now, PAST: 2005 values = 2015 or 2017 values
# TODO: determine how energy intensity should evolve over time

      NREL_complete_years <- NREL_vehicle_types_mapped %>%
        dplyr::mutate( Year = paste0( "X", Year ) ) %>%
        tidyr::spread( Year, Value ) %>%
        #dplyr::mutate(annual_rate = case_when( !is.na(X2015) ~ (((X2020/X2015)^(1/5))-1),
        #                                         !is.na(X2017) ~ (((X2020/X2017)^(1/3))-1) ) ) %>%
        # dplyr::mutate( X2005 = case_when( !is.na(X2015) ~ (X2015/(1 + annual_rate)^10) ,
        #                                   !is.na(X2017) ~ (X2017/(1 - annual_rate)^12) )) %>%
        #For now, PAST: 2005 values = 2015, 2015, 2017, or 2020 values (whichever year is earliest available)
        dplyr::mutate( X2015 = if_else( !is.na(X2015), X2015, 
                                        if_else( !is.na(X2016), X2016,
                                                 if_else( !is.na(X2017), X2017, X2020)))) %>%
        dplyr::mutate( X2005 = X2015) %>%
        dplyr::mutate( X2060 = X2050 + (X2050 - X2040)/(2050-2040)/2 ) %>%
        dplyr::mutate( X2070 = X2060 + (X2060 - X2050)/(2060-2050)/2 ) %>%
        dplyr::mutate( X2080 = X2070 + (X2070 - X2060)/(2070-2060)/2 ) %>%
        dplyr::mutate( X2090 = X2080 + (X2080 - X2070)/(2080-2070)/2 ) %>%
        dplyr::mutate( X2100 = X2090 + (X2090 - X2080)/(2090-2080)/2 ) %>%
      # Add all years which are not present to the table
        dplyr::mutate_at( c('X2010', "X2065", "X2075", "X2085", "X2095"), funs( identity( NA ) ) ) %>%
        dplyr::select( UCD_region, Subsector, UCD_sector, mode, size.class, variable, unit, COMPLETE_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        dplyr::mutate( UCD_technology = "Liquids", UCD_fuel = "Liquids" )

#     Make NREL data long format
      NREL_complete_years_long <- NREL_complete_years %>%
        tidyr::gather( COMPLETE_YEARS_X, key = "Year", value = "Value" )
# ------------------------------------------------------------------------------

# COST DIFFERENTIATION BY SIZE.CLASS

      #Make UCD data long format
      UCD_clean_long <- UCD_clean %>%
        tidyr::gather( UCD_YEARS_X, key = "Year", value = "Value" )

      #Map NREL class types to UCD data
      UCD_vehicle_types_mapped <- UCD_clean_long %>%
        dplyr::left_join( NREL_veh_map, by = c("UCD_sector", "mode", "size.class") ) %>%
        dplyr::filter( ! is.na( Subsector ) ) %>%
        dplyr::select( -Note )

      # # ------------------------------------------------------------------------------

      #1. Light Duty Cars Cost Data
      UCD_LDC_Liq_costs <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Light Duty Cars", UCD_technology == "Liquids",
                      variable %in% c("Capital costs (purchase)",
                                      "Operating costs (maintenance)") )

      #Using USA Liq cost as the base light duty car cost
      UCD_USA_LDC_Liq_costs <- UCD_LDC_Liq_costs %>%
        dplyr::filter( UCD_region == "USA", size.class == "Midsize Car", Year == "X2020") %>%
        dplyr::select(Year, variable, Value)

      #Map representative cars costs to all vehicles to obtain ratio =  size.class cost / representative car cost
      UCD_LDC_Liq_ratio <- UCD_LDC_Liq_costs %>%
        filter(Year == "X2020") %>%
        dplyr::left_join( UCD_USA_LDC_Liq_costs, by = c("variable", "Year"), suffix = c("", ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -Year )

      #Join UCD LDC Liq cost ratios to NREL LDC data
      #Drop rows which have no ratio - these vehicle types do not exist in UCD
      #Multiply NREL cost data by the ratio to obtain cost differentiation by vehicle size.class
      NREL_LDC <- NREL_complete_years_long %>%
        dplyr::filter( Subsector == "Light Duty Cars" ) %>%
        dplyr::left_join( UCD_LDC_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
                                                   "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)
      
      #1.5 Light Duty Cars intensity data
      #a. CARS
      UCD_LDC_Car_Liq_intensity <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Midsize Car", UCD_technology == "Liquids",
                       variable == "intensity" )
      
      #Using USA Midsize car intensity as the base light duty car intensity
      UCD_USA_Car_Liq_intensity <- UCD_LDC_Car_Liq_intensity %>%
        dplyr::filter( UCD_region == "USA", size.class == "Midsize Car", Year == "X2020") %>%
        dplyr::select(Year, variable, Value)
      
      #Map representative cars intensity to all vehicles to obtain ratio =  size.class intensity / representative car intensity
      UCD_LDC_Car_Liq_intensity_ratio <- UCD_LDC_Car_Liq_intensity %>%
        filter(Year == "X2020") %>%
        dplyr::left_join( UCD_USA_Car_Liq_intensity, by = c("variable", "Year"), suffix = c("", ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -Year )
      
      #Join UCD LDC Liq intensity ratios to NREL LDC data (CAFE standards)
      #Drop rows which have no ratio - these vehicle types do not exist in UCD
      #Multiply NREL intensity data by the ratio to obtain intensity differentiation by vehicle size.class
      NREL_LDC_Car_intensity <- NREL_complete_years_long %>%
        dplyr::filter( Subsector == "Midsize Car" ) %>%
        dplyr::left_join(UCD_LDC_Car_Liq_intensity_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
                                                    "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)
      
      
      #b. LIGHT TRUCK AND SUV
      UCD_LDC_LTSUV_Liq_intensity <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Light Truck and SUV", UCD_technology == "Liquids",
                      variable == "intensity" )
      
      #Using USA Light Truck and SUV intensity as the base light duty car intensity
      UCD_USA_LTSUV_Liq_intensity <- UCD_LDC_LTSUV_Liq_intensity %>%
        dplyr::filter( UCD_region == "USA", size.class == "Light Truck and SUV", Year == "X2020") %>%
        dplyr::select(Year, variable, Value)
      
      #Map representative cars intensity to all vehicles to obtain ratio =  size.class intensity / representative car intensity
      UCD_LDC_LTSUV_Liq_intensity_ratio <- UCD_LDC_LTSUV_Liq_intensity %>%
        filter(Year == "X2020") %>%
        dplyr::left_join( UCD_USA_LTSUV_Liq_intensity, by = c("variable", "Year"), suffix = c("", ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -Year )
      
      #Join UCD LDC Liq intensity ratios to NREL LDC data (CAFE standards)
      #Drop rows which have no ratio - these vehicle types do not exist in UCD
      #Multiply NREL intensity data by the ratio to obtain intensity differentiation by vehicle size.class
      NREL_LDC_LTSUV_intensity <- NREL_complete_years_long %>%
        dplyr::filter( Subsector == "Light Truck and SUV" ) %>%
        dplyr::left_join(UCD_LDC_LTSUV_Liq_intensity_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
                                                                 "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)
      

      # # ------------------------------------------------------------------------------
      #2. Buses Cost Data

      UCD_Bus_Liq_costs <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Buses", UCD_technology == "Liquids",
                       variable %in% c("CAPEX and non-fuel OPEX",
                                       "intensity"), Year == "X2020" )

      #Using USA liquids bus cost as the base bus cost
      UCD_USA_Bus_Liq_costs <- UCD_Bus_Liq_costs %>%
        dplyr::filter( UCD_region == "USA" ) %>%
        dplyr::select( variable, Value )
      
      #Map USA liquids costs to all buses to obtain ratio = region/size.class bus / base bus cost
      UCD_Bus_Liq_ratio <- UCD_Bus_Liq_costs %>%
        dplyr::left_join( UCD_USA_Bus_Liq_costs, by = c("variable"), suffix = c("" , ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -UCD_technology, -UCD_fuel, -Year )
      
      #Join UCD Bus Liq cost ratios to NREL Buses Data
      NREL_Buses <- NREL_complete_years_long %>%
        dplyr::filter( Subsector == "Buses", variable %in% c("CAPEX and non-fuel OPEX", "intensity") ) %>%
        dplyr::left_join( UCD_Bus_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                                    "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)

      # # ------------------------------------------------------------------------------
      
      #3. Light, Medium, and Heavy Trucks Cost Data
      #Get NREL Truck data
      NREL_Trucks_costs <- NREL_complete_years_long %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks"),
                       variable == "CAPEX and non-fuel OPEX")
      
      NREL_Trucks_intensity <- NREL_complete_years_long %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks"),
                       variable == "intensity")
      
      #Obtain the median weight of every truck size.class
      UCD_Truck_size_class <- UCD_Truck_size_class_map %>%
        dplyr::mutate( median = (lower_bound + upper_bound)/2 ) %>%
        dplyr::select( size.class, median )
      
      fun_NREL_Truck_interp = function (df, variable) {
        
        #Create a table for interpolated weight_class
        NREL_Truck_weight_classes <- tibble( weight_class =  seq(0,32,0.05),
                                             variable = variable )
        
        NREL_Truck_blank_weight_class <- NREL_Truck_weight_classes %>%
          dplyr::mutate_at( COMPLETE_YEARS_X, funs( identity( NA) ) ) %>%
          tidyr::gather( COMPLETE_YEARS_X, key = "Year", value = "Value") %>%
          tidyr::spread( weight_class , Value )
        
        #Filtering costs for each NREL weight class from USA data. For now, they are the same in all regions.
        NREL_LDT_costs <- df %>%
          dplyr::filter( UCD_region == "USA", variable == variable,
                         Subsector == "Light Duty Trucks" ) %>%
          dplyr::select( Year, Value ) %>%
          dplyr::distinct( Year, Value ) %>%
          dplyr::rename( Value_2.25 = Value )
        
        NREL_MDT_costs <- df %>%
          dplyr::filter( UCD_region == "USA", variable == variable,
                         Subsector == "Medium Duty Trucks" ) %>%
          dplyr::select( Year, Value ) %>%
          dplyr::distinct( Year, Value ) %>%
          dplyr::rename( Value_8.25 = Value )
        
        NREL_HDT_costs <- df %>%
          dplyr::filter( UCD_region == "USA", variable == variable,
                         Subsector == "Heavy Duty Trucks" ) %>%
          dplyr::select( Year, Value ) %>%
          dplyr::distinct( Year, Value ) %>%
          dplyr::rename( Value_22 = Value )
        
        # Interpolate between the NREL median weight class values to get a full table
        NREL_Truck_interp_weight_class <- NREL_Truck_blank_weight_class %>%
          dplyr::left_join( NREL_LDT_costs, by = "Year" ) %>%
          dplyr::left_join( NREL_MDT_costs, by = "Year" ) %>%
          dplyr::left_join( NREL_HDT_costs, by = "Year" ) %>%
          dplyr::mutate( "0" = Value_2.25,
                         "2.25" = Value_2.25,
                         "8.25" = Value_8.25,
                         "22" = Value_22,
                         "32" = Value_22) %>%
          dplyr::select( -Value_2.25, -Value_8.25, -Value_22 ) %>%
          interpolate_NAs_new( ) %>%
          tidyr::gather( as.character( seq(0,32,0.05) ), key = "median", value = "Value" )
        
        #Map the NREL median weight class costs to NREL Truck data
        NREL_Trucks_median <- df %>%
          dplyr::filter( variable == variable ) %>%
          dplyr::left_join( UCD_Truck_size_class, by = "size.class" ) %>%
          dplyr::select( -Value ) %>%
          dplyr::mutate( median = as.character( median) ) %>%
          dplyr::left_join( NREL_Truck_interp_weight_class, by = c("median", "Year", "variable") ) %>%
          dplyr::select( -median )
        
        return (NREL_Trucks_median)
      }
      
      NREL_Trucks_median_costs = fun_NREL_Truck_interp(NREL_Trucks_costs, "CAPEX and non-fuel OPEX")
      NREL_Trucks_median_intensity = fun_NREL_Truck_interp(NREL_Trucks_intensity, "intensity")
      
      NREL_Trucks_median <- bind_rows(NREL_Trucks_median_costs, NREL_Trucks_median_intensity)
      
      
      #Map NREL Truck data to UCD Truck data, to only keep the size.classes for each region which already exist in the UCD database
      NREL_Trucks_UCD_size_class <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks" ),
                       variable %in% c("CAPEX and non-fuel OPEX", "intensity") ) %>%
        dplyr::select( -UCD_technology, -UCD_fuel, -unit, -Value, -Year ) %>%
        dplyr::distinct( ) %>%
        dplyr::left_join( NREL_Trucks_median, by = c("UCD_region", "Subsector", "UCD_sector", "mode", "size.class", "variable") ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)
      
      #TRUCKS ARE NOT DIFFERENTIATED BY REGION
      
# # ------------------------------------------------------------------------------
      #Bind Cars, Buses and Trucks together
      NREL_all_Vehicles <- dplyr::bind_rows( NREL_LDC, NREL_LDC_Car_intensity, NREL_LDC_LTSUV_intensity, NREL_Buses, NREL_Trucks_UCD_size_class )

      #Make new capital costs (other) data
      UCD_cap_costs_other <- UCD_clean_long %>%
        filter(variable == "Capital costs (other)")
      
      UCD_cap_costs_purchase <- UCD_clean_long %>%
        filter(variable == "Capital costs (purchase)")
      
      UCD_cap_costs_share <- UCD_cap_costs_purchase %>%
        left_join(UCD_cap_costs_other, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                              "UCD_technology", "UCD_fuel", "unit", "Year"), suffix = c(".p", ".o")) %>%
        # 2W in Africa don't have capital costs (other)?
        na.omit() %>%
        mutate(share = Value.o/Value.p) %>%
        # All years capital costs (other) percent share of capital costs (purchase) are the same. Drop extra columns
        filter(Year == "X2020") %>%
        select(-Value.p, -Value.o, -variable.p, -variable.o, -Year)
      
      NREL_cap_costs_other <- NREL_all_Vehicles %>%
        filter(variable == "Capital costs (purchase)") %>%
        left_join(UCD_cap_costs_share, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "unit")) %>%
        mutate(Value = Value * share,
               variable = "Capital costs (other)") %>%
        select(-share)
      
      #Spread years
      NREL_final <- bind_rows(NREL_all_Vehicles, NREL_cap_costs_other) %>%
        tidyr::spread( Year, Value )

# # ------------------------------------------------------------------------------
#  # V. Write out the final data to a csv

      #Set WD to output folder and define warning for if df has no data
      if( computer_type == "MAC" ){

        setwd( "/Users/yarl762/Desktop/Vehicle_costs/Output" )

      } else {

        setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Output" )

      }


      write.csv( NREL_final, file = "NREL_ICE_data.csv" , row.names=FALSE, na = "" )
