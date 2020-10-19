# ---------------------------------------------------------------------------
# Program Name: NG_LDC_cost_update.R
# Author: Brinda Yarlagadda and Patrick O'Rourke
# Date Last Updated: 11.20.19
# Program Purpose: To update NG, Hybrid Liquids, and FCEV vehicle cost assumptions
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

#   Define constants
    NREL_VEH_LIST <- c( "Light Duty Cars" )
    CONV_2016_TO_2005 <- (100/105.935)*(87.421/100)
    NREL_YEARS <- c( 2015, seq( 2020, 2050, 10 ) )
    NREL_YEARS_X <- paste0( "X", NREL_YEARS )
    UCD_YEARS <-  c( 2005, seq( 2020, 2095, 15 ) )
    UCD_YEARS_X <- paste0( "X", UCD_YEARS )
    COMPLETE_YEARS <- c(seq(2005,2100,5))
    COMPLETE_YEARS_X <- paste0 ( "X", COMPLETE_YEARS)

    FCR_VEH <- 0.1627454 # Fixed charge rate, from A_trn_data
    
    #Multipliers to obtain NG cost data (Source: Mishra et al. UCD Research Report UCD-ITS-RR-13-05 (2013).)
    NG_CAP_COST_PURCHASE_RATIO <- 1.14
    NG_OP_COST_MAIN_RATIO <- 0.95

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
    NREL_ICE_LDC_data <- read.xlsx( xlsxFile = "EFS_70485_figure_data.xlsx", sheet = "4", startRow = 5)

#   Load NREL mapping files
    NREL_var_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Variables" )
    NREL_veh_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Vehicles" )
    NREL_region_tech_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Region_Tech" )
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
    NREL_ICE_LDC_data_clean <- NREL_ICE_LDC_data %>%
      dplyr::rename( Case = EFS.Case.or.Source,
                     Value = "Capital.Cost.(2016$)" ) %>%
      dplyr::mutate( Subsector = "Light Duty Cars" )

    NREL_ICE_data <- bind_rows( NREL_ICE_LDC_data_clean )

      NREL_clean <- NREL_ICE_data %>%
#     Select desired NREL EFs case and NREL technologies (Vehicles)
        dplyr::filter( Case == NREL_EFS_ICE_case,
                       Technology == "ICEV" ) %>%
        dplyr::mutate( unit = "2016$", variable = "Capital costs (purchase)") %>%
        dplyr::select( Subsector, Technology, variable, unit, Year, Value )

#     Bind in Operating costs (maintenance)
      NREL_ICE_main_costs <- NREL_conv_main_costs %>%
        dplyr::mutate( Technology = "ICEV", variable = "Operating costs (maintenance)", unit = "2016$/yr" ) %>%
        dplyr::rename( Value = conv_main_cost ) %>%
        dplyr::left_join( NREL_ICE_years, by = "Subsector" ) %>%
        dplyr::select( -Notes)

      NREL_clean_with_main <- dplyr::bind_rows( NREL_clean, NREL_ICE_main_costs )

      
#     Multiply by UCD percentages to obtain NG cost data
      NREL_NG <- NREL_clean_with_main %>%
        dplyr::mutate( Value = case_when( variable == "Capital costs (purchase)" ~ Value*NG_CAP_COST_PURCHASE_RATIO,
                                         variable == "Operating costs (maintenance)" ~ Value*NG_OP_COST_MAIN_RATIO),
                      Technology = "NG" )
      
#     Map to UCD regions
      NREL_region_mapped <- NREL_NG %>%
        dplyr::left_join( NREL_region_tech_map, by = c(  "Subsector" ) ) %>%
        dplyr::filter( !( is.na( UCD_region ) ) ) %>%
        dplyr::select( UCD_region,  Subsector, variable, unit, Year, Value )

#     Convert to $2005/veh or $2005/vkt for all variables:
      NREL_2005USD <- NREL_region_mapped %>%
        dplyr::mutate(Value_2005 = Value * CONV_2016_TO_2005) %>%
        dplyr::mutate(unit = case_when(unit == "2016$" ~ "2005$/veh",
                                       unit == "2016$/yr" ~ "2005$/veh/yr",
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
#     PAST: 2005 values should be non-linearly extrapolated based on average rate of decrease from 2015-2020 or 2017-2020, going back
#     FUTURE: Between 2050 and 2095, the slope halves every decade
      NREL_UCD_years <- NREL_vehicle_types_mapped %>%
        dplyr::mutate( Year = paste0( "X", Year ) ) %>%
        tidyr::spread( Year, Value ) %>%
        dplyr::mutate(annual_rate = case_when( !is.na(X2015) ~ (((X2020/X2015)^(1/5))-1),
                                                 !is.na(X2017) ~ (((X2020/X2017)^(1/3))-1) ) ) %>%
        dplyr::mutate( X2005 = case_when( !is.na(X2015) ~ (X2015/(1 + annual_rate)^10) ,
                                          !is.na(X2017) ~ (X2017/(1 - annual_rate)^12) )) %>%
        dplyr::mutate( X2060 = X2050 + (X2050 - X2040)/(2050-2040)/2 ) %>%
        dplyr::mutate( X2070 = X2060 + (X2060 - X2050)/(2060-2050)/2 ) %>%
        dplyr::mutate( X2080 = X2070 + (X2070 - X2060)/(2070-2060)/2 ) %>%
        dplyr::mutate( X2090 = X2080 + (X2080 - X2070)/(2080-2070)/2 ) %>%
        dplyr::mutate( X2100 = X2090 + (X2090 - X2080)/(2090-2080)/2 ) %>%
        dplyr::mutate_at( c("X2010", "X2035", "X2045", "X2055", "X2065", "X2075", "X2085", "X2095"), funs( identity( NA ) ) ) %>%
        dplyr::select(UCD_region, Subsector, UCD_sector, mode, size.class, variable, unit,
                      COMPLETE_YEARS_X) %>%
        interpolate_NAs_new( ) %>%
        dplyr::mutate( UCD_technology = "NG", UCD_fuel = "Natural Gas" )

#     Make NREL data long format
      NREL_UCD_years_long <- NREL_UCD_years %>%
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
      UCD_LDC_NG_costs <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Light Duty Cars", UCD_technology == "NG",
                      variable %in% c("Capital costs (purchase)",
                                      "Operating costs (maintenance)"), Year == "X2005" )

      #Using USA NG cost as the base light duty car cost
      UCD_USA_LDC_NG_costs <- UCD_LDC_NG_costs %>%
        dplyr::filter( UCD_region == "USA", size.class == "Midsize Car") %>%
        dplyr::select( variable, Value)

      #Map representative cars costs to all vehicles to obtain ratio =  size.class cost / representative car cost
      UCD_LDC_NG_ratio <- UCD_LDC_NG_costs %>%
        dplyr::left_join( UCD_USA_LDC_NG_costs, by = c("variable"), suffix = c("", ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -Year )

      #Join UCD LDC NG cost ratios to NREL LDC data
      #Drop rows which have no ratio - these vehicle types do not exist in UCD
      #Multiply NREL cost data by the ratio to obtain cost differentiation by vehicle size.class
      NREL_LDC <- NREL_UCD_years_long %>%
        dplyr::filter( Subsector == "Light Duty Cars" ) %>%
        dplyr::left_join( UCD_LDC_NG_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
                                                   "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)

     
# # ------------------------------------------------------------------------------
      #Bind Cars, Buses and Trucks together
      NREL_all_Vehicles <- dplyr::bind_rows( NREL_LDC )
      
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


      write.csv( NREL_final, file = "NREL_NG_data.csv" , row.names=FALSE, na = "" )
