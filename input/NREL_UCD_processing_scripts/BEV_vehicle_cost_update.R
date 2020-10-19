# ---------------------------------------------------------------------------
# Program Name: BEV_vehicle_cost_update.R
# Author: Brinda Yarlagadda and Patrick O'Rourke
# Date Last Updated: 11.20.19
# Program Purpose: To update BEV vehicle cost assumptions
# Input Files: UCD_transportation_database.csv,
#              EFS_Technology_Data.xlsx,
#              NREL_to_UCD_mapping.xlsx,
#              Transportation_Energy_Data_Book_Ed37_2019.xlsx
# Output Files: NREL_BEV_data.csv
# Notes:
# TODO:

# ------------------------------------------------------------------------------

# 0 computer Options
#   Pick other options
    user <- "Brinda"         # Options only defined for "Pat" and "Brinda"
    computer_type <- "PC" # Only needed if user set to "Pat" - Computer type (MAC or PC)

# ------------------------------------------------------------------------------

# 0.5 Pick options and define constants and functions

#   Pick options
    NREL_EFS_case <- "Rapid Advancement"
    BEV_TRUCK_LF_PENALTY_PHASEOUT <- TRUE

#   Define constants
    NREL_VEH_subs_LIST <- c( "Buses", "Medium Duty Trucks", "Heavy Duty Trucks",
                        "Light Duty Cars", "Light Duty Trucks" )
    NREL_VEH_tech_LIST <- c("BEV", "BEV 100", "BEV 200", "BEV 300")
    CONV_2016_TO_2005 <- (100/105.935)*(87.421/100)
    NREL_YEARS <- c( 2015, seq( 2020, 2050, 10 ) )
    NREL_YEARS_X <- paste0( "X", NREL_YEARS )
    UCD_YEARS <-  c( 2005, seq( 2020, 2095, 15 ) )
    UCD_YEARS_X <- paste0( "X", UCD_YEARS )
    COMPLETE_YEARS <- c(seq(2005,2100,5))
    COMPLETE_YEARS_X <- paste0 ( "X", COMPLETE_YEARS)

    FCR_VEH <- 0.1627454 # Fixed charge rate, from A_trn_data

   Wh_per_gal <- 33700 #Fuel economy conversion factor from (NREL EFS, Source: DOE 2017a)
   MJ_per_Wh <- 0.0036
   km_per_mi <- 1.60934

#    THIEL_BEV_ICE_RATIO  <- 35055/19853 #Ratio of cost of BEV/ICE vehicle, from C. Thiel et. al. (2010). Applied to 2005 and 2010 cars.

#   Define functions for interpolating between years
    all.na_new  <- function(x) return( all( is.na(x) ) )

      interpolate_NAs_new <- function(df) {

        if( !is.data.frame( df ) ) {

          warning( "interpolate_NAs expects a data frame; attempting to convert" )

          df <- as.data.frame( df )

        }

#     Convert columns that are all NA to numeric
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

#     USEFUL FUNCTIONS
      #Function "is not an element of" (opposite of %in%)
      '%!in%' <- function( x, y ) !( '%in%'( x, y ) )
      
      #add new columns with NAs for columns that don't exist from a list
      fncols <- function(data, cname) {
        add_cname <-cname[cname%!in%names(data)]
        
       if(length(add_cname)!=0){
         cbind(data, setNames( lapply(add_cname, function(x) x=NA), add_cname) ) 
       }
      }
      

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
      
# ------------------------------------------------------------------------------

# #   Load UCD transportation data
#     UCD <- read.csv( "UCD_transportation_database.csv",
#                      header = TRUE )

#   Load UCD transportation data
    UCD <- read.csv( "UCD_trn_data_CORE_ref.csv", skip=7 )

#   Load NREL transportation data
    NREL_data <- read.xlsx( xlsxFile = "EFS_Technology_Data.xlsx", sheet = "EFS Data" )

#   Load NREL mapping files
    NREL_var_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Variables" )
    NREL_veh_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Vehicles" )
    NREL_region_tech_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Region_Tech" )
    UCD_Truck_size_class_map <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Truck_size_class")
    NREL_conv_main_costs <- read.xlsx( xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "Conv_main_costs")
    TEDB_vkt_veh_yr <- read.xlsx( xlsxFile = "Transportation_Energy_Data_Book_Ed37_2019.xlsx", sheet = "vkt_veh_yr")
    
    BEV_range_path_region <- read.xlsx(xlsxFile = "NREL_to_UCD_mapping.xlsx", sheet = "BEV_range_path_region")

#   Load NREL ICE data (used to correct 2005 and 2010 BEV data)
    NREL_ICE_data <- read.csv("../Output/NREL_ICE_data.csv")
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
      NREL_clean <- NREL_data %>%

#     Select desired NREL EFs case and NREL technologies (Vehicles)
        dplyr::filter( Sector == "Transportation",
                       EFS.Case == NREL_EFS_case,
                       Subsector %in%  NREL_VEH_subs_LIST,
                       Technology %in% NREL_VEH_tech_LIST) %>%
        dplyr::rename( unit = Units ) %>%
        dplyr::select( Subsector, Technology, Metric, unit, Year, Value )
      

      
# ------------------------------------------------------------------------------
# Costs for LDV (Cars and Trucks) by region (differentiated ranges by region and year)      
      
#     TODO: figure out how this process could be generalized to interpolate between any years
      
#     Create an interpolation table of NREL costs by BEV LDVs range
      NREL_interpolated_range_costs <-NREL_clean %>%
        filter(Technology %in% c("BEV 100", "BEV 200", "BEV 300")) %>%
        mutate(Range = as.numeric(gsub("^BEV ", "", Technology))) %>%
        select(-Technology) %>%
        spread(Range, Value) %>%
        fncols(c(seq(100,300,25))) %>% #add columns at 25-mi increments of BEV range
        select(sort(names(.))) %>%
        select(c(Subsector, Metric, unit, Year), everything()) %>%
        interpolate_NAs_new() %>%
        gather(key = "Range", value = "Value", as.character(c(seq(100,300,25)))) %>%
        mutate(Range = as.numeric(Range))

#     Table of ranges for each region in each year    
      BEV_range_NREL_years_region <- BEV_range_path_region %>%
        fncols(c(NREL_YEARS_X)) %>%
        select(sort(names(.))) %>%
        select(UCD_region, everything()) %>%
        interpolate_NAs_new() %>%
        mutate(X2040 = X2030, X2050 = X2030) %>%
        gather(key = "Year", value = "Range", NREL_YEARS_X) %>%
        mutate(Year = as.numeric(gsub("^X", "", Year))) 
        
#     Map costs into BEV_range_NREL_years_region
      LDV_BEV_range_costs_region <- BEV_range_NREL_years_region %>%
        left_join(NREL_interpolated_range_costs, by = c("Year", "Range")) %>%
        dplyr::select( UCD_region,  Subsector, Metric, unit, Year, Value )
      
# ------------------------------------------------------------------------------
      
# #     Map to UCD regions and select NREL technology BEV types (BEV vs. BEV 100 vs. BEV 200...)
#       NREL_region_mapped <- NREL_clean %>%
#         dplyr::left_join( NREL_region_tech_map, by = c(  "Subsector", "Technology" ) ) %>%
#         dplyr::filter( !( is.na( UCD_region ) ) ) %>%
#         dplyr::select( UCD_region,  Subsector, Metric, unit, Year, Value )
      
#     Combine LDV table with rest of NREL vehicles
      NREL_region_mapped <- NREL_clean %>%
        dplyr::left_join( NREL_region_tech_map, by = c(  "Subsector", "Technology" ) ) %>%
        dplyr::filter( !( is.na( UCD_region ) ), Technology %in% c("BEV") ) %>% #retain just Buses, Medium Duty Trucks, Heavy Duty Trucks
        dplyr::select( UCD_region,  Subsector, Metric, unit, Year, Value ) %>%
      # Map in Light Duty Cars and Trucks with range costs differentiated by region and year
        dplyr::bind_rows(LDV_BEV_range_costs_region)

#     Map to UCD variables
      NREL_variables_mapped <- NREL_region_mapped %>%
        dplyr::left_join( NREL_var_map, by = "Metric" ) %>%
        dplyr::filter( !( is.na( variable ) ) ) %>%
        dplyr::select( UCD_region, Subsector, variable, unit, Year, Value )

#     Convert energy intensity from MPGe to MJ/vkm
      NREL_intensity <- NREL_variables_mapped %>%
        dplyr::filter( variable == "intensity" ) %>%
        dplyr::mutate( Value = 1/(Value*(1/Wh_per_gal) * (1/MJ_per_Wh) * km_per_mi), unit = "MJ/vkm")

#     Bind energy intensity back in with rest of table
      NREL_en_in_converted <- dplyr::bind_rows( dplyr::filter( NREL_variables_mapped, variable != "intensity" ),
                                                NREL_intensity )

#     Convert Operating costs (maintenance) from ratio to $2016/veh/yr for all vehicles
      NREL_OM_USD_veh_year <- NREL_en_in_converted %>%
        dplyr::filter( variable == "Operating costs (maintenance)" ) %>%
        dplyr::left_join( NREL_conv_main_costs, by = "Subsector" ) %>%
        dplyr::mutate( Value_2016USD_veh = Value * conv_main_cost, unit = "2016$/veh/yr" ) %>%
        dplyr::select( -Value, -conv_main_cost, -Notes ) %>%
        dplyr::rename( Value = Value_2016USD_veh )
      
#     Bind Operating costs (maintenance) back into the rest of the cost table
      NREL_OM_converted <- dplyr::bind_rows( dplyr::filter( NREL_en_in_converted, variable != "Operating costs (maintenance)" ),
                                            dplyr::filter( NREL_OM_USD_veh_year))
      

#     Convert capital costs from $2016/veh to $2016/veh/yr for Trucks and Buses
      NREL_cap_USD_veh_yr <- NREL_OM_converted %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks", "Buses"), variable %in% c("Capital costs (purchase)",
                                                                                                                                  "Capital costs (infrastructure)")) %>%
        dplyr::mutate( Value_2016USD_veh_yr = Value * (FCR_VEH), unit = "2016$/veh/yr" ) %>%
        dplyr::select( -Value ) %>%
        dplyr::rename( Value = Value_2016USD_veh_yr )
      
#     Bind capital costs back into the rest of the cost table
      NREL_2016USD_veh_yr <- dplyr::bind_rows( filter( NREL_OM_converted, 
                                                      !(Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks", "Buses") &
                                                        variable %in% c("Capital costs (purchase)","Capital costs (infrastructure)")) ),
                                             NREL_cap_USD_veh_yr)


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
#     PAST: 2005 values should be non-linearly extrapolated based on average rate of decrease from 2015-2020, going back
#     FUTURE: Between 2050 and 2095, the slope halves every decade
      NREL_UCD_years <- NREL_vehicle_types_mapped %>%
        dplyr::mutate( Year = paste0( "X", Year ) ) %>%
        tidyr::spread( Year, Value ) %>%
        dplyr::mutate(annual_rate = (((X2020/X2015)^(1/5))-1) ) %>%
        dplyr::mutate( X2005 = (X2015/(1 + annual_rate)^10) ) %>%
        dplyr::mutate( X2060 = X2050 + (X2050 - X2040)/(2050-2040)/2 ) %>%
        dplyr::mutate( X2070 = X2060 + (X2060 - X2050)/(2060-2050)/2 ) %>%
        dplyr::mutate( X2080 = X2070 + (X2070 - X2060)/(2070-2060)/2 ) %>%
        dplyr::mutate( X2090 = X2080 + (X2080 - X2070)/(2080-2070)/2 ) %>%
        dplyr::mutate( X2100 = X2090 + (X2090 - X2080)/(2090-2080)/2 ) %>%
      # Add all years which are not present to the table
        dplyr::mutate_at( c('X2010', "X2025", "X2035", "X2045", "X2055", "X2065", "X2075", "X2085", "X2095"), funs( identity( NA ) ) ) %>%
        dplyr::select( UCD_region, Subsector, UCD_sector, mode, size.class, variable, unit, COMPLETE_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        dplyr::mutate( UCD_technology = "BEV", UCD_fuel = "Electricity" )

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
      UCD_LDC_BEV_costs <- UCD_vehicle_types_mapped %>%
        dplyr::filter( Subsector == "Light Duty Cars", UCD_technology == "BEV",
                      variable %in% c("Capital costs (infrastructure)",
                                      "Capital costs (purchase)",
                                      "Operating costs (maintenance)",
                                      "intensity") )

      #Using 2020 ratio as fixed ratio so that costs in some regions do not inadvertently go up over time
      #Using USA BEV Midsize car cost as the base light duty car cost
      UCD_USA_LDC_BEV_costs <- UCD_LDC_BEV_costs %>%
        dplyr::filter( UCD_region == "USA", size.class == "Midsize Car", Year == "X2020") %>%
        dplyr::select(variable, Value)

      #Map representative cars costs to all vehicles to obtain ratio =  size.class cost / representative car cost
      UCD_LDC_BEV_ratio <- UCD_LDC_BEV_costs %>%
        filter(Year == "X2020") %>%
        dplyr::left_join( UCD_USA_LDC_BEV_costs, by = c("variable"), suffix = c("", ".USA") ) %>%
        dplyr::mutate( ratio = Value / Value.USA ) %>%
        dplyr::select( -unit, -Value, -Value.USA, -Year)

      #Join UCD LDC BEV cost ratios to NREL LDC data
      #Drop rows which have no ratio - these vehicle types do not exist in UCD
      #Multiply NREL cost data by the ratio to obtain cost differentiation by vehicle size.class
      NREL_LDC <- NREL_UCD_years_long %>%
        dplyr::filter( Subsector == "Light Duty Cars" ) %>%
        dplyr::left_join( UCD_LDC_BEV_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
                                                   "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)

##CAUSES 2005 BEV to be cheaper than 2020 BEV, NOT DOING
      # #For light duty cars, change the 2005 BEV vehicle costs to be a ratio of the corresponding ICE vehicle costs. Ratio from Thiel et. al. (2010)
      # NREL_LDC_ratio_adj <- NREL_LDC %>%
      #   dplyr::filter( Year == "X2005", variable == "Capital costs (purchase)") %>%
      #   dplyr::left_join( dplyr::select(NREL_ICE_data, -c(UCD_technology, UCD_fuel, X2020, X2035, X2050, X2065, X2080, X2095)) ,
      #                     by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit")) %>%
      #   dplyr::mutate( Value = X2005 * THIEL_BEV_ICE_RATIO) %>%
      #   dplyr::select(-X2005)
      #
      # #Bind back with the rest of light duty cars
      # NREL_LDC_2005_adj <- bind_rows(filter(NREL_LDC, !(Year == "X2005" & variable == "Capital costs (purchase)")),
      #                                NREL_LDC_ratio_adj)


      # # ------------------------------------------------------------------------------
      #2. Buses Cost Data
      #need to get ratios from some other fuel, because BEV buses do not exist. For now, choosing "Liquids"
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
      NREL_Buses <- NREL_UCD_years_long %>%
        dplyr::filter( Subsector == "Buses", variable %in% c("CAPEX and non-fuel OPEX", "intensity") ) %>%
        dplyr::left_join( UCD_Bus_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class",
                                                    "variable", "Subsector"), suffix = c(".NREL", ".UCD") ) %>%
        dplyr::filter( ! is.na( ratio ) ) %>%
        dplyr::mutate( Value = Value * ratio ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)


      # # ------------------------------------------------------------------------------

      #3. Light, Medium, and Heavy Trucks Cost Data
      #Get NREL Truck data
      NREL_Trucks_costs <- NREL_UCD_years_long %>%
        dplyr::filter( Subsector %in% c("Light Duty Trucks", "Medium Duty Trucks", "Heavy Duty Trucks"),
                       variable == "CAPEX and non-fuel OPEX")
      
      NREL_Trucks_intensity <- NREL_UCD_years_long %>%
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
      NREL_all_Vehicles <- dplyr::bind_rows( NREL_LDC, NREL_Buses, NREL_Trucks_UCD_size_class)

# # ------------------------------------------------------------------------------
      #ADDITIONAL VARIABLES

      #Make new variable for energy, Value is 0 and only specified in 2005
      BEV_energy <- NREL_all_Vehicles %>%
        dplyr::select( -variable, -unit, -Year, -Value ) %>%
        dplyr::distinct( ) %>%
        dplyr::mutate( variable = "energy",
                      unit = "PJ/yr",
                      Year = "X2005",
                      Value = 0 )
      
      #TODO: Make new variables for BEV bus subsidy


      #Copy original UCD database liquid vehicle load factors for BEV LDV_4W and Buses
      BEV_LF_LDVBus <- UCD_clean_long %>%
        dplyr::filter( mode %in% c("Bus", "LDV_4W"), UCD_technology == "Liquids", variable == "load factor" ) %>%
        dplyr::mutate(UCD_technology = "BEV", UCD_fuel = "Electricity") %>%
        tidyr::spread( Year, Value ) %>%
        # Add all years which are not present to the table
        dplyr::mutate_at( c('X2010', "X2015", "X2025", "X2030", "X2040", "X2045", "X2055", "X2060", "X2070", "X2075", "X2085", "X2090", "X2100"), funs( identity( NA ) ) ) %>%
        # Copy 2095 values to 2100
        dplyr::mutate(X2100 = X2095) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, COMPLETE_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        tidyr::gather(Year, Value, COMPLETE_YEARS_X)
      
      #decrease original UCD database liquid vehicle load factors by 20% for BEV Trucks
      #TODO: revisit battery assumptions
      BEV_LF_Trucks <- UCD_clean_long %>%
        dplyr::filter( mode %in% c("Truck"), UCD_technology == "Liquids", variable == "load factor" ) %>%
        tidyr::spread( Year, Value ) %>%
        # Add all years which are not present to the table
        dplyr::mutate_at( c('X2010', "X2015", "X2025", "X2030", "X2040", "X2045", "X2055", "X2060", "X2070", "X2075", "X2085", "X2090", "X2100"), funs( identity( NA ) ) ) %>%
        # Copy 2095 values to 2100
        dplyr::mutate(X2100 = X2095) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, COMPLETE_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        tidyr::gather(Year, Value, COMPLETE_YEARS_X) %>%
        dplyr::mutate(UCD_technology = "BEV", UCD_fuel = "Electricity",
                      Value = Value * 0.8)
      
      #if implementing the BEV truck load factor penalty phaseout
      if (BEV_TRUCK_LF_PENALTY_PHASEOUT) {
        BEV_LF_Trucks <- UCD_clean_long %>%
        dplyr::filter( mode %in% c("Truck"), UCD_technology == "Liquids", variable == "load factor" ) %>%
        # put in a 20% load penalty in the initial years (up to 2020)
        dplyr::mutate(UCD_technology = "BEV", UCD_fuel = "Electricity",
                      Value = if_else(Year %in% c("X2005", "X2020"), Value * 0.8, Value)) %>%
        tidyr::spread( Year, Value ) %>%
        # Copy 2095 values to 2100
        dplyr::mutate(X2100 = X2095) %>% 
        # Add all years which are not present to the table, and years between 2020 and 2050
        dplyr::mutate_at( c('X2010', "X2015", "X2025", "X2030", "X2035", "X2040", "X2045", "X2055", "X2060", "X2070", "X2075", "X2085", "X2090"), funs( identity( NA ) ) ) %>%
        dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, COMPLETE_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        tidyr::gather(Year, Value, COMPLETE_YEARS_X)
          
      }
      
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
      NREL_final <- dplyr::bind_rows(NREL_all_Vehicles, NREL_cap_costs_other, BEV_energy, BEV_LF_LDVBus, BEV_LF_Trucks) %>%
        tidyr::spread( Year, Value )
      


# # ------------------------------------------------------------------------------
#  # V. Write out the final data to a csv

    #Set WD to output folder and define warning for if df has no data
      if( computer_type == "MAC" ){

        setwd( "/Users/yarl762/Desktop/Vehicle_costs/Output" )

      } else {

        setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Output" )

      }


      write.csv( NREL_final, file = "NREL_BEV_data.csv" , row.names=FALSE, na = "" )
