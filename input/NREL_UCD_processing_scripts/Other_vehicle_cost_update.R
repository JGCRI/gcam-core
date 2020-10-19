# ---------------------------------------------------------------------------
# Program Name: Other_vehicle_cost_update.R
# Author: Brinda Yarlagadda and Patrick O'Rourke
# Date Last Updated: 12.6.19
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
    
    UCD_YEARS <-  c( 2005, seq( 2020, 2095, 15 ) )
    UCD_YEARS_X <- paste0( "X", UCD_YEARS )
    COMPLETE_YEARS <- c(seq(2005,2100,5))
    COMPLETE_YEARS_X <- paste0 ( "X", COMPLETE_YEARS)
    
    NEMS_YEARS <-  c(paste0(c(seq(2015,2050,1))))
    
    #Natural gas heavy duty vehicle cost in $2005/vkt, from Tong. et. al. (2019)
    LNG_US_HDT_INF_2005USD_VKT <- 0.041184601
    CNG_US_HDT_INF_2005USD_VKT <- 0.055187365


    
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

    #   Load NREL ICE and BEV data
    NREL_ICE_data <- read.csv("../Output/NREL_ICE_data.csv")
    NREL_BEV_data <- read.csv("../Output/NREL_BEV_data.csv")
    
    NEMS_data <- read.xlsx( xlsxFile = "NEMS_costs.xlsx", sheet = "Sheet1", startRow = 1)
    
    
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
    
    UCD_clean_complete_years <- UCD_clean %>%
      # Copy 2095 values to 2100
      dplyr::mutate(X2100 = X2095) %>%
      # Add the years not present in UCD years
      dplyr::mutate_at( c('X2010', "X2015","X2025", "X2030", "X2040", "X2045", "X2055", "X2060", "X2070", "X2075", "X2085", "X2090"), funs( identity( NA ) ) ) %>%
      dplyr::select(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit,
                      COMPLETE_YEARS_X) %>%
      interpolate_NAs_new( )
    
    #Make data long format
    #old data (every 15 years)
    UCD_clean_long <- UCD_clean %>%
      tidyr::gather( UCD_YEARS_X, key = "Year", value = "Value" )
    
    #new data (every 5 years)
    UCD_clean_complete_years_long <- UCD_clean_complete_years %>%
      tidyr::gather( COMPLETE_YEARS_X, key = "Year", value = "Value" )
  
    NREL_ICE_long <- NREL_ICE_data %>%
      tidyr::gather( COMPLETE_YEARS_X, key = "Year", value = "Value" )
    
    NREL_BEV_long <- NREL_BEV_data %>%
      tidyr::gather( COMPLETE_YEARS_X, key = "Year", value = "Value" )
  
# ------------------------------------------------------------------------------  

    # III. Initial processing of NEMS data (to get FCEV to ICE ratio)
    
    #Make the extra UCD years not included in NEMS
    NEMS_data_years_extended <- NEMS_data
    
    NEMS_Liq <- NEMS_data_years_extended %>%
      tidyr::gather(key = "Year", value = "Value", NEMS_YEARS) %>%
      dplyr::filter(Vehicle == "Conventional Gasoline")
    
    NEMS_ratio <- NEMS_data_years_extended %>%
      tidyr::gather(key = "Year", value = "Value", NEMS_YEARS) %>%
      dplyr::filter(Vehicle == "Fuel Cell Hydrogen",
                    Year %in% COMPLETE_YEARS) %>%
      left_join(NEMS_Liq, by = c("Case", "Year"), suffix = c(".FCEV", ".Liq")) %>%
      mutate(ratio = Value.FCEV/Value.Liq,
             Year = paste0("X", Year, "")) %>%
      select(Year, ratio) %>%
      tidyr::spread(Year, ratio) %>%
      #add in 2005 and 2100 so that we can interpolate
      dplyr::mutate(X2005 = X2015,
                    X2100 = X2050) %>%
      #Add in the remaining missing years
      dplyr::mutate_at( c('X2010', "X2055", "X2060", "X2065", "X2070", "X2075", "X2080", "X2085", "X2090", "X2095"), funs( identity( NA ) ) ) %>%
      dplyr::select(COMPLETE_YEARS_X) %>%
      interpolate_NAs_new() %>%
      tidyr::gather(key = "Year", value = "ratio", COMPLETE_YEARS_X)
      
    # Apply to UCD classes
    NEMS_UCD_classes_ratio <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "Liquids", variable %in% c("Capital costs (purchase)") ) %>%
      select(-unit, -Value) %>%
      left_join(NEMS_ratio, by = c("Year")) %>%
      select(-UCD_technology, -UCD_fuel)
    
# ------------------------------------------------------------------------------      
    # IV. Calculation of new data
    
#  #Calculate new costs and intensities for LDCs Hybrid Liquids, based on ratios of fuel:liquid ratio in original UCD database
    UCD_LDC_Liq <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "Liquids", variable %in% c("Capital costs (purchase)", "Operating costs (maintenance)", "intensity") )
    
    UCD_LDC_Hybrid_Liq <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "Hybrid Liquids", variable %in% c("Capital costs (purchase)", "Operating costs (maintenance)", "intensity") )
    
    UCD_LDC_Hybrid_Liq_Liq_ratio <- UCD_LDC_Hybrid_Liq %>%
      dplyr::left_join( UCD_LDC_Liq, by = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_fuel", "variable", "unit", "Year"), 
                        suffix = c(".HL", ".L")) %>%
      dplyr::mutate(ratio = Value.HL/Value.L) %>%
      dplyr::select(UCD_region, UCD_sector, mode, size.class, variable, unit, Year, ratio)
    
    # #NEW Hybrid Liquids data
    NREL_Hybrid_Liq <- NREL_ICE_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "Liquids", variable %in% c("Capital costs (purchase)", "Operating costs (maintenance)", "intensity") ) %>%
      dplyr::left_join(UCD_LDC_Hybrid_Liq_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year")) %>%
      dplyr::mutate(Value = Value * ratio, UCD_technology = "Hybrid Liquids") %>%
      dplyr::select(-ratio)
    
#  #Calculate new purchase costs for LDCs FCEV, based on ratios of Liquids:FCEV ratio in NEMS database
    # #NEW FCEV purchase data
    NREL_NEMS_FCEV_purchase <- NREL_ICE_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "Liquids", variable %in% c("Capital costs (purchase)") ) %>%
      dplyr::left_join(NEMS_UCD_classes_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "Year")) %>%
      dplyr::mutate(Value = Value * ratio, UCD_technology = "FCEV", "UCD_fuel" = "Hydrogen") %>%
      dplyr::select(-ratio)
  
#   #Calculate new operations, infrastructure costs, intensity based on ratios of BEV:FCEV ratio in original UCD database
    UCD_LDC_BEV  <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "BEV", variable %in% c("Capital costs (infrastructure)", "Operating costs (maintenance)", "intensity") )
    
    UCD_LDC_FCEV <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "FCEV", variable %in% c("Capital costs (infrastructure)", "Operating costs (maintenance)", "intensity") )
    
    UCD_LDC_FCEV_BEV_ratio <- UCD_LDC_FCEV %>%
      dplyr::left_join( UCD_LDC_BEV, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year"), 
                        suffix = c(".F", ".B")) %>%
      dplyr::mutate(ratio = Value.F/Value.B) %>%
      dplyr::select(UCD_region, UCD_sector, mode, size.class, variable, unit, Year, ratio)
    
    # #NEW FCEV operations, infrastructure cost and intensity data
    NREL_FCEV_op_inf_int <- NREL_BEV_long %>%
      dplyr::filter( mode == "LDV_4W", UCD_technology == "BEV", variable %in% c("Capital costs (infrastructure)", "Operating costs (maintenance)", "intensity") ) %>%
      dplyr::left_join(UCD_LDC_FCEV_BEV_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year")) %>%
      dplyr::mutate(Value = Value * ratio, UCD_technology = "FCEV", "UCD_fuel" = "Hydrogen") %>%
      dplyr::select(-ratio)

# Calculate new costs and intensities for Trucks, Buses from NG, based on ratios of NG:liquid ratio in original UCD database
    UCD_TB_Liq <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode %in% c("Truck", "Bus"), UCD_technology == "Liquids", variable %in% c("CAPEX and non-fuel OPEX", "intensity") )
    
    UCD_TB_NG <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode %in% c("Truck", "Bus"), UCD_technology == "NG", variable %in% c("CAPEX and non-fuel OPEX", "intensity") )
    
    UCD_TB_NG_Liq_ratio <- UCD_TB_NG %>%
      dplyr::left_join( UCD_TB_Liq, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year"), 
                        suffix = c(".NG", ".L")) %>%
      dplyr::mutate(ratio = Value.NG/Value.L) %>%
      dplyr::select(UCD_region, UCD_sector, mode, size.class, variable, unit, Year, ratio)
    
    # #NEW NG Truck and Bus data
    NREL_NG_TB <- NREL_ICE_long %>%
      dplyr::filter( mode %in% c("Truck", "Bus"), UCD_technology == "Liquids", variable %in% c("CAPEX and non-fuel OPEX", "intensity") ) %>%
      dplyr::left_join(UCD_TB_NG_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year")) %>%
      dplyr::mutate(Value = Value * ratio, UCD_technology = "NG", "UCD_fuel" = "Natural Gas") %>%
      dplyr::select(-ratio)
    
# Calculate new intensities for LDCs from NG, based on ratios of NG:liquid ratio in original UCD database
    UCD_LDC_Liq <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode %in% c("LDV_4W"), UCD_technology == "Liquids", variable %in% c("intensity") )
    
    UCD_LDC_NG <- UCD_clean_complete_years_long %>%
      dplyr::filter( mode %in% c("LDV_4W"), UCD_technology == "NG", variable %in% c("intensity") )
    
    UCD_LDC_NG_Liq_ratio <- UCD_LDC_NG %>%
      dplyr::left_join( UCD_LDC_Liq, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year"), 
                        suffix = c(".NG", ".L")) %>%
      dplyr::mutate(ratio = Value.NG/Value.L) %>%
      dplyr::select(UCD_region, UCD_sector, mode, size.class, variable, unit, Year, ratio)    

    # #NEW NG LDC data    
    NREL_NG_LDC_intensity <- NREL_ICE_long %>%
      dplyr::filter( mode %in% c("LDV_4W"), UCD_technology == "Liquids", variable %in% c("intensity") ) %>%
      dplyr::left_join(UCD_LDC_NG_Liq_ratio, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year")) %>%
      dplyr::mutate(Value = Value * ratio, UCD_technology = "NG", "UCD_fuel" = "Natural Gas") %>%
      dplyr::select(-ratio)
    
# Calculate new NG infrastructure costs based on 0.5 * FCEV infrastructure (LDCs)
    NREL_NG_inf <- NREL_FCEV_op_inf_int %>%
      filter(variable == "Capital costs (infrastructure)") %>%
      mutate(Value = Value/2,
             UCD_technology = "NG",
             UCD_fuel = "Natural Gas")
    
# Calculate new NG infrastructure costs for Trucks (based on Tong et. al. (2019))
    
# First, get UCD intensity for USA Truck >12t (this is the base truck)
  UCD_USA_NG_truck12t <- UCD_clean_complete_years_long %>%
    filter(UCD_region == "USA", size.class == "Truck (>12t)",  UCD_technology == "NG", variable == "intensity", Year == "X2020") %>%
    select(variable, mode, UCD_technology, Value)
    
 Tong_NG_truck_cap_cost_inf <- UCD_clean_complete_years_long %>%
   filter(variable == "intensity", mode ==  "Truck", UCD_technology == "NG") %>%
   left_join(UCD_USA_NG_truck12t, by = c("variable", "mode", "UCD_technology"), suffix = c(".truck", ".base_truck")) %>%
   # divide to get ratio between base truck and each truck intensity
   mutate(ratio = Value.truck / Value.base_truck,
          CNG_inf_cost = ratio * CNG_US_HDT_INF_2005USD_VKT,
          LNG_inf_cost = ratio * LNG_US_HDT_INF_2005USD_VKT,
          # for now, take LNG as the cost, 
          Value = LNG_inf_cost,
          unit = "2005$/vkt",
          #change variable name so we can match and add it to the rest of CAPEX and non-fuel OPEX in the next step
          variable = "CAPEX and non-fuel OPEX") %>%
   select(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year, Value)
 
# Add new NG Truck infrastructure costs to the rest of CAPEX and non-fuel OPEX
 NREL_NG_TB_with_truck_inf_cost <- NREL_NG_TB %>%
   bind_rows(Tong_NG_truck_cap_cost_inf) %>%
   group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, Year) %>%
   dplyr::summarise(Value = sum(Value)) %>%
   ungroup()
   
   
# # ------------------------------------------------------------------------------
    #Bind Hybrid Liquids and FCEV cars and NG TBs together
    NREL_all_Vehicles <- dplyr::bind_rows( NREL_Hybrid_Liq, NREL_NEMS_FCEV_purchase, NREL_FCEV_op_inf_int, NREL_NG_TB_with_truck_inf_cost, NREL_NG_LDC_intensity, NREL_NG_inf )
    
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
    
    #Add BEV 3W for the regions that have 3W
    #Obtain ratio between New BEV and Liquid Mini cars intensity to determine new intensity for BEV 3W
    NREL_Liq_mini_car_intensity_3W <- NREL_ICE_long %>%
      #filter only regions that have 3W. Use mini car as this is the closest to 3W
      filter(UCD_region %in% c("Africa", "China", "India", "Southeast Asia"), size.class == "Mini Car", variable == "intensity")
      
    
    NREL_BEV_Liq_mini_car_intensity_ratio_3W <- NREL_BEV_long %>%
      #filter only regions that have 3W. Use mini car as this is the closest to 3W
      filter(UCD_region %in% c("Africa", "China", "India", "Southeast Asia"), size.class == "Mini Car", variable == "intensity") %>% 
      left_join(NREL_Liq_mini_car_intensity_3W, by = c("UCD_region", "UCD_sector", "mode", "size.class", "variable", "unit", "Year"),
                suffix = c(".BEV", ".Liq")) %>%
      mutate(ratio = Value.BEV/Value.Liq) %>%
      select(UCD_region, variable, unit, Year, ratio)
      
    New_3W_intensity <- UCD_clean_complete_years_long %>%
      filter(mode == "LDV_3W",UCD_fuel == "Liquids", variable == "intensity") %>%
      left_join(NREL_BEV_Liq_mini_car_intensity_ratio_3W, by = c("UCD_region", "variable", "unit", "Year")) %>%
      # New BEV intensity = Liquid intensity * BEV/Liq ratio as calculated above
      mutate(Value = Value*ratio, UCD_technology = "BEV", UCD_fuel = "Electricity") %>%
      select(-ratio)
    
    #Create all the other new variables for BEV 3W, bind new intensity with this table
    New_3W <- UCD_clean_complete_years_long %>%
      filter(mode == "LDV_3W", UCD_fuel == "Liquids", variable %in% c("Capital costs (total)",
                                                                      "energy",
                                                                      "load factor",
                                                                      "Operating costs (total non-fuel)")) %>%
      mutate(UCD_technology = "BEV", UCD_fuel = "Electricity", 
             Value = if_else(variable == "energy", 0, Value)) %>%
      bind_rows(New_3W_intensity) %>%
      arrange(UCD_region, Year, variable)
    
    #Spread years
    NREL_final <- bind_rows(NREL_all_Vehicles, NREL_cap_costs_other, New_3W) %>%
      tidyr::spread( Year, Value )
    

# # ------------------------------------------------------------------------------
#  # V. Write out the final data to a csv

      #Set WD to output folder and define warning for if df has no data
      if( computer_type == "MAC" ){

        setwd( "/Users/yarl762/Desktop/Vehicle_costs/Output" )

      } else {

        setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Output" )

      }


      write.csv( NREL_final, file = "NREL_Other_data.csv" , row.names=FALSE, na = "" )
