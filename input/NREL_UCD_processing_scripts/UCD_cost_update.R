#   Load packages
library( "dplyr" )
library( "tidyr" )
library( "openxlsx" )
library( "zoo" )


# ---------------------------------------------------------------------------
# Program Name: BEV_vehicle_cost_update.R
# Author: Brinda Yarlagadda and Patrick O'Rourke
# Date Last Updated: 10.11.19
# Program Purpose: Take BEV and ICE (and NG, FCEV, Hybrid Liquids), replace the data in the UCD database
# Input Files:
# 
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

#   Define constants
    COMPLETE_YEARS <- c(seq(2005,2100,5))
    COMPLETE_YEARS_X <- paste0 ( "X", COMPLETE_YEARS)
    
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
  
  setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Output" )
  
} else if( user == "Pat" ){
  
  if( computer_type == "MAC" ){ 
    
    setwd( "/Users/patrickorourke/Desktop/Vehicle_costs/Output" )
    
  } else {
    
    setwd( "C:/Users/orou913/Desktop/Vehicle_costs/Output" )
    
  }
  
} else {
  
  stop ( paste0( "setwd() Options are not set for this user. See Section 1." ) )
  
}

# # ------------------------------------------------------------------------------
#  I. Read in the final NREL BEV and ICE data, UCD transportation database

NREL_BEV_data <- readr::read_csv( "NREL_BEV_data.csv" )
NREL_ICE_data <- readr::read_csv( "NREL_ICE_data.csv" )
NREL_NG_data <- readr::read_csv( "NREL_NG_data.csv" )
NREL_Other_data <- readr::read_csv( "NREL_Other_data.csv" )

#   Load UCD transportation data
UCD <- read.csv( "UCD_trn_data_CORE_ref.csv", skip=7 )


# # ------------------------------------------------------------------------------
#   II. Clean UCD Data - transform identification variables to characters
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


# # ------------------------------------------------------------------------------
#  III. Final NREL data cleaning and update UCD data with cleaned NREL data

# Combine all NREL data
NREL_data <- dplyr::bind_rows( NREL_BEV_data, NREL_ICE_data, NREL_NG_data, NREL_Other_data )

#   Make unique list NREL info variables (pasted as one variable)
NREL_update_info <- NREL_data %>%
  dplyr::select( UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable,
                 unit ) %>%
  dplyr::distinct( ) %>%
  dplyr::mutate( full_name = paste0( UCD_region, "-", UCD_sector, "-", mode, "-", size.class, "-",
                                     UCD_technology, "-", UCD_fuel, "-", variable,
                                     "-", unit  ) ) %>%
  dplyr::select( full_name )

NREL_update_unique <- sort( unique( NREL_update_info$full_name ) )

#   Subset UCD data - without data to be updated
UCD_without_rows_to_be_updated <- UCD_clean_complete_years %>%
  dplyr::mutate( full_name = paste0( UCD_region, "-", UCD_sector, "-", mode, "-", size.class, "-",
                                     UCD_technology, "-", UCD_fuel, "-", variable,
                                     "-", unit  ) ) %>%
  dplyr::filter( ! ( full_name %in% NREL_update_unique ) ) %>%
  dplyr::select( -full_name )

#   Subset UCD data to be updated
UCD_to_update <- UCD_clean_complete_years %>%
  dplyr::mutate( full_name = paste0( UCD_region, "-", UCD_sector, "-", mode, "-", size.class, "-",
                                     UCD_technology, "-", UCD_fuel, "-", variable,
                                     "-", unit  ) ) %>%
  dplyr::filter( full_name %in% NREL_update_unique ) %>%
  dplyr::select( -full_name )

#   Update UCD data
UCD_NREL_updated <- UCD_to_update %>%
  dplyr::select( -COMPLETE_YEARS_X ) %>%
  dplyr::left_join( NREL_data, by = c("UCD_region",  "UCD_sector", "mode", "size.class",
                                       "UCD_technology", "UCD_fuel", "variable",
                                       "unit" ) )

#add in BEV Truck and Bus data, 3W data - this is new and not updating existing UCD data
UCD_NREL_updated_added <- dplyr::bind_rows( UCD_NREL_updated, 
                                     filter( NREL_data, UCD_technology == "BEV", mode %in% c("Truck", "Bus", "LDV_3W")))


#   Combine updated data with original data
UCD_with_update_final <- dplyr::bind_rows( UCD_without_rows_to_be_updated, UCD_NREL_updated_added )

UCD_with_update_final <- filter(UCD_with_update_final, variable %!in% c("Operating subsidy", "Capital costs (other)"))

colnames( UCD_with_update_final ) <- c("UCD_region",  "UCD_sector", "mode", "size.class",
                                       "UCD_technology", "UCD_fuel", "variable",
                                       "unit", COMPLETE_YEARS)


# # ------------------------------------------------------------------------------
# 
# VII.  Output data (updated data, original data that was changed, changed data)

#   Set WD to output folder and define warning for if df has no data
if( computer_type == "MAC" ){
  
  setwd( "/Users/yarl762/Desktop/Vehicle_costs/Output" )
  
} else {
  
  setwd( "C:/Users/YARL762/Documents/India/Vehicle_costs/Output" )
  
}

# ------------------------------------------------------------------------------



# Output original data which was updated (subset of original data)
#write.csv( UCD_to_update, file = "UCD-original_data_which_was_updated.csv" , row.names=FALSE, na = "" )

# Output updated data
write.csv( UCD_NREL_updated_added, file = "UCD_updated_data_with_NREL.csv", row.names=FALSE, na = "" )

# Output new UCD data file
write.csv( UCD_with_update_final, file = "UCD_trn_data_CORE_HighElec(09.07.20).csv" , row.names=FALSE, na = "" )


