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
logstart( "L100.IEA_downscale_ctry.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "IEA energy balances by all countries and historical years" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )
en_OECD <- readdata( "ENERGY_LEVEL0_DATA", "en_OECD" )
en_nonOECD <- readdata( "ENERGY_LEVEL0_DATA", "en_nonOECD" )
IEA_product_downscaling <- readdata( "ENERGY_MAPPINGS", "IEA_product_downscaling" )
IEA_ctry <- readdata( "ENERGY_MAPPINGS", "IEA_ctry" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# subset only the relevant years and combine OECD with non-OECD
printlog( "Combining OECD and non-OECD databases, subsetting only specified years")
IEA_IDcodes <- c( "COUNTRY", "FLOW", "PRODUCT" )
L100.IEAfull <- rbind( en_OECD[ c( IEA_IDcodes, X_historical_years ) ], en_nonOECD[ c( IEA_IDcodes, X_historical_years ) ] )

#Rename fuels with inconsistent naming between the two databases
L100.IEAfull$PRODUCT[ L100.IEAfull$PRODUCT == "Natural Gas" ] <- "Natural gas"
L100.IEAfull$PRODUCT[ L100.IEAfull$PRODUCT == "Other Kerosene" ] <- "Other kerosene"
L100.IEAfull$PRODUCT[ L100.IEAfull$PRODUCT == "Total" ] <- "Total of all energy sources"

###UP FRONT ADJUSTMENTS
# NEARLY THE ENTIRE SUPPLY OF NATURAL GAS IN OTHER AFRICA BETWEEN 2001 and 2004 IS ALLOCATED TO GTL PLANTS
# OPERATING AT NEARLY 100% EFFICIENCY. ADJUSTING THE ENERGY INPUT QUANTITIES TO AVOID NEGATIVE VALUES LATER ON.
GTL_coef <- 1.7
GTL_adj_years <- paste0( "X", 2001:2004 )
L100.IEAfull[ L100.IEAfull$COUNTRY == "Other Africa" & L100.IEAfull$FLOW == "TGTL" & L100.IEAfull$PRODUCT == "Natural gas",  GTL_adj_years] <-
      L100.IEAfull[ L100.IEAfull$COUNTRY == "Other Africa" & L100.IEAfull$FLOW == "TGTL" & L100.IEAfull$PRODUCT == "Other hydrocarbons",  GTL_adj_years] *
      GTL_coef * -1     #Need to multiply by -1 because other hydrocarbons are the output and have a different sign

# TURKEY HAS ELECTRICITY PRODUCTION FROM PRIMARY SOLID BIOFUELS (ELAUTOC) BETWEEN 1971 AND 1981 WITH NO CORRESPONDING FUEL INPUT BY ANY SECTORS
# ADDING A FUEL INPUT TO AVOID NEGATIVE NUMBERS LATER ON.
CHP_IOcoef <- 5
conv_GWh_ktoe <- 0.08598452 #ELAUTOC (the output) is in gigawatt hours, whereas AUTOCHP (the input) is in ktoe
CHP_adj_years <- paste0( "X", 1971:1981 )
L100.IEAfull[ L100.IEAfull$COUNTRY == "Turkey" & L100.IEAfull$FLOW == "AUTOCHP" & L100.IEAfull$PRODUCT == "Primary solid biofuels",  CHP_adj_years] <-
      L100.IEAfull[ L100.IEAfull$COUNTRY == "Turkey" & L100.IEAfull$FLOW == "ELAUTOC" & L100.IEAfull$PRODUCT == "Primary solid biofuels",  CHP_adj_years] *
      CHP_IOcoef * conv_GWh_ktoe * -1

#Split the country mapping table into composite regions and single-countries
IEA_ctry_composite <- subset( IEA_ctry, IEA_ctry %in% c( "Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)",
      "Other Africa", "Other Non-OECD Americas", "Other Asia" ) )
IEA_ctry_single <- subset( IEA_ctry, IEA_ctry %!in% IEA_ctry_composite$IEA_ctry )

#Split IEA energy balances into table of single countries and composite regions (keeping only desired composite regions)
L100.IEAcomposite <- subset( L100.IEAfull, COUNTRY %in% IEA_ctry_composite$IEA_ctry )
L100.IEAsingle <- subset( L100.IEAfull, COUNTRY %in% IEA_ctry_single$IEA_ctry )
L100.IEAsingle$iso <- IEA_ctry_single$iso[ match( L100.IEAsingle$COUNTRY, IEA_ctry_single$IEA_ctry ) ]

#Subset countries that are being downscaled in certain years using historical energy data in a specified year
#Former Soviet Union and Yugoslavia: use specified flows for each product
## The IEA data has many inter-sectoral inconsistencies between the USSR and separated countries thereafter. Results in unrealistic fuel shares.

postUSSR_Yug_years_IEA <- historical_years[ historical_years >= 1990 ]
X_postUSSR_Yug_years_IEA <- paste( "X", postUSSR_Yug_years_IEA, sep = "" )
USSR_Yug_years <- historical_years[ historical_years < 1990 ]
X_USSR_Yug_years <- paste( "X", USSR_Yug_years, sep = "" )
L100.USSR_Yug <- subset( L100.IEAcomposite, COUNTRY %in% c( "Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)" ) )

#Re-map the "if no detail" forms of coal in the historical years prior to the relevant coal types for matching with the more recent years
X_no_detail_coal_years <- paste( "X", 1971:1977, sep = "" )
##Hard coal needs to split proportionally between coking coal and other bituminous coal in order to minimize bias from
## different country-wise shares of the two fuel types. Note that anthracite is not considered in these regions
##NOTE: using round() to avoid NA's for any values whose base value is >1e6. This seems to work
L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Other bituminous coal", X_no_detail_coal_years ] <-
      L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Hard coal (if no detail)", X_no_detail_coal_years ] *
      round( L100.USSR_Yug$X1978[ L100.USSR_Yug$PRODUCT == "Other bituminous coal" ] /
             ( L100.USSR_Yug$X1978[ L100.USSR_Yug$PRODUCT == "Coking coal" ] +
               L100.USSR_Yug$X1978[ L100.USSR_Yug$PRODUCT == "Other bituminous coal" ] + 1e-3 ), 2 )
L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Coking coal", X_no_detail_coal_years ] <-
      L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Hard coal (if no detail)", X_no_detail_coal_years ] -
      L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Other bituminous coal", X_no_detail_coal_years ]
L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Hard coal (if no detail)", X_no_detail_coal_years ] <- 0

#Brown coal is simpler, as lignite is the only relevant fuel (sub-bituminous coal is not considered in these regions)
L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Lignite", X_no_detail_coal_years ] <-
      L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Brown coal (if no detail)", X_no_detail_coal_years ]
L100.USSR_Yug[ L100.USSR_Yug$PRODUCT == "Brown coal (if no detail)", X_no_detail_coal_years ] <- 0

L100.USSR_Yug_ctry <- subset( L100.IEAsingle, iso %in% IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry %in%
      c( "Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)" ) ] )
L100.USSR_Yug_ctry$IEAcomposite <- IEA_ctry_composite$IEA_ctry[ match( L100.USSR_Yug_ctry$iso, IEA_ctry_composite$iso ) ]
L100.USSR_Yug_ctry_FLOW_PRODUCT <- subset( L100.USSR_Yug_ctry, paste( PRODUCT, FLOW ) %in% paste( IEA_product_downscaling$PRODUCT, IEA_product_downscaling$FLOW ) )
L100.USSR_Yug_FLOW_PRODUCT <- aggregate( L100.USSR_Yug_ctry_FLOW_PRODUCT[ "X1990" ],
      by=as.list( L100.USSR_Yug_ctry_FLOW_PRODUCT[ c( "IEAcomposite", "FLOW", "PRODUCT" ) ] ), sum )
L100.USSR_Yug_ctry_FLOW_PRODUCT$X1990_share <- L100.USSR_Yug_ctry_FLOW_PRODUCT$X1990 /
       L100.USSR_Yug_FLOW_PRODUCT$X1990[
          match( paste( L100.USSR_Yug_ctry_FLOW_PRODUCT$IEAcomposite, L100.USSR_Yug_ctry_FLOW_PRODUCT$PRODUCT ),
                 paste( L100.USSR_Yug_FLOW_PRODUCT$IEAcomposite, L100.USSR_Yug_FLOW_PRODUCT$PRODUCT ) ) ]
L100.USSR_Yug_ctry_FLOW_PRODUCT[ is.na( L100.USSR_Yug_ctry_FLOW_PRODUCT ) ] <- 0

#Calculate the energy balances of the individual countries during the USSR years as the total in the composite region times the country-wise shares in 1990
L100.USSR_Yug_ctry_bal <- L100.USSR_Yug_ctry[ c( "iso", "FLOW", "PRODUCT", "IEAcomposite" ) ]
L100.USSR_Yug_ctry_bal[ X_USSR_Yug_years ] <-
      L100.USSR_Yug[
         match( paste( L100.USSR_Yug_ctry_bal$IEAcomposite, L100.USSR_Yug_ctry_bal$FLOW, L100.USSR_Yug_ctry_bal$PRODUCT ),
                paste( L100.USSR_Yug$COUNTRY, L100.USSR_Yug$FLOW, L100.USSR_Yug$PRODUCT ) ),
            X_USSR_Yug_years ] *
      L100.USSR_Yug_ctry_FLOW_PRODUCT$X1990_share[
         match( paste( L100.USSR_Yug_ctry_bal$iso, L100.USSR_Yug_ctry_bal$PRODUCT ),
                paste( L100.USSR_Yug_ctry_FLOW_PRODUCT$iso, L100.USSR_Yug_ctry_FLOW_PRODUCT$PRODUCT ) ) ]
L100.USSR_Yug_ctry_bal[ X_postUSSR_Yug_years_IEA ] <- L100.USSR_Yug_ctry[ X_postUSSR_Yug_years_IEA ]

#Composite regions where population is used to downscale energy to countries over all historical years
#Subset composite regions
L100.Afr <- subset( L100.IEAcomposite, COUNTRY == "Other Africa" )
L100.LAM <- subset( L100.IEAcomposite, COUNTRY == "Other Non-OECD Americas" )
L100.Asia <- subset( L100.IEAcomposite, COUNTRY == "Other Asia" )

#Repeat by number of countries in each
L100.Afr_repCtry <- repeat_and_add_vector( L100.Afr, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Africa" ] )
L100.LAM_repCtry <- repeat_and_add_vector( L100.LAM, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Non-OECD Americas" ] )
L100.Asia_repCtry <- repeat_and_add_vector( L100.Asia, "iso", IEA_ctry_composite$iso[ IEA_ctry_composite$IEA_ctry == "Other Asia" ] )

#Combine these into a single data table
L100.Others_repCtry <- rbind( L100.Afr_repCtry, L100.LAM_repCtry, L100.Asia_repCtry )

#Calculate population shares
L100.Others_pop <- subset( L100.Pop_thous_ctry_Yh, iso %in% L100.Others_repCtry$iso )
L100.Others_pop$IEAcomposite <- IEA_ctry_composite$IEA_ctry[ match( L100.Others_pop$iso, IEA_ctry_composite$iso ) ]

#Aggregate by country-within-composite-region and year to calculate population shares
L100.Composites_pop <- aggregate( L100.Others_pop[ X_historical_years ], by=as.list( L100.Others_pop[ "IEAcomposite" ] ), sum )
L100.Others_pop_share <- L100.Others_pop[ "iso" ]
L100.Others_pop_share[ X_historical_years ] <-  L100.Others_pop[ X_historical_years ] / L100.Composites_pop[
      match( L100.Others_pop$IEAcomposite, L100.Composites_pop$IEAcomposite ),
      X_historical_years ]

#Multiply the repeated country databases by the population shares to get the energy balances by country
L100.Others_ctry_bal <- data.frame( L100.Others_repCtry[ c( "iso", "FLOW", "PRODUCT" ) ], IEAcomposite = L100.Others_repCtry$COUNTRY )
L100.Others_ctry_bal[ X_historical_years ] <- L100.Others_repCtry[ X_historical_years ] *
      L100.Others_pop_share[ match( L100.Others_ctry_bal$iso, L100.Others_pop_share$iso ),
      X_historical_years ]

#Subset each of these final energy balances to only the rows that aren't zero in all years
L100.IEAsingle <- L100.IEAsingle[ rowSums( L100.IEAsingle[ X_historical_years ] ) !=0, ]
L100.IEAsingle_noUSSR_Yug <- subset( L100.IEAsingle, iso %!in% L100.USSR_Yug_ctry_bal$iso )
L100.USSR_Yug_ctry_bal <- L100.USSR_Yug_ctry_bal[ rowSums( L100.USSR_Yug_ctry_bal[ X_historical_years ] ) !=0, ]
L100.Others_ctry_bal <- L100.Others_ctry_bal[ rowSums( L100.Others_ctry_bal[ X_historical_years ] ) !=0, ]
      
#Combine the country-level data tables and write out energy balances (using iso codes rather than IEA's country names)
IEA_isoID <- c( "iso", "FLOW", "PRODUCT" )
L100.IEA_en_bal_ctry_hist <- rbind( L100.IEAsingle_noUSSR_Yug[ c( IEA_isoID, X_historical_years ) ],
                                    L100.USSR_Yug_ctry_bal[ c( IEA_isoID, X_historical_years ) ],
                                    L100.Others_ctry_bal[ c( IEA_isoID, X_historical_years ) ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.IEA_en_bal_ctry_hist <- c( "IEA energy balances downscaled to 201 countries (iso / FLOW / PRODUCT / historical year)","Unit = ktoe and GWh" )

#write tables as CSV files
writedata( L100.IEA_en_bal_ctry_hist, domain="ENERGY_LEVEL1_DATA", fn="L100.IEA_en_bal_ctry_hist", comments=comments.L100.IEA_en_bal_ctry_hist )

# Every script should finish with this line
logstop()
