#------------------------------------------------------------------------------------
#USA's region in the GCAM data system
USA_regID <- 1

#regional_fuel_markets: fuels whose markets will be modeled at the level of the FERC regions, with prices calibrated
regional_fuel_markets <- c( "regional coal", "delivered coal", "wholesale gas", "delivered gas", "refined liquids industrial", "refined liquids enduse" )
#also indicate whether to use regional as opposed to national fuel markets (FALSE = national markets)
use_regional_fuel_markets <- TRUE

#use_regional_elec_markets: indicate whether to resolve electricity demands at the level of the nation or the grid regions
elect_td_sectors <- c( "elect_td_bld", "elect_td_ind", "elect_td_trn" )
use_regional_elec_markets <- TRUE

#Identifier columns
state <- "state"
state_F <- c( "state", "fuel" )
state_S <- c( "state", "sector" )
state_S_F_U <- c( "state", "sector", "fuel", "service" )
EIA_S_F <- c( "EIA_sector", "EIA_fuel" )
S_F <- c( "sector", "fuel" )
S_F_tech <- c( "sector", "fuel", "technology" )
F_U <- c( "fuel", "service" )
S_F_U <- c( "sector", "fuel", "service" )
F_tech <- c( "fuel", "technology" )
R_S_F <- c( "GCAM_region_ID", "sector", "fuel" )
state_S_F <- c( "state", "sector", "fuel" )
UCD_techID <- c( "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel" )

sup_sub_tech <- c( "supplysector", "subsector", "technology" )
sup_sub_input <- c( "supplysector", "subsector", "minicam_energy_input" )
sup_sub_tech_input <- c( "supplysector", "subsector", "technology", "minicam_energy_input" )
R_sup_sub_tech_input <- c( "region", "supplysector", "subsector", "technology", "minicam_energy_input" )
R_sup_sub_stubtech_input <- c( "region", "supplysector", "subsector", "stub_technology", "minicam_energy_input" )
globaltech_ID <- c( "sector_name", "subsector_name", "technology" )

#list the states
states <- c( "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
             "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
             "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY" )

#Shorthand names for residential buildings services in the RECS databases
RECS_heating_fuels <- c( "electricity", "gas", "refined liquids" )
RECS_cooling_fuels <- "electricity"
RECS_hotwater_fuels <- c( "electricity", "gas", "refined liquids" )

#services in the CBECS databases
CBECS_heating_fuels <- c( "electricity", "gas", "refined liquids" )
CBECS_cooling_fuels <- c( "electricity", "gas" )

#select the number of decimal points to round
CalInput_digits <- 6
flsp_digits <- 6
flsp_sat_digits <- 9
HDDCDD_digits <- 0
cost_digits <- 3
intgains_digits <- 3
retirement_fn_digits <- 3
BEND_efficiency_digits <- 4
BEND_kbtu_digits <- 0
BEND_Tbtu_digits <- 4
capacity_factor_digits <- 2

P_elas_flsp <- -1         #price elasticity of floorspace

#Set the satiation level of floorspace, compared with base year per-capita floorspace demands
satiation_flsp_mult <- 1.118

#Buildings node structure
bld_node_names <- c( "gcam_consumer", "nodeInput", "building_node_input" )

#buildings service names
thermal_services <- c( "resid heating", "resid cooling", "comm heating", "comm cooling" )
heating_services <- c( "resid heating", "comm heating" )
cooling_services <- c( "resid cooling", "comm cooling" )

floor_to_surface <- 5.5

coal_phaseout_year <- 2020   #the year at which coal use in buildings is assumed to go to 0
efficiency_partition_year <- 2005 # the year in which stock averages are used to allocate shares to hi-tech and typical techs

BEND_serviceID <- c( "BEND_sector", "BEND_service" )
BEND_techID <- c( "BEND_sector", "BEND_service", "BEND_fuel", "BEND_technology" )

#Average USA HDD and CDD in 2005, used for state indexing
base_HDD_USA <- 4524
base_CDD_USA <- 1215

#logit exponent regulating competition between different grid regions in USA electricity market (single market approach only)
grid_region_logit <- -6

#Resources that will be modeled at the state level
state_renewable_resources <- c( "distributed_solar", "geothermal", "onshore wind resource" )
state_unlimited_resources <- c( "global solar resource", "limestone" )
geothermal_default_efficiency <- 0.1

