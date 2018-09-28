#' module_gcam.usa_LA1233.Process_UCS_data_ref
#'
#' This script reads in and processes "Union of Concerned Scientists. 2012. UCS EW3 Energy-Water Database V.1.3" www.ucsusa.org/ew3database - Main Data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{LA1233.CoolingSystemShares_RG3_ref},The corresponding file in the
#' original data system was \code{LA1233.Process_UCS_data_ref.R} (gcam-usa-processing-code level1).
#' @details This script reads in and processes "Union of Concerned Scientists. 2012. UCS EW3 Energy-Water
#' Database V.1.3" - Main Data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author LL March 2017, ZK Sep 2019

module_gcam.usa_LA1233.Process_UCS_data_ref <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/UCS_Database",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/elec_tech_water_map"
             #"L120.RsrcCurves_EJ_R_offshore_wind_USA"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("LA1233.CoolingSystemShares_RG3_ref"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    plant_type <- water_type <- fuel <- technology <- State <- NEMS <- cooling_system <- NULL

# ===================================================
# Load required inputs

    #' Note 1: The states_subregions raw file was modified to include NEMS region mappings.
    #' In states_subregion.csv row 65, West Virginia Central East Grid has subregion4 as South, insted of Midwest
    #' as in the harmonization folder.
    #' Note 2: the file elec_tech_water_map has changed and has different mappings.
    #' Note 3: UCS_Database used blanks instead of N/A in new database. Also the "generation technology" column was missing
    #' and was added to the new database.

    UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database") # Union of concerned scientists Energy Water Database
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    elec_tech_water_map <- get_data(all_data, "gcam-usa/elec_tech_water_map")
    #L120.RsrcCurves_EJ_R_offshore_wind_USA <-get_data(all_data, "L120.RsrcCurves_EJ_R_offshore_wind_USA")

# ===================================================
# Modify Data (Remove NAs, reclassify Types, Change Fuel and Cooling Technologies to GCAM syntax)
# Note: In original code some of these modifications were done in the for loop at line 98.
# These modifications have been shifted here outside that loop.

        UCS_Database%>%
        filter((!is.na(Fuel) | Fuel!="NA") & `Reported Water Source (Type)`!="Unknown") %>%
        filter(!`Generation Technology`%in% c("CT","WN")) %>% #Remove Combustion Turbine and Wind
        mutate(`Reported Water Source (Type)`=case_when(
        `Reported Water Source (Type)`%in% c("Surface Water","Municipal","Groundwater","Unknown Freshwater",
                                             "Waste Water","GW/Waste Water","GW/Surface Water",
                                             "GW/Municipal","GW/fresh")~"fresh",
        `Reported Water Source (Type)`%in% c("Ocean","Unknown Ocean","Unknown seawater")~"seawater",
        `Cooling Technology`=="Dry Cooled"~"fresh",
        TRUE~`Reported Water Source (Type)`),
        # GCAM Fuels
        Fuel=case_when( Fuel=="Hydropower"~"hydro",
                        Fuel=="Natural Gas"~"gas",
                        Fuel=="Oil" ~"refined liquids",
                       TRUE~tolower(Fuel)),
        `Cooling Technology`=case_when(`Cooling Technology`=="Dry Cooled"~"dry cooling",
                                       TRUE~`Cooling Technology`),
        `Cooling Technology`=gsub("-"," ",`Cooling Technology`),
        `Cooling Technology`=tolower(`Cooling Technology`))->
      UCS_Database_Edited


    # NOTE: In original code after filtering and renaming, data left with only 10330 rows.
    # In new code we have 13890 rows. If we open the .csv file and manually filter the rows
    # to remove NA fuel and "unknown" Reported water source) we get 13890 rows.

    # Create data frame for complete set of technologies for each state and join NEMS region
    elec_tech_water_map%>%
      dplyr::select(plant_type,cooling_system,water_type,fuel,technology)%>%
      unique %>%
      repeat_add_columns(tibble::tibble(state = unique(states_subregions$state))) %>%
      left_join(states_subregions%>%dplyr::select(state,NEMS),by="state") %>%
      rename(State=state)->
    complete_tech

    # NOTE: Original Script edits offshore states. Line 89-95 of original code.
    # This has been left out for now.

# ===================================================
# Calculate Cooling technology share

    #---------------------------------
    # Historical cooling shares
    # For each year calculate cumulative cooling share upto that year & modify names to GCAM syntax

    cooling_share<-NULL; # initiate cooling share data frame

    for(year_i in 1970:2008){

    UCS_Database_Edited%>%
      filter(`First Year of Operation` <= year_i)%>%
      group_by(State,Fuel,`Generation Technology`,`Cooling Technology`,`Reported Water Source (Type)`)%>%
      summarise(x=sum(`Nameplate Capacity (MW)`))%>%
      mutate(plant_type=case_when((Fuel=="gas" & `Generation Technology`=="CC")~"combined cycle",
                                (Fuel=="refined liquids" & `Generation Technology`=="CC")~"CC",
                                (Fuel=="solar" & `Generation Technology`=="SU")~"CSP",
                                (Fuel %in% c("solar","wind","hydro"))~"no cooling",
                                (Fuel %in% c("gas","refined liquids","biomass"))~"fossil non-coal",
                                TRUE~Fuel))%>%
      group_by(State,plant_type,`Cooling Technology`,`Reported Water Source (Type)`,Fuel)%>%
      summarise(x=sum(x))->
      capacity_tech

    # Note: Values are different when compared with old script. However, when manually summing from original
    # UCS_database_ref.csv the values match results from this script.

    capacity_tech%>%
      group_by(State,plant_type,Fuel)%>%
      summarise(x=sum(x))->
    capacity_tech_state

   capacity_tech%>%as.data.frame%>%
     mutate(x=x/inner_join((capacity_tech%>%
                              ungroup()%>%
                              dplyr::select(State,plant_type,Fuel)),
                           capacity_tech_state,
                           by=c("State","plant_type","Fuel"))$x)%>%
     mutate(year=year_i)->
     capacity_tech_cooling


   cooling_share<-bind_rows(cooling_share,capacity_tech_cooling)

  } # Close loop for years


    cooling_share%>%unique%>%
      left_join(states_subregions%>%dplyr::select(state,NEMS)%>%rename(State=state),by="State")->
      cooling_share

    # Repeat cooling technology distribution for all technologies
    left_join((complete_tech%>%unique%>%
                 rename(Fuel=fuel,`Reported Water Source (Type)`=water_type,`Cooling Technology`=cooling_system)),
              cooling_share,by=c("plant_type","Fuel","State","NEMS","Reported Water Source (Type)","Cooling Technology"))%>%na.omit->
      cooling_share_historical

    #-----------------------------------------
    # Calculate Cooling Shares for future years
    # Note: In original code for some reason data states dropped and analysis done by NEMS regions
    # and the later states added in again. Here analysis is done by state.


    UCS_Database_Edited%>%
      filter(`First Year of Operation` > 1999)%>%
      group_by(State,Fuel,`Generation Technology`,`Cooling Technology`,`Reported Water Source (Type)`)%>%
      summarise(x=sum(`Nameplate Capacity (MW)`))%>%
      mutate(plant_type=case_when((Fuel=="gas" & `Generation Technology`=="CC")~"combined cycle",
                                (Fuel=="refined liquids" & `Generation Technology`=="CC")~"CC",
                                (Fuel=="solar" & `Generation Technology`=="SU")~"CSP",
                                (Fuel %in% c("solar","wind","hydro"))~"no cooling",
                                (Fuel %in% c("gas","refined liquids","biomass"))~"fossil non-coal",
                                TRUE~Fuel))%>%
      group_by(State,plant_type,`Cooling Technology`,`Reported Water Source (Type)`,Fuel)%>%
      summarise(x=sum(x))->
      capacity_tech_future

    # Compute shares of cooling technology by fuel
    capacity_tech_future%>%
      group_by(Fuel,`Cooling Technology`,`Reported Water Source (Type)`)%>%
      summarise(x=sum(x))->
    capacity_tech_future_US

    # Calculate Total for US by fuel
    capacity_tech_future_US%>%
      group_by(Fuel)%>%
      summarise(x=sum(x))->
      capacity_tech_future_US_Total

    capacity_tech_future_US%>%as.data.frame%>%
      mutate(x=x/inner_join((capacity_tech_future_US%>%
                               ungroup()%>%
                               dplyr::select(Fuel)),
                            capacity_tech_future_US_Total,
                            by=c("Fuel"))$x)->
      cooling_share_future_US

    # Aggregate by State & Fuel
    capacity_tech_future%>%
      group_by(State,plant_type,Fuel)%>%
      summarise(x=sum(x))->
      capacity_tech_future_state

    capacity_tech_future%>%as.data.frame%>%
      mutate(x=x/inner_join((capacity_tech_future%>%ungroup()%>%
                               dplyr::select(State,Fuel,plant_type)),
                            capacity_tech_future_state,
                            by=c("State","Fuel","plant_type"))$x)->
      cooling_share_future_state

    # --------------------------------
    # Cooling Share complete list
    # If cooling share availble by State, use that else
    # Assume the cooling share for US total

    full_join(cooling_share_future_state,cooling_share_future_US%>%rename(y=x),
              by = c("Cooling Technology", "Reported Water Source (Type)", "Fuel"))->
      cooling_share_future

    complete_tech%>%
      rename(Fuel=fuel,`Cooling Technology`=cooling_system,`Reported Water Source (Type)`=water_type)%>%
      unique%>%
      left_join(cooling_share_future,
                by = c("Fuel","Cooling Technology","plant_type","Reported Water Source (Type)","State"))%>%
      repeat_add_columns(tibble::tibble(year = c(2010,2020,2100)))%>%
      mutate(x=case_when(is.na(x)~0,TRUE~x),
             y=case_when(is.na(y)~0,TRUE~y),
             z=case_when((x==0 & y==0)~0,
                         (x==0 & y>0)~y,
                         TRUE~x))%>%
      dplyr::select(-x,-y)%>%rename(x=z)%>%
      mutate(`Cooling Technology`=case_when(plant_type=="no cooling"~"none",TRUE~`Cooling Technology`),
             `Reported Water Source (Type)`=case_when(plant_type=="no cooling"~"fresh",TRUE~`Reported Water Source (Type)`))->
      cooling_share_future_complete_tech

    # Combine into one Cooling Share Data Frame
    # All no cooling has cooling_type none and is assigned 1
    # As per original script all historical Gen_III is made 0
    bind_rows(cooling_share_historical,cooling_share_future_complete_tech)%>%unique%>%
      mutate(x=case_when(plant_type=="no cooling"~ 1,
                         (technology=="Gen_III" & year<2010)~ 0,
                         TRUE~x))->
    complete_tech_cooling_share

    # ----------------------------------------
    # Assumptions for Unassigned Cooling technologies in the Future
    # If sum of cooling technologies across water_types is less than 0, then assign all remaining shares to recirculating
    # In 2020 freshwater recirculating =0.85, dry cooling=0.05, cooling pond =0.05, Once through seawater =0.05
    # In 2020 nuclear freshwater recirculating =0.09, cooling pond=0.05, once through seawater=0.05
    # Assign 2010 to 2008 values
    # Assign 2100 to 2020 values
    # In this script have ensured that sum of cooling techs across seawater and freshwater do no exceed 1

    # CSP storage and regular after 2020 has recirculating assigned to 1.
    # In original script both dry-hybrid and recirculaitng were assigned 1 which didn't make sense.


    left_join(complete_tech_cooling_share%>%spread(key=`Cooling Technology`,value=x),
              complete_tech_cooling_share%>%group_by(plant_type,Fuel,technology,State,NEMS,year)%>%summarise(sumy=sum(x,na.rm=T)),
              by=c("plant_type","Fuel","technology","State","NEMS","year"))%>%
      mutate(sumx=rowSums(select(.,`cooling pond`,`dry cooling`,dry_hybrid,none,`once through`,recirculating),na.rm=T))%>%
      mutate(`dry cooling`=case_when((!is.na(`dry cooling`) & sumy<0.85 & year==2020 & (Fuel=="gas" | Fuel=="biomass" | Fuel=="coal" | Fuel=="refined liquids") & `Reported Water Source (Type)`=="fresh")~0.05,TRUE~`dry cooling`),
             `cooling pond`=case_when((!is.na(`cooling pond`) & sumy<0.85 & year==2020 & (Fuel=="gas" | Fuel=="nuclear" | Fuel=="biomass" | Fuel=="coal" | Fuel=="refined liquids") & `Reported Water Source (Type)`=="fresh")~0.05,TRUE~`cooling pond`),
             `once through`=case_when((!is.na(`once through`) & sumy<0.85 & year==2020 & (Fuel=="gas" | Fuel=="nuclear" | Fuel=="biomass" | Fuel=="coal" | Fuel=="refined liquids") & `Reported Water Source (Type)`=="seawater")~0.05,TRUE~`once through`))%>%
      mutate(sumx=rowSums(select(.,`cooling pond`,`dry cooling`,dry_hybrid,none,`once through`,recirculating),na.rm=T))->
      complete_tech_cooling_share_Edited

    left_join(complete_tech_cooling_share_Edited%>%dplyr::select(-sumy),
              complete_tech_cooling_share_Edited%>%dplyr::select(-sumx,-sumy)%>%gather(key=`Cooling Technology`,value=x,-Fuel,-technology,-State,-plant_type,-`Reported Water Source (Type)`,-year,-NEMS)%>%
                group_by(plant_type,Fuel,technology,State,NEMS,year)%>%summarise(sumy=sum(x,na.rm=T)),
              by=c("plant_type","Fuel","technology","State","NEMS","year"))%>%
      mutate(recirculating=case_when(is.na(recirculating)~0,TRUE~recirculating),
             recirculating=case_when((sumy!=0 & sumy!=1 & `Reported Water Source (Type)`=="fresh")~recirculating+(1-sumy),
                                     (Fuel=="solar CSP" & `Reported Water Source (Type)`=="fresh")~recirculating+(1-sumy),
                                     TRUE~recirculating))%>%
      mutate(sumx=rowSums(select(.,`cooling pond`,`dry cooling`,dry_hybrid,none,`once through`,recirculating),na.rm=T))%>%
      dplyr::select(-sumx,-sumy)%>%
      gather(key=`Cooling Technology`,value=x,-Fuel,-technology,-State,-plant_type,-`Reported Water Source (Type)`,-year,-NEMS)%>%
      spread(key=year,value=x)%>%
      rename(fuel=Fuel,water_type=`Reported Water Source (Type)`,cooling_system=`Cooling Technology`)%>%
      replace(., is.na(.), 0)%>%
      mutate(`2010`=`2008`,`2100`=`2020`)->
      LA1233.CoolingSystemShares_RG3_ref

    # Checks
    # d<- LA1233.CoolingSystemShares_RG3_ref
    # d%>%filter(State=="CA",fuel=="nuclear",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="gas",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="coal",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="biomass",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="refined liquids",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="geothermal",`2100`!=0);d%>%filter(fuel=="geothermal",`2100`!=0);
    # d%>%filter(State=="CA",fuel=="solar CSP",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="solar PV",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="wind",`2100`!=0)
    # d%>%filter(State=="CA",fuel=="hydro",`2100`!=0)
    # d%>%filter(`2020`<0)

    # NOTES: Differences from original script data output
    # Note 1: All "no cooling" plant_types are now assinged "fresh" water_type. In old data wind
    # and rooftop_pv were assigned "none" water_type while hydro, PV, PV_storage were assigned "fresh".
    # Note 2: In CA for fue==coal the original script uses a US average distribution. However, in this script when
    # future technologies do not exist in a state (past 1999) in the UCS database then it is assumed that those technologies do not have
    # a representative share based on the US average. The distribution is based on the assumptions made above for dry cooling, recirculating,
    # once through.
    # Note 3: rooftop_pv in the original script has no shares assigned to it. In this script all technologies have summed shares == 1. In this
    # case the cooling system is "none" with a share of 1.


# ===================================================
# Produce Outputs

    LA1233.CoolingSystemShares_RG3_ref %>%
      add_title("Historical and future power plant cooling technology shares by state") %>%
      add_units("ratio") %>%
      add_comments("Written by LA1233.Process_UCS_data_ref.R") %>%
      add_legacy_name("LA1233.Process_UCS_data_ref") %>%
      add_precursors(
        #"L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/states_subregions",
                     "gcam-usa/elec_tech_water_map"
                     ) ->
      LA1233.CoolingSystemShares_RG3_ref

    return_data(LA1233.CoolingSystemShares_RG3_ref)
  } else {
    stop("Unknown command")
  }
}
