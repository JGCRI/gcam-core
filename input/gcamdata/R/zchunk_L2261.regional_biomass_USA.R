# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2261.regional_biomass_USA
#'
#' Create biomass supply sectors at the state level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2261.DeleteSupplysector_bio_USA}, \code{L2261.Supplysector_bio_USA},
#' \code{L2261.SubsectorShrwtFllt_bio_USA}, \code{L2261.SubsectorInterp_bio_USA}, \code{L2261.SubsectorLogit_bio_USA},
#' \code{L2261.StubTech_bio_USA}, \code{L2261.StubTechMarket_bio_USA}, \code{L2261.StubTechShrwt_rbO_USA},
#' \code{L2261.StubTechFractSecOut_bio_USA}, \code{L2261.StubTechFractProd_bio_USA}, \code{L2261.Rsrc_DDGS_USA},
#' \code{L2261.RsrcPrice_DDGS_USA}, \code{L2261.Tech_rbm_USA}, \code{L2261.TechShrwt_rbm_USA},
#' \code{L2261.TechCoef_rbm_USA}, \code{L2261.Tech_dbm_USA}, \code{L2261.TechShrwt_dbm_USA},
#' \code{L2261.TechEff_dbm_USA}, \code{L2261.TechCost_dbm_USA}, \code{L2261.CarbonCoef_bio_USA},
#' \code{L2261.StubTechMarket_en_USA}, \code{L2261.StubTechMarket_elecS_USA}, \code{L2261.StubTechMarket_ind_USA},
#' \code{L2261.StubTechMarket_cement_USA}, \code{L2261.StubTechMarket_bld_USA}.
#' The corresponding file in the original data system was \code{L2261.regional_biomass_USA.R} (gcam-usa level2).
#' @details Create biomass supply sectors at the state level, in order ensure that biomass carbon-tracking is
#' contained entirely within the consuming region (state).
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter inner_join if_else mutate select semi_join
#' @importFrom tibble tibble
#' @author MTB Aug 2018
module_gcamusa_L2261.regional_biomass_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-usa/A28.sector",
             FILE = 'gcam-usa/A23.elecS_tech_mapping_cool',
             FILE = "energy/calibrated_techs",
             FILE = "gcam-usa/usa_seawater_states_basins",
             "L122.out_EJ_state_refining_F",
             "L202.CarbonCoef",
             "L221.GlobalTechCoef_en",
             "L221.StubTechCoef_bioOil",
             "L221.SubsectorInterp_en",
             "L221.StubTech_en",
             "L221.StubTechShrwt_bioOil",
             "L221.StubTechFractProd_en",
             "L221.StubTechFractSecOut_en",
             "L221.Rsrc_en",
             "L221.RsrcPrice_en",
             "L226.SubsectorInterp_en",
             "L226.GlobalTechEff_en",
             "L226.GlobalTechCost_en",
             "L222.StubTechMarket_en_USA",
             "L2234.StubTechMarket_elecS_USA",
             "L232.StubTechMarket_ind_USA",
             "L2321.StubTechMarket_cement_USA",
             "L244.StubTechMarket_bld",
             "L221.StubTechFractCalPrice_en",
             "L221.StubTechCalInput_bioOil",
             "L221.StubTechInterp_bioOil"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2261.DeleteSupplysector_bio_USA",
             "L2261.Supplysector_bio_USA",
             "L2261.SubsectorShrwtFllt_bio_USA",
             "L2261.SubsectorInterp_bio_USA",
             "L2261.SubsectorLogit_bio_USA",
             "L2261.StubTech_bio_USA",
             "L2261.StubTechMarket_bio_USA",
             "L2261.StubTechCoef_bioOil_USA",
             "L2261.StubTechShrwt_rbO_USA",
             "L2261.StubTechFractSecOut_bio_USA",
             "L2261.StubTechFractProd_bio_USA",
             "L2261.StubTechFractCalPrice_bio_USA",
             "L2261.StubTechCalInput_bio_USA",
             "L2261.StubTechInterp_bio_USA",
             "L2261.Rsrc_DDGS_USA",
             "L2261.RsrcPrice_DDGS_USA",
             "L2261.Tech_rbm_USA",
             "L2261.TechShrwt_rbm_USA",
             "L2261.TechCoef_rbm_USA",
             "L2261.Tech_dbm_USA",
             "L2261.TechShrwt_dbm_USA",
             "L2261.TechEff_dbm_USA",
             "L2261.TechCost_dbm_USA",
             "L2261.CarbonCoef_bio_USA",
             "L2261.StubTechMarket_en_USA",
             "L2261.StubTechMarket_elecS_USA",
             "L2261.StubTechMarket_ind_USA",
             "L2261.StubTechMarket_cement_USA",
             "L2261.StubTechMarket_bld_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence check package notes
    from.year <- input.cost <- input.unit <- logit.exponent <- logit.type <- market.name <-
      minicam.non.energy.input <- output.unit <- price.unit <- region <- sector.name <- state <-
      stub.technology <- subsector <- supplysector <- technology <- to.year <- year <- traded <-
      subsector.name <- share.weight <- fractional.secondary.output <- price <- fraction.produced <-
      PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- minicam.energy.input <- sector <- calibrated.value <-
      value <- share <- fuel <- . <- output.ratio <- subs.share.weight <- tech.share.weight <-
      subsector_1 <- to.technology <- NULL

    # Load required inputs
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = TRUE)
    A26.sector <- get_data(all_data, "energy/A26.sector", strip_attributes = TRUE)
    A28.sector <- get_data(all_data, "gcam-usa/A28.sector", strip_attributes = TRUE)
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = TRUE)
    A26.sector <- get_data(all_data, "energy/A26.sector", strip_attributes = TRUE)
    A28.sector <- get_data(all_data, "gcam-usa/A28.sector", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    L122.out_EJ_state_refining_F <- get_data(all_data, "L122.out_EJ_state_refining_F")
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef", strip_attributes = TRUE)
    L221.GlobalTechCoef_en <- get_data(all_data, "L221.GlobalTechCoef_en", strip_attributes = TRUE)
    #Adding in country level co-efficients for regional oil crop
    L221.StubTechCoef_bioOil<- get_data(all_data, "L221.StubTechCoef_bioOil", strip_attributes = TRUE)
    L221.SubsectorInterp_en <- get_data(all_data, "L221.SubsectorInterp_en", strip_attributes = TRUE)
    L221.StubTech_en <- get_data(all_data, "L221.StubTech_en", strip_attributes = TRUE)
    L221.StubTechShrwt_bioOil <- get_data(all_data, "L221.StubTechShrwt_bioOil", strip_attributes = TRUE)
    L221.StubTechFractProd_en <- get_data(all_data, "L221.StubTechFractProd_en", strip_attributes = TRUE)
    L221.StubTechFractSecOut_en <- get_data(all_data, "L221.StubTechFractSecOut_en", strip_attributes = TRUE)
    L221.Rsrc_en <- get_data(all_data, "L221.Rsrc_en", strip_attributes = TRUE)
    L221.RsrcPrice_en <- get_data(all_data, "L221.RsrcPrice_en", strip_attributes = TRUE)
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en", strip_attributes = TRUE)
    L226.GlobalTechEff_en <- get_data(all_data, "L226.GlobalTechEff_en", strip_attributes = TRUE)
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en", strip_attributes = TRUE)
    L222.StubTechMarket_en_USA <- get_data(all_data, "L222.StubTechMarket_en_USA", strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_USA <- get_data(all_data, "L2234.StubTechMarket_elecS_USA", strip_attributes = TRUE)
    L232.StubTechMarket_ind_USA <- get_data(all_data, "L232.StubTechMarket_ind_USA", strip_attributes = TRUE)
    L2321.StubTechMarket_cement_USA <- get_data(all_data, "L2321.StubTechMarket_cement_USA", strip_attributes = TRUE)
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld", strip_attributes = TRUE)
    L221.StubTechFractCalPrice_en <- get_data(all_data, "L221.StubTechFractCalPrice_en", strip_attributes = TRUE)
    L221.StubTechCalInput_bioOil <- get_data(all_data, "L221.StubTechCalInput_bioOil", strip_attributes = TRUE)
    L221.StubTechInterp_bioOil <- get_data(all_data, "L221.StubTechInterp_bioOil", strip_attributes = TRUE)

    # ===================================================
    # Data Processing

    # Supply Sectors and Subsectors
    # Deleting USA-level biomass sectors
    # Not deleting USA regional biomass because gas processing & H2 central production
    # still both occur at the USA level and consume USA regional biomass
    A28.sector %>%
      filter(supplysector != "regional biomass") %>%
      mutate(region = gcam.USA_REGION) %>%
      select(region, supplysector) -> L2261.DeleteSupplysector_bio_USA

    # Supply sector information for state-level biomass sectors
    L2261.USA_biomass_sectors <- unique(A28.sector$supplysector)

    A21.sector %>%
      bind_rows(A26.sector) %>%
      filter(supplysector %in% L2261.USA_biomass_sectors) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$Supplysector, logit.type) -> L2261.Supplysector_bio_USA

    # Subsector shareweights of state-level biomass sectors
    L2261.Supplysector_bio_USA %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$SubsectorShrwtFllt) -> L2261.SubsectorShrwtFllt_bio_USA

    # Subsector shareweight interpolations for state-level biomass sectors
    L221.SubsectorInterp_en %>%
      bind_rows(L226.SubsectorInterp_en) %>%
      filter(region == gcam.USA_REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$SubsectorInterp) -> L2261.SubsectorInterp_bio_USA

    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L2261.SubsectorShrwtFllt_bio_USA %>%
      select(LEVEL2_DATA_NAMES$Subsector) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = gcamusa.DEFAULT_LOGIT_TYPE) -> L2261.SubsectorLogit_bio_USA


    # Stub-technologies for biomass sectors that consume from global markets
    # Stub-technologies for state-level biomass sector
    L221.StubTech_en %>%
      filter(region == gcam.USA_REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      # NOTE: can't use stub technology for state-level regional biomass sectors
      # because they would inherit the wrong energy-inputs
      filter(stub.technology != "regional biomass") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$StubTech) -> L2261.StubTech_bio_USA

    # Technology inputs & markets of state-level biomass sectors
    #kbn 2020-
    L221.GlobalTechCoef_en %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      semi_join(L2261.StubTech_bio_USA, by = c("supplysector", "subsector", "stub.technology")) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES$StubTechMarket) -> L2261.StubTechMarket_bio_USA

    # kbn 2020
    L221.StubTechCoef_bioOil %>%
      filter(region=="USA") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(market.name = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES$StubTechCoef)->L2261.StubTechCoef_bioOil_USA

    # Technology share-weights of state-level regional biomassOil sectors
    # kbn 2020-03-29 No longer writing out share-weights for some regions. But we need this for GCAM-USA
    L221.StubTechShrwt_bioOil %>%
      # mutate that does nothing to ensure prior metadata is dropped
      mutate(region = region) %>%
      filter(region == gcam.USA_REGION) -> L2261.StubTechShrwt_rbO_USA

    if(nrow(L2261.StubTechShrwt_rbO_USA) > 0) {
      L2261.StubTechShrwt_rbO_USA %>%
        select(-region) %>%
        repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
        select(LEVEL2_DATA_NAMES$StubTechYr, share.weight) -> L2261.StubTechShrwt_rbO_USA
    }

    # Secondary (feed) outputs of global technologies for upstream (bio)energy
    # NOTE: secondary outputs are only considered in future time periods
    # NOTE: secondary outputs are only written for the regions/technologies where applicable,
    # so the global tech database can not be used
    L221.StubTechFractSecOut_en %>%
      filter(region == gcam.USA_REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$StubTechFractSecOut) -> L2261.StubTechFractSecOut_bio_USA

    # Cost curve points for producing secondary output feedcrops
    L221.StubTechFractProd_en %>%
      filter(region == gcam.USA_REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$StubTechYr, fractional.secondary.output, price, fraction.produced) -> L2261.StubTechFractProd_bio_USA

    L221.StubTechFractCalPrice_en %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$StubTechFractCalPrice) -> L2261.StubTechFractCalPrice_bio_USA

    calibrated_techs %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by=c("minicam.energy.input" = "supplysector")) %>%
      select(sector, minicam.energy.input) %>%
      # filter for biomass energy only
      inner_join(L122.out_EJ_state_refining_F, c("sector")) %>%
      #filter(value != 0) %>%
      select(-sector, -fuel) %>%
      group_by(minicam.energy.input, year) %>%
      mutate(share = value / sum(value),
             share = if_else(is.na(share), 0, share)) %>%
      ungroup() %>%
      rename(supplysector = minicam.energy.input) %>%
      # expand L221.StubTechCalInput_bioOil to the states, adjust calibrated.value by the state shares
      left_join(filter(L221.StubTechCalInput_bioOil, region == gcam.USA_REGION), ., by=c("supplysector", "year")) %>%
      mutate(calibrated.value = share * calibrated.value,
             region = state) %>%
      select(LEVEL2_DATA_NAMES$StubTechCalInput) -> L2261.StubTechCalInput_bio_USA

    L221.StubTechInterp_bioOil %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$StubTechInterp) -> L2261.StubTechInterp_bio_USA

    # Connecting state-level DDGS & feedcakes secondary outputs to USA sector
    # depletable resource info for state-level DDGS & feedcake secondary outputs
    L221.Rsrc_en %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$Rsrc) -> L2261.Rsrc_DDGS_USA

    # Depletable resource prices for state-level DDGS & feedcake secondary outputs
    L221.RsrcPrice_en %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$RsrcPrice) -> L2261.RsrcPrice_DDGS_USA


    # Technologies for state-level regional biomass sectors, which consume "regional biomass" from USA regional biomass sector
    # NOTE: can't use stub technology for state-level regional biomass sectors because they would inherit the wrong energy-inputs
    L221.StubTech_en %>%
      filter(region == gcam.USA_REGION,
             stub.technology == "regional biomass") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      rename(technology = stub.technology) %>%
      select(LEVEL2_DATA_NAMES$Tech) -> L2261.Tech_rbm_USA

    # Technology shareweights of state-level regional biomass sectors
    L2261.Tech_rbm_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$TechYr, share.weight) -> L2261.TechShrwt_rbm_USA

    # Technology input & efficiencies of state-level regional biomass sectors
    # State-level regional biomass sectors consume "regional biomass" from USA regional biomass sector
    # This is done to avoid any potential conflict with the bio trade feature
    L2261.TechShrwt_rbm_USA %>%
      select(LEVEL2_DATA_NAMES$TechYr) %>%
      mutate(minicam.energy.input = "regional biomass",
             coefficient = gcamusa.DEFAULT_COEFFICIENT,
             market.name = gcam.USA_REGION) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) -> L2261.TechCoef_rbm_USA


    # Technologies for delivered biomass sectors, which consume from state-level markets
    # NOTE: can't use stub technology for delivered biomass sectors because they would inherit the wrong energy-inputs
    # Technologies for state-level delivered biomass sector
    L226.GlobalTechEff_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$Tech) %>%
      distinct() -> L2261.Tech_dbm_USA

    # Technology shareweights of state-level delivered biomass sectors
    L2261.Tech_dbm_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$TechYr, share.weight) -> L2261.TechShrwt_dbm_USA

    # Technology efficiencies of state-level delivered biomass sectors
    L226.GlobalTechEff_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES$TechEff) -> L2261.TechEff_dbm_USA

    # Technology costs for state-level delivered biomass sectors
    L226.GlobalTechCost_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name)  %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(LEVEL2_DATA_NAMES$TechCost) -> L2261.TechCost_dbm_USA


    # Carbon coefficients for state-level biomass sectors
    L202.CarbonCoef %>%
      filter(region == gcam.USA_REGION,
             PrimaryFuelCO2Coef.name %in% c(L2261.USA_biomass_sectors)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L2261.CarbonCoef_bio_USA

    # Adjusting the carbon coefficient for USA regional biomass
    # This is necessary because, in order to ensure compatibility with the
    # bio trade and negative emissions budget features, US states can't consume biomass
    # from the global market, but rather consume regional biomass from the USA market.
    # In order to associate the sequestered carbon embedded in biomass feedstocks with
    # the state in which they're consumed (rather than the USA region), we set the
    # USA regional biomass carbon coefficient to zero
    L202.CarbonCoef %>%
      filter(region == gcam.USA_REGION,
             PrimaryFuelCO2Coef.name == "regional biomass") %>%
      mutate(PrimaryFuelCO2Coef = 0) %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L2261.CarbonCoef_rbm_USA

    L2261.CarbonCoef_rbm_USA %>%
      bind_rows(L2261.CarbonCoef_bio_USA) -> L2261.CarbonCoef_bio_USA


    # Update minicam-energy-inputs for technologies that consume biomass
    set_state_biomass_markets <- function( data ){
      data_new <- data %>%
        filter(region %in% gcamusa.STATES,
               minicam.energy.input %in% L2261.USA_biomass_sectors) %>%
        mutate(market.name  = region)
      return( data_new)
    }

    # Energy Transformation (Refining)
    L2261.StubTechMarket_en_USA <- set_state_biomass_markets(L222.StubTechMarket_en_USA)

    # Electricity (load segments)
    L2261.StubTechMarket_elecS_USA <- set_state_biomass_markets(L2234.StubTechMarket_elecS_USA)

    # Industry
    L2261.StubTechMarket_ind_USA <- set_state_biomass_markets(L232.StubTechMarket_ind_USA)

    # Cement
    L2261.StubTechMarket_cement_USA <- set_state_biomass_markets(L2321.StubTechMarket_cement_USA)

    # Buildings
    L2261.StubTechMarket_bld_USA <- set_state_biomass_markets(L244.StubTechMarket_bld)


    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs

    # Define unique states and basins that have access to seawater that will
    # allow for seawate cooling

    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    add_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology) -> data_new

      data_new %>% filter(grepl(gcamusa.WATER_TYPE_SEAWATER,technology)) %>% filter((region %in% seawater_states_basins)) %>%
        bind_rows(data_new %>% filter(!grepl(gcamusa.WATER_TYPE_SEAWATER,technology))) %>%
        arrange(region,year) -> data_new
      return(data_new)
    }
    L2261.StubTechMarket_elecS_USA <- add_cooling_techs(L2261.StubTechMarket_elecS_USA)


    # ===================================================
    # Produce outputs

    L2261.DeleteSupplysector_bio_USA %>%
      add_title("Delete USA-level Biomass Sectors") %>%
      add_units("NA") %>%
      add_comments("USA regional biomass not deleted") %>%
      add_comments("USA gas processing & H2 central production sectors still consume USA regional biomass") %>%
      add_legacy_name("L2261.DeleteSupplysector_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector") ->
      L2261.DeleteSupplysector_bio_USA

    L2261.Supplysector_bio_USA %>%
      add_title("State-level Biomass Supply Sector Information") %>%
      add_units("unitless") %>%
      add_comments("Supply sector information for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.Supplysector_bio_USA") %>%
      add_precursors("energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-usa/A28.sector") ->
      L2261.Supplysector_bio_USA

    L2261.SubsectorShrwtFllt_bio_USA %>%
      add_title("Subsector Shareweights for State-level Biomass Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Subsector shareweights for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.SubsectorShrwtFllt_bio_USA") %>%
      same_precursors_as("L2261.Supplysector_bio_USA") ->
      L2261.SubsectorShrwtFllt_bio_USA

    L2261.SubsectorInterp_bio_USA %>%
      add_title("Subsector Shareweights for State-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweight interpolations for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.SubsectorInterp_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.SubsectorInterp_en",
                     "L226.SubsectorInterp_en") ->
      L2261.SubsectorInterp_bio_USA

    L2261.SubsectorLogit_bio_USA %>%
      add_title("Subsector Logits for State-level Biomass Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Subsector logits for state-level biomass supply sectors") %>%
      add_comments("There is only one tech per subsector so the logit choice does not matter") %>%
      add_legacy_name("L2261.SubsectorLogit_bio_USA") %>%
      same_precursors_as("L2261.Supplysector_bio_USA") ->
      L2261.SubsectorLogit_bio_USA

    L2261.StubTech_bio_USA %>%
      add_title("State-level Biomass Supply Sector Stub Technologies") %>%
      add_units("NA") %>%
      add_comments("Stub-technologies for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.StubTech_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.StubTech_en") ->
      L2261.StubTech_bio_USA

    L2261.StubTechMarket_bio_USA %>%
      add_title("Technology Market Information for State-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Technology inputs and markets for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.StubTechMarket_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.GlobalTechCoef_en",
                     "L221.StubTech_en") ->
      L2261.StubTechMarket_bio_USA

    L2261.StubTechCoef_bioOil_USA %>%
      add_title("Technology coefficients specific crops at State-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Technology coefficients specific crops at State-level Biomass Supply Sectors") %>%
      add_legacy_name("L2261.StubTechCoef_bioOil_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.StubTechCoef_bioOil") ->L2261.StubTechCoef_bioOil_USA



    L2261.StubTechShrwt_rbO_USA %>%
      add_title("Technology Shareweights for State-level Regional Biomass Oil Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology share-weights for state-level regional biomassOil supply sectors") %>%
      add_legacy_name("L2261.StubTechShrwt_rbO_USA") %>%
      add_precursors("L221.StubTechShrwt_bioOil") ->
      L2261.StubTechShrwt_rbO_USA

    L2261.StubTechFractSecOut_bio_USA %>%
      add_title("Secondary Feed Outputs of State-level Biomass Supply Sectors") %>%
      add_units("fractions") %>%
      add_comments("Secondary output (DDGS and feedcakes) generated from corn and biomassOil only") %>%
      add_comments("Secondary outputs are only considered in future time periods") %>%
      add_legacy_name("L2261.StubTechFractSecOut_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.StubTechFractSecOut_en") ->
      L2261.StubTechFractSecOut_bio_USA

    L2261.StubTechFractProd_bio_USA %>%
      add_title("Production Information for State-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("1975$ (price); fraction") %>%
      add_comments("Cost curve points (prices and production fraction) for producing secondary output feedcrops") %>%
      add_comments("Secondary output (DDGS and feedcakes) generated from corn and biomassOil only") %>%
      add_legacy_name("L2261.StubTechFractProd_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L221.StubTechFractProd_en") ->
      L2261.StubTechFractProd_bio_USA

    L2261.StubTechFractCalPrice_bio_USA %>%
      add_title("Calibrated historical price for DDGS") %>%
      add_units("1975$/kg") %>%
      add_comments("Value only relevant for share-weight calculation") %>%
      add_precursors("L221.StubTechFractCalPrice_en") ->
      L2261.StubTechFractCalPrice_bio_USA

    L2261.StubTechCalInput_bio_USA %>%
      add_title("Calibrated output of biomassOil by feedstock type") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibration is necessary to allow regions to have multiple biomassOil feedstocks") %>%
      add_precursors("energy/calibrated_techs",
                     "gcam-usa/A28.sector",
                     "L122.out_EJ_state_refining_F",
                     "L221.StubTechCalInput_bioOil") ->
      L2261.StubTechCalInput_bio_USA

    L2261.StubTechInterp_bio_USA %>%
      add_title("biomassOil technology (feedstock type) shareweight interpolation") %>%
      add_units("unitless") %>%
      add_comments("Regions with multiple feedstocks in the base year have their share-weights passed forward") %>%
      add_precursors("L221.StubTechInterp_bioOil") ->
      L2261.StubTechInterp_bio_USA

    L2261.Rsrc_DDGS_USA %>%
      add_title("Depletable Resource Information for State-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("NA") %>%
      add_comments("Depletable resource info for state-level DDGS & feedcake secondary outputs") %>%
      add_legacy_name("L2261.Rsrc_DDGS_USA") %>%
      add_precursors("L221.Rsrc_en") ->
      L2261.Rsrc_DDGS_USA

    L2261.RsrcPrice_DDGS_USA %>%
      add_title("Depletable Resource Information for State-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("1975$/kg") %>%
      add_comments("Depletable resource prices for state-level DDGS & feedcake secondary outputs") %>%
      add_legacy_name("L2261.RsrcPrice_DDGS_USA") %>%
      add_precursors("L221.RsrcPrice_en") ->
      L2261.RsrcPrice_DDGS_USA

    L2261.Tech_rbm_USA %>%
      add_title("State-level Regional Biomass Technologies") %>%
      add_units("NA") %>%
      add_comments("Technologies for state-level regional biomass sector") %>%
      add_comments("Can't use stub-technology tag for regional biomass sectors because they would inherit the wrong energy-inputs") %>%
      add_precursors("L221.StubTech_en") ->
    L2261.Tech_rbm_USA

    L2261.TechShrwt_rbm_USA%>%
      add_title("Technology Shareweights for State-level Regional Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology shareweights for state-level regional biomass sectors") %>%
      same_precursors_as("L2261.Tech_rbm_USA") ->
    L2261.TechShrwt_rbm_USA

    L2261.TechCoef_rbm_USA%>%
      add_title("Technology Market Info for State-level Regional Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology market info and coefficients for state-level regional biomass sectors") %>%
      same_precursors_as("L2261.Tech_rbm_USA") ->
    L2261.TechCoef_rbm_USA

    L2261.Tech_dbm_USA %>%
      add_title("State-level Delivered Biomass Technologies") %>%
      add_units("NA") %>%
      add_comments("Technologies for state-level delivered biomass sector") %>%
      add_comments("Can't use stub-technology tag for delivered biomass sectors because they would inherit the wrong energy-inputs") %>%
      add_legacy_name("L2261.Tech_dbm_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L226.GlobalTechEff_en") ->
      L2261.Tech_dbm_USA

    L2261.TechShrwt_dbm_USA %>%
      add_title("Technology Shareweights for State-level Delivered Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology shareweights for state-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechShrwt_dbm_USA") %>%
      same_precursors_as("L2261.Tech_dbm_USA") ->
      L2261.TechShrwt_dbm_USA

    L2261.TechEff_dbm_USA %>%
      add_title("Technology Efficiencies for State-level Delivered Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology efficiencies for state-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechEff_dbm_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L226.GlobalTechEff_en") ->
      L2261.TechEff_dbm_USA

    L2261.TechCost_dbm_USA %>%
      add_title("Technology Costs for State-level Delivered Biomass Sectors") %>%
      add_units("1975$/GJ") %>%
      add_comments("Technology costs for state-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechCost_dbm_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L226.GlobalTechCost_en") ->
      L2261.TechCost_dbm_USA

    L2261.CarbonCoef_bio_USA %>%
      add_title("Primary Energy CO2 Coefficient") %>%
      add_units("kgC/GJ") %>%
      add_comments("Carbon coefficients for state-level biomass sectors") %>%
      add_legacy_name("L2261.CarbonCoef_bio_USA") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L202.CarbonCoef") ->
      L2261.CarbonCoef_bio_USA

    L2261.StubTechMarket_en_USA %>%
      add_title("Market Information for State-level Refining Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to state-level refining sectors") %>%
      add_comments("Now consuming from state-level biomass supply sectors") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L222.StubTechMarket_en_USA") ->
      L2261.StubTechMarket_en_USA

    L2261.StubTechMarket_elecS_USA %>%
      add_title("Market Information for State-level (multiple load segment) Electricity Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to state-level electricity sectors") %>%
      add_comments("Now consuming from state-level biomass supply sectors") %>%
      add_precursors("gcam-usa/A28.sector",
                     'gcam-usa/A23.elecS_tech_mapping_cool',
                     "gcam-usa/usa_seawater_states_basins",
                     "L2234.StubTechMarket_elecS_USA") ->
      L2261.StubTechMarket_elecS_USA

    L2261.StubTechMarket_ind_USA %>%
      add_title("Market Information for State-level Industrial Energy Use Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to state-level industrial energy use sectors") %>%
      add_comments("Now consuming from state-level biomass supply sectors") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L232.StubTechMarket_ind_USA") ->
      L2261.StubTechMarket_ind_USA

    L2261.StubTechMarket_cement_USA %>%
      add_title("Market Information for State-level Process Heat Cement Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to state-level process heat cement sectors") %>%
      add_comments("Now consuming from state-level biomass supply sectors") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L2321.StubTechMarket_cement_USA") ->
      L2261.StubTechMarket_cement_USA

    L2261.StubTechMarket_bld_USA %>%
      add_title("Market Information for State-level Building Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to state-level building sectors") %>%
      add_comments("Now consuming from state-level biomass supply sectors") %>%
      add_precursors("gcam-usa/A28.sector",
                     "L244.StubTechMarket_bld") ->
      L2261.StubTechMarket_bld_USA


    return_data(L2261.DeleteSupplysector_bio_USA,
                L2261.Supplysector_bio_USA,
                L2261.SubsectorShrwtFllt_bio_USA,
                L2261.SubsectorInterp_bio_USA,
                L2261.SubsectorLogit_bio_USA,
                L2261.StubTech_bio_USA,
                L2261.StubTechMarket_bio_USA,
                L2261.StubTechCoef_bioOil_USA,
                L2261.StubTechShrwt_rbO_USA,
                L2261.StubTechFractSecOut_bio_USA,
                L2261.StubTechFractProd_bio_USA,
                L2261.StubTechFractCalPrice_bio_USA,
                L2261.StubTechCalInput_bio_USA,
                L2261.StubTechInterp_bio_USA,
                L2261.Rsrc_DDGS_USA,
                L2261.RsrcPrice_DDGS_USA,
                L2261.Tech_rbm_USA,
                L2261.TechShrwt_rbm_USA,
                L2261.TechCoef_rbm_USA,
                L2261.Tech_dbm_USA,
                L2261.TechShrwt_dbm_USA,
                L2261.TechEff_dbm_USA,
                L2261.TechCost_dbm_USA,
                L2261.CarbonCoef_bio_USA,
                L2261.StubTechMarket_en_USA,
                L2261.StubTechMarket_elecS_USA,
                L2261.StubTechMarket_ind_USA,
                L2261.StubTechMarket_cement_USA,
                L2261.StubTechMarket_bld_USA)
  } else {
    stop("Unknown command")
  }
}
