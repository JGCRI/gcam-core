#' module_energy_LA122.gasproc_refining
#'
#' This chunk creates gasproc, oil refining and crops inputs, outputs and IO "input-output" coefficients for refining.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.out_EJ_R_gasproc_F_Yh}, \code{L122.in_EJ_R_gasproc_F_Yh}, \code{L122.IO_R_oilrefining_F_Yh}, \code{L122.out_EJ_R_refining_F_Yh}, \code{L122.in_EJ_R_refining_F_Yh}, \code{L122.in_Mt_R_C_Yh}. The corresponding file in the
#' original data system was \code{LA122.gasproc_refining.R} (energy level1).
#' @details This chunk creates gasproc, oil refining and crops inputs, outputs and IO "input-output" coefficients for refining.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author FF, May 2017


module_energy_LA122.gasproc_refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A22.globaltech_coef",
             FILE = "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "temp-data-inject/L121.in_EJ_R_unoil_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.out_EJ_R_gasproc_F_Yh",
             "L122.in_EJ_R_gasproc_F_Yh",
             "L122.IO_R_oilrefining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L122.in_Mt_R_C_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef")
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh")
    L121.in_EJ_R_unoil_F_Yh <- get_data(all_data, "temp-data-inject/L121.in_EJ_R_unoil_F_Yh")

    # Data in long format and removing "X" year
    L1011.en_bal_EJ_R_Si_Fi_Yh%>%
    gather(year,value,-GCAM_region_ID,-fuel,-sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) -> L1011.en_bal_EJ_R_Si_Fi_Yh

    L121.in_EJ_R_unoil_F_Yh%>%
      gather(year,value,-GCAM_region_ID,-fuel,-sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) -> L121.in_EJ_R_unoil_F_Yh

    # ===================================================
    # Perform computations: Will start with refining

    # Most of the technologies (in calibrated_techs.csv) have inputs from outputs based on exogenous coefficients (A22.globaltech_coef).
    # Extracting those coefficients and interpolating them to 2010.
    # Remove "x" year
    A22.globaltech_coef %>%
      semi_join(select(calibrated_techs, supplysector, subsector, technology), by = c("supplysector", "subsector", "technology")) %>%
      left_join_error_no_match(select(calibrated_techs, supplysector, subsector, technology, minicam.energy.input, sector, fuel), by = c("supplysector", "subsector", "technology","minicam.energy.input")) %>%
      gather(hist_year, value, -supplysector, -subsector, -technology, -minicam.energy.input, -sector, -fuel) %>%
      mutate(hist_year = as.numeric(hist_year)) %>%
      filter(hist_year == "1971") %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      select(-hist_year) -> L122.globaltech_coef

    # BIOMASS LIQUIDS: Ethanol and biodiesel output are equal to regional TPES
    # Creating fuel constant for biomass liquids that will be used to filter biofuels from L1011.en_bal_EJ_R_Si_Fi_Yh and to create L122.out_EJ_R_biofuel_Yh
    BIOMASS_LIQUIDS <- c("refined biofuels_ethanol", "refined biofuels_FT")
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES") %>%
      filter(fuel %in% BIOMASS_LIQUIDS) -> L122.out_EJ_R_biofuel_Yh

    # Create ethanol sector for biomass from A_regions. This will be "row binded" with biodiesel sector below.
    L122.out_EJ_R_biofuel_Yh %>%
      filter(fuel == "refined biofuels_ethanol") %>%
      left_join_error_no_match(select(A_regions, GCAM_region_ID, ethanol), by = "GCAM_region_ID") %>%
      select(-sector) %>%
      rename(sector = ethanol) -> BIOMASS_LIQUIDS_Ethanol

    # Biodiesel sector and then binded with ethanol sector to consolidate everything into L122.out_EJ_R_biofuel_Yh
    L122.out_EJ_R_biofuel_Yh %>%
      filter(fuel == "refined biofuels_FT") %>%
      left_join_error_no_match(select(A_regions, GCAM_region_ID, biodiesel), by = "GCAM_region_ID") %>%
      select(-sector) %>%
      rename(sector = biodiesel) %>%
      bind_rows(BIOMASS_LIQUIDS_Ethanol) -> L122.out_EJ_R_biofuel_Yh

    # Adding fuels to appropiate sectors and regions in L122.out_EJ_R_biofuel_Yh from calibrated_techs
    calibrated_techs %>%
      select(sector, fuel) %>%
      distinct(sector, .keep_all = TRUE) %>%
      right_join(L122.out_EJ_R_biofuel_Yh, by = "sector") %>%
      select(GCAM_region_ID, sector, fuel = fuel.x, year, value) -> L122.out_EJ_R_biofuel_Yh


    # Inputs to biofuel production are region-specific
    # Because some have multiple inputs, repeat coefficient table by number of regions and then subset only the applicable combinations
    L122.globaltech_coef %>%
      filter(sector %in% L122.out_EJ_R_biofuel_Yh$sector) %>%
      repeat_add_columns(tibble::tibble(GCAM_region_ID = A_regions$GCAM_region_ID)) -> L122.biofuel_coef_repR

    # subset L122.biofuel_coef_repR based on A_regions by region and sector (ethanol)
    L122.biofuel_coef_repR %>%
    # Using semi_join to keep the sector = ethanol in L122.biofuel_coef_repR based on A_regions
      semi_join(select(A_regions, GCAM_region_ID, sector = ethanol), by = c("sector", "GCAM_region_ID")) -> L122.biofuel_coef_repR_Ethanol

    # subset L122.biofuel_coef_repR based on A_regions by region and sector (biodisiel). Then adding ethanol sector (L122.biofuel_coef_repR_Ethanol)
    L122.biofuel_coef_repR %>%
      # Using semi_join to keep the sector = biodiesel in L122.biofuel_coef_repR based on A_regions
      semi_join(select(A_regions, GCAM_region_ID, sector = biodiesel), by = c("sector", "GCAM_region_ID")) %>%
      bind_rows(L122.biofuel_coef_repR_Ethanol) -> L122.biofuel_coef_R

    # Build table of inputs to biofuel production (IO coefs times output)
    L122.biofuel_coef_R %>%
      rename(fuelInput = fuel, valueInput = value) %>%
      left_join(L122.out_EJ_R_biofuel_Yh, by = c("GCAM_region_ID", "sector","year")) %>%
      mutate(value  = valueInput * value) %>%
      select(-valueInput, -fuel) %>%
      rename(fuel = fuelInput) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L122.in_EJ_R_biofuel_F_Yh

    # GAS AND COAL TO LIQUIDS
    # Create L122.out_EJ_R_gtlctl_Yh from L1011.en_bal_EJ_R_Si_Fi_Yh for gas to liquids (gtl) and coal to liquids (ctl) sectors
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_gtl" | sector == "out_ctl") %>%
      mutate(sector = if_else(sector=="out_gtl", "gtl", "ctl")) -> L122.out_EJ_R_gtlctl_Yh

    # GTL and CTL inputs (L122.in_EJ_R_gtlctl_F_Yh): derived as output times exogenous input-output coefficients
    # Interpolate gas processing IO coefs to all historical years and match in the fuel name
    L122.globaltech_coef %>%
      filter(sector == "gtl"| sector == "ctl") -> L122.gtlctl_coef

    L122.out_EJ_R_gtlctl_Yh %>%
      rename(valueInput = value) %>%
      left_join(L122.gtlctl_coef, by = c("sector", "fuel", "year")) %>%
      mutate(value = valueInput * value) %>%
      select(-valueInput) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L122.in_EJ_R_gtlctl_F_Yh

    # CRUDE OIL REFINING
    # Copied from original text:
    # NOTE*1: This is complicated. The outputs of CTL and GTL have the same fuel name as the output of oil refining,
    # so need to be deducted from TPES in order to calculate regional output of oil refining.
    # In contrast, biofuels are assigned different names, so they are not in the TPES of refined liquids.

    #creating tibble with appropiate sectir and fuels for oil refining (output) for L122.out_EJ_R_oilrefining_Yh and adding historical years
    tibble(GCAM_region_ID = GCAM_region_names$GCAM_region_ID, sector = "oil refining", fuel = "oil" )%>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) -> L122.out_EJ_R_oilrefining_Yh

    # Creating en_bal_TPES_OIL, en_bal_oil, ctl_OIL, and gtlctl_oil to adjust the outputs of CTL and GTL given the same fuel names of the oil refininf outputs (as mentioned in the note above)
    # Getting output for refined liquids for oil refining (TPES) sector
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES") %>%
      filter(fuel == "refined liquids") %>%
      select(GCAM_region_ID,sector, year, value_en_bal_TPES = value) %>%
      mutate(sector = "oil refining") -> en_bal_TPES_OIL

    # Getting output for refined liquids for net_oil refining sector
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "net_oil refining") %>%
      filter(fuel == "refined liquids") %>%
      select(GCAM_region_ID, sector, year, value_en_bal_net_oil = value) %>%
      mutate(sector = "oil refining") %>%
      left_join_error_no_match(en_bal_TPES_OIL, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value_en_bal = value_en_bal_TPES - value_en_bal_net_oil) %>%
      select(-value_en_bal_TPES, -value_en_bal_net_oil) -> en_bal_oil

    # Getting output for coal for CTL sector
    L122.out_EJ_R_gtlctl_Yh %>%
      filter(sector == "ctl", fuel == "coal") %>%
      select(GCAM_region_ID, sector, year, value_ctl_oil = value) %>%
      mutate(sector = "oil refining") -> ctl_OIL

    # Getting output for coal for GTL sector
    L122.out_EJ_R_gtlctl_Yh %>%
      filter(sector == "gtl", fuel == "gas") %>%
      select(GCAM_region_ID, sector, year, value_gtl_oil = value) %>%
      mutate(sector = "oil refining") %>%
      left_join_error_no_match(ctl_OIL, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value_gtlctl = value_ctl_oil + value_gtl_oil) %>%
      select(-value_ctl_oil, -value_gtl_oil) -> gtlctl_oil

    # Final adjustments to fuel outputs for CTL and GTL to tackle the Note (NOTE*1) made above
    # left_join_error_no_match could be used here since it has some problems with the timeshifting test
    L122.out_EJ_R_oilrefining_Yh %>%
      left_join(en_bal_oil, by = c("GCAM_region_ID", "sector", "year")) %>%
      left_join(gtlctl_oil, by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = value_en_bal - value_gtlctl) %>%
      select(-value_en_bal, -value_gtlctl) -> L122.out_EJ_R_oilrefining_Yh

    # Oil refining: input of oil is equal to TPES, and input of other fuels is from net refinery energy use
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "net_oil refining") %>%
      filter(fuel == "refined liquids") %>%
      left_join_error_no_match(select(filter(L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES", fuel == "refined liquids"), -sector), by = c("GCAM_region_ID", "fuel", "year")) %>%
      select(-value.x) %>%
      rename(value = value.y) %>%
      bind_rows(filter(L1011.en_bal_EJ_R_Si_Fi_Yh,sector == "net_oil refining", fuel!= "refined liquids")) %>%
      mutate(sector = "oil refining") %>%
      mutate(fuel = if_else(fuel == "refined liquids", "oil", fuel)) -> L122.in_EJ_R_oilrefining_F_Yh


    # Calculating region- and fuel-specific coefficients of crude oil refining
    L122.in_EJ_R_oilrefining_F_Yh %>%
     left_join(select(L122.out_EJ_R_oilrefining_Yh, -fuel), by = c("GCAM_region_ID", "sector", "year")) %>%
      mutate(value = value.x/value.y) %>%
      select(-value.x, -value.y) -> L122.IO_R_oilrefining_F_Yh

    # Combine(RBIND) all calibrated refinery input and output tables
    bind_rows(L122.out_EJ_R_oilrefining_Yh, L122.out_EJ_R_gtlctl_Yh, L122.out_EJ_R_biofuel_Yh) -> L122.out_EJ_R_refining_F_Yh
    bind_rows(L122.in_EJ_R_oilrefining_F_Yh, L122.in_EJ_R_gtlctl_F_Yh, L122.in_EJ_R_biofuel_F_Yh) -> L122.in_EJ_R_refining_F_Yh


    # Extra final step: calculate and create the derived crop inputs to the various first-generation biofuel technologies, for the AGLU processing
    L122.in_EJ_R_biofuel_F_Yh %>%
      left_join(select(calibrated_techs, sector, fuel, passthrough.sector = minicam.energy.input), by = c("sector", "fuel")) %>%
      filter(passthrough.sector %in% A21.globaltech_coef$supplysector) -> L122.in_EJ_R_1stgenbio_F_Yh

    A21.globaltech_coef %>%
      select(passthrough.sector = supplysector, minicam.energy.input) %>%
      distinct(passthrough.sector, .keep_all = TRUE) %>%
      right_join(L122.in_EJ_R_1stgenbio_F_Yh, by = "passthrough.sector") %>%
      rename(GCAM_commodity = minicam.energy.input) -> L122.in_EJ_R_1stgenbio_F_Yh


    # crop inputs to biodiesel are region-specific
    L122.in_EJ_R_1stgenbio_F_Yh %>%
      filter(passthrough.sector == "regional biomassOil") %>%
      left_join(select(A_regions, GCAM_region_ID, biomassOil_tech), by = "GCAM_region_ID") %>%
      select(-GCAM_commodity) %>%
      rename(GCAM_commodity = biomassOil_tech) %>%
      bind_rows(filter(L122.in_EJ_R_1stgenbio_F_Yh, passthrough.sector != "regional biomassOil")) %>%
      filter(GCAM_commodity %in% FAO_ag_items_PRODSTAT$GCAM_commodity) %>%
      select(GCAM_region_ID, sector, fuel, passthrough.sector, GCAM_commodity, year, value) -> L122.in_EJ_R_1stgenbio_F_Yh

    # Interpolate coefs (using repeat_add_columns) to all historical periods, and then multiply by the input quantities
    # Filter 1971 since the value associated to this year is the same till 2100, but all historical years are missing. Therefore filter
    # 1971 and the repeat the corresponding value (repeat_add_columns) for historical years
    A21.globaltech_coef %>%
      gather(hist_year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      filter(hist_year == "1971") %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS) ) %>%
      select(-hist_year) -> L121.globaltech_coef

    # Multiplying by input quantities by coefficients in L121.globaltech_coef
    L122.in_EJ_R_1stgenbio_F_Yh %>%
      select(GCAM_region_ID, GCAM_commodity, year, in_value = value) %>%
      left_join(select(L121.globaltech_coef, GCAM_commodity = minicam.energy.input, year, value), by = c("GCAM_commodity", "year")) %>%
      mutate(value = in_value * value) %>%
      select(-in_value) -> L122.in_Mt_R_C_Yh

    # GAS PROCESSING
    # Note: Gas processing input-output coefficients are exogenous
    # L122.gasproc_coef created by repeating 1971 coef (repeat_add_columns) for historical years based on A22.globaltech_coef.
    # Mathing with the correct technologies based on calibrated_techs
    A22.globaltech_coef %>%
      filter(supplysector == "gas processing") %>%
      gather(hist_year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
    # Filter 1971 since the value associated to this year is the same till 2100, but all historical years are missing. Therefore filter
    # 1971 and the repeat the corresponding value (repeat_add_columns) for historical years
      filter(hist_year == "1971") %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      select(-hist_year) %>%
      left_join_error_no_match(select(calibrated_techs, supplysector, subsector, technology, sector, fuel), by = c("supplysector", "subsector" ,"technology")) -> L122.gasproc_coef

    # Gas processing output from biomass gasification is equal to regional TPES
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES" , fuel == "gasified biomass") %>%
      mutate(sector = "gas processing", fuel = "biomass") -> L122.out_EJ_R_gasproc_bio_Yh

    # Gas processing output from coal gasification is calculated from the input of coal
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "in_gas processing", fuel == "coal") %>%
      mutate(sector = "gas processing") -> L122.in_EJ_R_gasproc_coal_Yh

    # Calulate output of gas (L122.out_EJ_R_gasproc_coal_Yh) based on coefficients and inputs.
    # Output gas is obtain by dividing the input gas by coefficients from L122.gasproc_coef (gas_coef)
    L122.in_EJ_R_gasproc_coal_Yh %>%
      left_join(select(L122.gasproc_coef, sector, fuel, gas_coef = value, year), by = c("sector", "fuel", "year")) %>%
      mutate(value = value/gas_coef) %>%
      select(-gas_coef) -> L122.out_EJ_R_gasproc_coal_Yh

    # Natural gas is equal to regional TPES minus upstream use of natural gas (e.g. GTL). Procedure and assumptiosn are explained below
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "TPES", fuel == "gas") %>%
      mutate(sector = "gas processing") -> L122.out_EJ_R_gasproc_gas_Yh

    # Note: The following code and their reason is given in  "NOTE2" (pasted from original code) below
    # NOTE2 (copied from original code): This is complicated. Several of the "upstream" energy users--in GCAM 3.0, unconventional oil production and gas-to-liquids--are assumed
    # to use natural gas upstream of the processing and T&D infrastructure. If this is the case, then these sectors' consumption of gas should not
    # be included in the gas processing and pipeline sectors. However this may be changed in the future, and for this reason the following
    # method is designed to work if future users re-set the energy inputs to these technologies from "regional natural gas" to "wholesale gas".

    # First, extract the input names to reg_nat_gas, gas_to_unconv_oil, gas_to_gtl
    calibrated_techs %>%
      filter(sector == "gas processing", fuel == "gas") %>%
      select(minicam.energy.input) -> reg_nat_gas_tibble
    reg_nat_gas_tibble$minicam.energy.input -> reg_nat_gas

    calibrated_techs %>%
      filter(sector == "unconventional oil production", fuel == "gas") %>%
      select(minicam.energy.input) -> gas_to_unconv_oil_tibble
    gas_to_unconv_oil_tibble$minicam.energy.input -> gas_to_unconv_oil

    calibrated_techs %>%
      filter(sector == "gtl", fuel == "gas") %>%
      select(minicam.energy.input) -> gas_to_gtl_tibble
    gas_to_gtl_tibble$minicam.energy.input -> gas_to_gtl

    # (Copied from original text)
    # Where the input names for unconv oil or GTL are equal to the name of the input to the gas processing sector,
    # subtract from the gas processing sector's production

    if (gas_to_unconv_oil == reg_nat_gas){
      L122.out_EJ_R_gasproc_gas_Yh %>%
        filter(GCAM_region_ID %in% L121.in_EJ_R_unoil_F_Yh$GCAM_region_ID) %>%
        left_join(select(L121.in_EJ_R_unoil_F_Yh, GCAM_region_ID, fuel, year, in_value = value), by = c("GCAM_region_ID", "fuel", "year")) %>%
        mutate(value = value - in_value) %>%
        select(-in_value)%>%
        bind_rows(filter(L122.out_EJ_R_gasproc_gas_Yh,!(GCAM_region_ID %in% L121.in_EJ_R_unoil_F_Yh$GCAM_region_ID))) -> L122.out_EJ_R_gasproc_gas_Yh
    }

    if (gas_to_gtl == reg_nat_gas){
      L122.out_EJ_R_gasproc_gas_Yh %>%
        left_join(select(L122.in_EJ_R_gtlctl_F_Yh, GCAM_region_ID, fuel, year, in_value = value), by = c("GCAM_region_ID", "fuel", "year")) %>%
        mutate(value = value - in_value)%>%
        select(-in_value) -> L122.out_EJ_R_gasproc_gas_Yh
    }

    # Combine (rbind) individual fuel tables
    bind_rows(L122.out_EJ_R_gasproc_gas_Yh, L122.out_EJ_R_gasproc_bio_Yh, L122.out_EJ_R_gasproc_coal_Yh) -> L122.out_EJ_R_gasproc_F_Yh

    # Calculate the inputs to gas processing
    L122.out_EJ_R_gasproc_F_Yh %>%
      left_join(select(L122.gasproc_coef, sector, fuel, gas_coef = value, year), by = c("sector", "fuel","year")) %>%
      mutate(value = value * gas_coef)%>%
      select(-gas_coef) -> L122.in_EJ_R_gasproc_F_Yh

    # Creating final outputs
    L122.out_EJ_R_gasproc_F_Yh %>%
      add_title("Outputs of gas processing by GCAM region / fuel / historical year ") %>%
      add_units("EJ") %>%
      add_comments("Combine individual fuel tables, including L122.out_EJ_R_gasproc_gas_Yh, L122.out_EJ_R_gasproc_bio_Yh, and L122.out_EJ_R_gasproc_coal_Yh") %>%
      add_legacy_name("L122.out_EJ_R_gasproc_F_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.out_EJ_R_gasproc_F_Yh

    L122.in_EJ_R_gasproc_F_Yh %>%
      add_title("Inputs to gas processing by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Based on outputs and gasproc coefficients. Obtained as L122.out_EJ_R_gasproc_F_Yh times L122.gasproc_coef ") %>%
      add_legacy_name("L122.in_EJ_R_gasproc_F_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.in_EJ_R_gasproc_F_Yh

    L122.IO_R_oilrefining_F_Yh %>%
      add_title("Oil refining input-output coefficients by GCAM region / fuel / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Obtained by caltulating the ratio inpout/output for oil refining from L122.in_EJ_R_oilrefining_F_Yh and L122.out_EJ_R_oilrefining_Yh") %>%
      add_legacy_name("L122.IO_R_oilrefining_F_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.IO_R_oilrefining_F_Yh

    L122.out_EJ_R_refining_F_Yh %>%
      add_title("Outputs of refining by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Combines all calibrated refinery output tables, including oil refining, gtl-ctl and biofuels ") %>%
      add_legacy_name("L122.out_EJ_R_refining_F_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.out_EJ_R_refining_F_Yh

    L122.in_EJ_R_refining_F_Yh %>%
      add_title("Inputs to refining by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Combines all calibrated refinery input tables, including oil refining, gtl-ctl and biofuels") %>%
      add_legacy_name("L122.in_EJ_R_refining_F_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.in_EJ_R_refining_F_Yh

    L122.in_Mt_R_C_Yh %>%
      add_title("Crop inputs to first-generation biofuel production by GCAM region / commodity / historical year") %>%
      add_units("Mt") %>%
      add_comments("Created by matching 1st generation bio with the global technologies coefficients for existing minicam energy inputs") %>%
      add_legacy_name("L122.in_Mt_R_C_Yh") %>%
      add_precursors("common/GCAM_region_names", "aglu/FAO_ag_items_PRODSTAT","energy/calibrated_techs",
                     "energy/A_regions","energy/A21.globaltech_coef","energy/A22.globaltech_coef","temp-data-inject/L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "temp-data-inject/L121.in_EJ_R_unoil_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L122.in_Mt_R_C_Yh

    return_data(L122.out_EJ_R_gasproc_F_Yh, L122.in_EJ_R_gasproc_F_Yh, L122.IO_R_oilrefining_F_Yh, L122.out_EJ_R_refining_F_Yh, L122.in_EJ_R_refining_F_Yh, L122.in_Mt_R_C_Yh)
  } else {
    stop("Unknown command")
  }
}


