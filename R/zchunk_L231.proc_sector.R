#' module_emissions_L231.proc_sector
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L231.UnlimitRsrc}, \code{L231.UnlimitRsrcPrice}, \code{L231.FinalDemand_urb}, \code{L231.SectorLogitTables[[ curr_table ]]$data}, \code{L231.Supplysector_urb_ind}, \code{L231.SubsectorLogitTables[[ curr_table ]]$data}, \code{L231.SubsectorLogit_urb_ind}, \code{L231.SubsectorShrwt_ind}, \code{L231.SubsectorShrwtFllt_urb_ind}, \code{L231.SubsectorInterp_urb_ind}, \code{L231.SubsectorInterpTo_urb_ind}, \code{L231.StubTech_urb_ind}, \code{L231.GlobalTechShrwt_urb_ind}, \code{L231.GlobalTechEff_urb_ind}, \code{L231.GlobalTechCoef_urb_ind}, \code{L231.GlobalTechCost_urb_ind}, \code{L231.RegionalTechCalValue_urb_ind}, \code{L231.IndCoef}. The corresponding file in the
#' original data system was \code{L231.proc_sector.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH July 2017
#' @export
module_emissions_L231.proc_sector <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions", # Units and source
             FILE = "emissions/A31.rsrc_info", # Units and source
             FILE = "emissions/A31.sector", # Units and source
             FILE = "emissions/A31.subsector_logit", # Units and source
             FILE = "emissions/A31.subsector_shrwt", # Units and source
             FILE = "emissions/A31.subsector_interp", # Units and source
             FILE = "emissions/A31.globaltech_shrwt", # Units and source
             FILE = "emissions/A31.globaltech_eff", # Units and source
             FILE = "emissions/A31.globaltech_cost", # Units and source
             FILE = "emissions/A31.globaltech_coef", # Units and source
             FILE = "energy/A32.globaltech_eff", # Units and source
             FILE = "emissions/mappings/GCAM_sector_tech",
             "L1322.in_EJ_R_indfeed_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L231.UnlimitRsrc",
             "L231.UnlimitRsrcPrice",
             "L231.FinalDemand_urb",
             "L231.Supplysector_urb_ind",
             "L231.SubsectorLogit_urb_ind",
             "L231.SubsectorShrwt_ind",
             "L231.SubsectorShrwtFllt_urb_ind",
             "L231.SubsectorInterp_urb_ind",
             "L231.SubsectorInterpTo_urb_ind",
             "L231.StubTech_urb_ind",
             "L231.GlobalTechShrwt_urb_ind",
             "L231.GlobalTechEff_urb_ind",
             "L231.GlobalTechCoef_urb_ind",
             "L231.GlobalTechCost_urb_ind",
             "L231.RegionalTechCalValue_urb_ind",
             "L231.IndCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    A31.rsrc_info <- get_data(all_data, "emissions/A31.rsrc_info")
    A31.sector <- get_data(all_data, "emissions/A31.sector")
    A31.subsector_logit <- get_data(all_data, "emissions/A31.subsector_logit")
    A31.subsector_shrwt <- get_data(all_data, "emissions/A31.subsector_shrwt")
    A31.subsector_interp <- get_data(all_data, "emissions/A31.subsector_interp")
    A31.globaltech_shrwt <- get_data(all_data, "emissions/A31.globaltech_shrwt")
    A31.globaltech_eff <- get_data(all_data, "emissions/A31.globaltech_eff")
    A31.globaltech_cost <- get_data(all_data, "emissions/A31.globaltech_cost")
    A31.globaltech_coef <- get_data(all_data, "emissions/A31.globaltech_coef")
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")

    # ===================================================
    L231.urban <- GCAM_sector_tech %>%
      filter(sector == "urban processes")

    L231.ind <- GCAM_sector_tech %>%
      filter(sector == "industrial processes")

    # L231.FinalDemand_urb: Final demand information for urban processes sector
    L231.FinalDemand_urb <- tibble(
      region = A_regions$region,
      energy.final.demand = "urban processes",
      perCapitaBased = 1,
      income.elasticity = 0,
      base.service = 0.004,
      aeei = 0
    ) %>%
      repeat_add_columns(tibble(year = BASE_YEARS))

    # L231.Supplysector_ind: Supply sector information for urban & industrial processes sectors
    L231.Supplysector_urb_ind <- A31.sector %>%
      write_to_all_regions(c("region", "supplysector", "output.unit", "input.unit", "price.unit",
                             "logit.year.fillout", "logit.exponent",  "logit.type"), GCAM_region_names = GCAM_region_names )

    # 2b. Subsector information
    # L231.SubsectorLogit_urb_ind: Subsector logit exponents of urban & industrial processes sectors
    L231.SubsectorLogit_urb_ind <- A31.subsector_logit %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "logit.year.fillout", "logit.exponent", "logit.type"),
                           GCAM_region_names = GCAM_region_names)

    # L231.SubsectorShrwt_urb_ind and L231.SubsectorShrwtFllt_urb_ind: Subsector shareweights of urban & industrial processes sectors
    if(any(!is.na(A31.subsector_shrwt$year))){
      L231.SubsectorShrwt_urb_ind <- write_to_all_regions(A31.subsector_shrwt[!is.na(A31.subsector_shrwt$year), ], names_SubsectorShrwt)
    }
    if(any(!is.na(A31.subsector_shrwt$year.fillout))){
      L231.SubsectorShrwtFllt_urb_ind <- write_to_all_regions(A31.subsector_shrwt[!is.na(A31.subsector_shrwt$year.fillout), ], names_SubsectorShrwtFllt)
    }

    # ===================================================

    # Produce outputs
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.UnlimitRsrc") %>%
    add_precursors("common/GCAM_region_names",
                   "emissions/A_regions",
                   "emissions/A31.rsrc_info",
                   "emissions/A31.sector",
                   "emissions/A31.subsector_logit",
                   "emissions/A31.subsector_shrwt",
                   "emissions/A31.subsector_interp",
                   "emissions/A31.globaltech_shrwt",
                   "emissions/A31.globaltech_eff",
                   "emissions/A31.globaltech_cost",
                   "emissions/A31.globaltech_coef",
                   "energy/A32.globaltech_eff",
                   "emissions/mappings/GCAM_sector_tech",
                   "L1322.in_EJ_R_indfeed_F_Yh",
                   "L1322.in_EJ_R_indenergy_F_Yh") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.UnlimitRsrc
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.UnlimitRsrcPrice") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.UnlimitRsrcPrice
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.FinalDemand_urb") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.FinalDemand_urb
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.Supplysector_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.Supplysector_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.SubsectorLogit_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.SubsectorLogit_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.SubsectorShrwt_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.SubsectorShrwt_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.SubsectorShrwtFllt_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.SubsectorShrwtFllt_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.SubsectorInterp_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.SubsectorInterp_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.SubsectorInterpTo_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.SubsectorInterpTo_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.StubTech_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.StubTech_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.GlobalTechShrwt_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.GlobalTechShrwt_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.GlobalTechEff_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.GlobalTechEff_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.GlobalTechCoef_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.GlobalTechCoef_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.GlobalTechCost_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.GlobalTechCost_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.RegionalTechCalValue_urb_ind") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.RegionalTechCalValue_urb_ind
    tibble() %>%
    add_title("descriptive title of data") %>%
    add_units("units") %>%
    add_comments("comments describing how data generated") %>%
    add_comments("can be multiple lines") %>%
    add_legacy_name("L231.IndCoef") %>%
    add_precursors("common/GCAM_region_names") %>%
    add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    L231.IndCoef

    return_data(L231.UnlimitRsrc, L231.UnlimitRsrcPrice, L231.FinalDemand_urb, L231.SectorLogitTables[[ curr_table ]]$data, L231.Supplysector_urb_ind, L231.SubsectorLogitTables[[ curr_table ]]$data, L231.SubsectorLogit_urb_ind, L231.SubsectorShrwt_ind, L231.SubsectorShrwtFllt_urb_ind, L231.SubsectorInterp_urb_ind, L231.SubsectorInterpTo_urb_ind, L231.StubTech_urb_ind, L231.GlobalTechShrwt_urb_ind, L231.GlobalTechEff_urb_ind, L231.GlobalTechCoef_urb_ind, L231.GlobalTechCost_urb_ind, L231.RegionalTechCalValue_urb_ind, L231.IndCoef)
  } else {
    stop("Unknown command")
  }
}



