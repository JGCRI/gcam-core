# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_transportation_UCD_CORE_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_batch_transportation_UCD_CORE_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.GlobalTranTechProfitShutdown",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml","transportation_UCD_highEV.xml")
    xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    ## silence package check.
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")

    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")

    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")

    L254.StubTech_passthru <- get_data(all_data, "L254.StubTech_passthru")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.GlobalTechShrwt_passthru<- get_data(all_data, "L254.GlobalTechShrwt_passthru")
    L254.GlobalTechShrwt_nonmotor <- get_data(all_data, "L254.GlobalTechShrwt_nonmotor")
    L254.GlobalTechCoef_passthru <- get_data(all_data, "L254.GlobalTechCoef_passthru")
    L254.GlobalRenewTech_nonmotor <- get_data(all_data, "L254.GlobalRenewTech_nonmotor")
    L254.GlobalTranTechInterp <- get_data(all_data, "L254.GlobalTranTechInterp")
    L254.GlobalTranTechShrwt <- get_data(all_data, "L254.GlobalTranTechShrwt")
    L254.GlobalTranTechSCurve <- get_data(all_data, "L254.GlobalTranTechSCurve")
    L254.GlobalTranTechProfitShutdown <- get_data(all_data, "L254.GlobalTranTechProfitShutdown")
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")


    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.StubTechCalInput_passthru <- get_data(all_data, "L254.StubTechCalInput_passthru")
    L254.StubTechProd_nonmotor <- get_data(all_data, "L254.StubTechProd_nonmotor")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn")


    # ===================================================

    # Produce outputs
    # Because `return_data` gets the name of the object from what's actually given in the call,
    # we need to assign xml_tmp to a correctly-named variable in the current environment
    # transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
    #   transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- transportation_UCD_CORE_highEV.xml <- NULL  # silence package check notes
    transportation_UCD_CORE.xml <- transportation_UCD_SSP1.xml <- transportation_UCD_SSP2.xml <-
      transportation_UCD_SSP3.xml <- transportation_UCD_SSP5.xml <- NULL  # silence package check notes

    ret_data <- c()
    curr_env <- environment()

    #for (i in c("CORE","SSP1","SSP3","SSP5", "highEV")){
    for (i in c("CORE","SSP1","SSP3","SSP5")){
      xml_name <- paste0("transportation_UCD_", i, ".xml")
      #Read SSP specific data
      L254.tranSubsectorSpeed_SSP <- L254.tranSubsectorSpeed %>% filter(sce== i)
      L254.StubTranTech_SSP <- L254.StubTranTech %>% filter(sce== i)
      #kbn 2020-03-26 We have energy demand assumptions only for SSP1. So get that data for SSP1. For the other SSPs, keep
      #data from the CORE.
      if (i=="SSP1"){
      #VOTT and Demand data
      L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce==i)
      L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT %>% filter(sce==i)
      L254.tranSubsectorFuelPref_SSP<-L254.tranSubsectorFuelPref %>% filter(sce==i)
      L254.PerCapitaBased_trn_SSP<- L254.PerCapitaBased_trn %>% filter(sce==i)
      L254.PriceElasticity_trn_SSP <- L254.PriceElasticity_trn %>%  filter(sce==i)
      L254.IncomeElasticity_trn_SSP <- L254.IncomeElasticity_trn %>% filter(sce==i)}else{
        L254.tranSubsectorSpeed_passthru_SSP <- L254.tranSubsectorSpeed_passthru %>% filter(sce=="CORE")
        L254.tranSubsectorVOTT_SSP<- L254.tranSubsectorVOTT %>% filter(sce=="CORE")
        L254.tranSubsectorFuelPref_SSP<-L254.tranSubsectorFuelPref %>% filter(sce=="CORE")
        L254.PerCapitaBased_trn_SSP <- L254.PerCapitaBased_trn %>% filter(sce=="CORE")
        L254.PriceElasticity_trn_SSP <- L254.PriceElasticity_trn %>% filter(sce=="CORE")
        L254.IncomeElasticity_trn_SSP <- L254.IncomeElasticity_trn %>% filter(sce=="CORE")
      }



      #kbn 2020-02-11 For the SSPs, we want to bring in values such as co-efficients, load factors and costs after the base year. This is because we are
      # feeding the model outputs from the CORE in the base year, so having SSP values for these variables in the base year would lead to a calibration error
      # i.e. mismatch between calibrated output and actual.

      L254.StubTranTechLoadFactor_SSP <- L254.StubTranTechLoadFactor %>% filter(sce== i)
      if (i != "CORE"){L254.StubTranTechLoadFactor_SSP<-L254.StubTranTechLoadFactor %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}


      L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== i)
      if (i != "CORE"){L254.StubTranTechCost_SSP<-L254.StubTranTechCost %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTranTechCoef_SSP <- L254.StubTranTechCoef %>%  filter(sce== i)

      if (i != "CORE"){L254.StubTranTechCoef_SSP<-L254.StubTranTechCoef %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.StubTech_passthru_SSP <- L254.StubTech_passthru %>% filter(sce==i)
      L254.StubTech_nonmotor_SSP <- L254.StubTech_nonmotor %>% filter(sce==i)
      L254.Supplysector_trn_SSP  <- L254.Supplysector_trn %>% filter(sce==i)
      L254.FinalEnergyKeyword_trn_SSP <- L254.FinalEnergyKeyword_trn %>% filter(sce==i)
      L254.tranSubsectorLogit_SSP <- L254.tranSubsectorLogit %>% filter(sce==i)
      #L254.tranSubsectorShrwt_SSP <- L254.tranSubsectorShrwt %>%  filter(sce ==i)
      L254.tranSubsectorShrwtFllt_SSP <- L254.tranSubsectorShrwtFllt %>%  filter(sce ==i)
      L254.tranSubsectorInterp_SSP <- L254.tranSubsectorInterp %>%  filter(sce ==i)
      L254.tranSubsectorFuelPref_SSP <- L254.tranSubsectorFuelPref %>%  filter(sce ==i)
      L254.StubTranTechCalInput_SSP <-  L254.StubTranTechCalInput %>% filter(sce ==i)
      L254.GlobalTranTechInterp_SSP <- L254.GlobalTranTechInterp %>% filter(sce==i)
      L254.GlobalTranTechShrwt_SSP <- L254.GlobalTranTechShrwt %>%  filter(sce==i)
      if (i != "CORE"){L254.StubTranTechCalInput_SSP<-L254.StubTranTechCalInput %>%  filter(sce== i) %>% filter(year>MODEL_FINAL_BASE_YEAR)}

      L254.BaseService_trn_SSP <- L254.BaseService_trn %>% filter(sce =="CORE")


      #Create xmls
      create_xml(xml_name) %>%
        add_logit_tables_xml(L254.Supplysector_trn_SSP, "Supplysector") %>%
        add_xml_data(L254.FinalEnergyKeyword_trn_SSP, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L254.tranSubsectorLogit_SSP, "tranSubsectorLogit", "tranSubsector") %>%
        add_xml_data(L254.tranSubsectorShrwtFllt_SSP, "tranSubsectorShrwtFllt") %>%
        add_xml_data(L254.tranSubsectorInterp_SSP, "tranSubsectorInterp") %>%
        add_xml_data(L254.tranSubsectorSpeed_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_passthru_SSP, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_noVOTT, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorSpeed_nonmotor, "tranSubsectorSpeed") %>%
        add_xml_data(L254.tranSubsectorVOTT_SSP, "tranSubsectorVOTT") %>%
        add_xml_data(L254.tranSubsectorFuelPref_SSP, "tranSubsectorFuelPref") %>%
        add_xml_data(L254.StubTranTech_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_passthru_SSP, "StubTranTech") %>%
        add_xml_data(L254.StubTech_nonmotor_SSP, "StubTranTech") %>%
        add_xml_data(L254.GlobalTechShrwt_passthru, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechShrwt_nonmotor, "GlobalTechShrwt") %>%
        add_xml_data(L254.GlobalTechCoef_passthru, "GlobalTechCoef") %>%
        add_xml_data(L254.GlobalRenewTech_nonmotor, "GlobalRenewTech") %>%
        add_xml_data(L254.GlobalTranTechInterp_SSP, "GlobalTranTechInterp") %>%
        add_xml_data(L254.GlobalTranTechShrwt_SSP, "GlobalTranTechShrwt") %>%
        add_xml_data(L254.GlobalTranTechSCurve, "GlobalTranTechSCurve") %>%
        add_xml_data(L254.GlobalTranTechProfitShutdown, "GlobalTranTechProfitShutdown") %>%
        add_xml_data(L254.StubTranTechCalInput_SSP, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTranTechLoadFactor_SSP, "StubTranTechLoadFactor") %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_xml_data(L254.StubTranTechCoef_SSP, "StubTranTechCoef") %>%
        add_xml_data(L254.StubTechCalInput_passthru, "StubTranTechCalInput") %>%
        add_xml_data(L254.StubTechProd_nonmotor, "StubTranTechProd") %>%
        add_xml_data(L254.PerCapitaBased_trn_SSP, "PerCapitaBased") %>%
        add_xml_data(L254.PriceElasticity_trn_SSP, "PriceElasticity") %>%
        add_xml_data(L254.IncomeElasticity_trn_SSP, "IncomeElasticity") %>%
        add_xml_data(L254.BaseService_trn_SSP, "BaseService") %>%
        add_precursors("L254.Supplysector_trn",
                       "L254.FinalEnergyKeyword_trn",
                       "L254.tranSubsectorLogit",
                       "L254.tranSubsectorShrwtFllt",
                       "L254.tranSubsectorInterp",
                       "L254.tranSubsectorSpeed",
                       "L254.tranSubsectorSpeed_passthru",
                       "L254.tranSubsectorSpeed_noVOTT",
                       "L254.tranSubsectorSpeed_nonmotor",
                       "L254.tranSubsectorVOTT",
                       "L254.tranSubsectorFuelPref",
                       "L254.StubTranTech",
                       "L254.StubTech_passthru",
                       "L254.StubTech_nonmotor",
                       "L254.GlobalTechShrwt_passthru",
                       "L254.GlobalTechShrwt_nonmotor",
                       "L254.GlobalTechCoef_passthru",
                       "L254.GlobalRenewTech_nonmotor",
                       "L254.GlobalTranTechInterp",
                       "L254.GlobalTranTechShrwt",
                       "L254.GlobalTranTechSCurve",
                       "L254.GlobalTranTechProfitShutdown",
                       "L254.StubTranTechCalInput",
                       "L254.StubTranTechLoadFactor",
                       "L254.StubTranTechCost",
                       "L254.StubTranTechCoef",
                       "L254.StubTechCalInput_passthru",
                       "L254.StubTechProd_nonmotor",
                       "L254.PerCapitaBased_trn",
                       "L254.PriceElasticity_trn",
                       "L254.IncomeElasticity_trn",
                       "L254.BaseService_trn")  %>%
                        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)

    }
    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()

  } else {
    stop("Unknown command")
  }
}
