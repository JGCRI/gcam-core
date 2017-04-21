#' module_aglu_LB109.ag_an_ALL_R_C_Y
#'
#' Calculate primary agricultural good and animal product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L109.ag_ALL_Mt_R_C_Y}, \code{L109.an_ALL_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LB109.ag_an_ALL_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB109.ag_an_ALL_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L101.ag_Food_Mt_R_C_Y",
              "L103.ag_Prod_Mt_R_C_Y",
              FILE = "temp-data-inject/L105.an_Food_Mt_R_C_Y",
              FILE = "temp-data-inject/L105.an_Prod_Mt_R_C_Y",
              FILE = "temp-data-inject/L106.ag_NetExp_Mt_R_C_Y",
              FILE = "temp-data-inject/L106.an_NetExp_Mt_R_C_Y",
              FILE = "temp-data-inject/L108.ag_Feed_Mt_R_C_Y",
              FILE = "temp-data-inject/L108.ag_NetExp_Mt_R_FodderHerb_Y",
              FILE = "temp-data-inject/L122.in_Mt_R_C_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.ag_Food_Mt_R_C_Y <- get_data(all_data, "L101.ag_Food_Mt_R_C_Y") %>%
      mutate(year = as.integer(year)) # year is character, convert to integer

    L103.ag_Prod_Mt_R_C_Y <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y")

    L105.an_Food_Mt_R_C_Y <- get_data(all_data, "temp-data-inject/L105.an_Food_Mt_R_C_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L105.an_Prod_Mt_R_C_Y <- get_data(all_data, "temp-data-inject/L105.an_Prod_Mt_R_C_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L106.ag_NetExp_Mt_R_C_Y <- get_data(all_data, "temp-data-inject/L106.ag_NetExp_Mt_R_C_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L106.an_NetExp_Mt_R_C_Y <- get_data(all_data, "temp-data-inject/L106.an_NetExp_Mt_R_C_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L108.ag_Feed_Mt_R_C_Y <- get_data(all_data, "temp-data-inject/L108.ag_Feed_Mt_R_C_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L108.ag_NetExp_Mt_R_FodderHerb_Y <- get_data(all_data, "temp-data-inject/L108.ag_NetExp_Mt_R_FodderHerb_Y") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    L122.in_Mt_R_C_Yh <- get_data(all_data, "temp-data-inject/L122.in_Mt_R_C_Yh") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))   # change Xyear to year

    # Part 1: Primary agricultural goods"
    # List of all flows for primary ag goods
    ag_Flow_cols <- c( "Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt", "OtherUses_Mt" )
    # Get a list of any commodities (e.g. pasture, residue, scavenging) in feed but not in production tables
    L103.ag_Prod_Mt_R_C_Y %>%
      select(GCAM_commodity) %>%
      unique() %>%
      .[['GCAM_commodity']] -> Primary_commodities

     L108.ag_Feed_Mt_R_C_Y %>%
      filter(!(GCAM_commodity %in% Primary_commodities)) %>%
      select(GCAM_commodity) %>%
      unique() %>%
      .[['GCAM_commodity']] -> Feed_commodities

    # Combine all tables
    L106.ag_NetExp_Mt_R_C_Y %>%
      bind_rows(L108.ag_NetExp_Mt_R_FodderHerb_Y) %>%
      # Name the flows in each table
      mutate(flow = "NetExp_Mt") %>%
      bind_rows(mutate(L103.ag_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L101.ag_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      bind_rows(mutate(L108.ag_Feed_Mt_R_C_Y, flow = "Feed_Mt")) %>%
      bind_rows(mutate(L122.in_Mt_R_C_Yh, flow = "Biofuels_Mt")) %>%
      # Get all combinations of each GCAM_commodity and flow, by spreading to wide format
      spread(flow, value) %>%
      # Set missing values in the complete combinations to zero
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # For any commodities (e.g. pasture, residue, scavenging) in feed but not in production tables, set production = feed
      mutate(Prod_Mt = if_else(GCAM_commodity %in% Feed_commodities, Feed_Mt, Prod_Mt),
             # Calculate the domestic supply and other uses
             Supply_Mt = Prod_Mt - NetExp_Mt,
             OtherUses_Mt = Supply_Mt - Food_Mt - Feed_Mt - Biofuels_Mt) -> L109.ag_ALL_Mt_R_C_Y

    # Adjusting regional crop mass balances to remove net negative other uses
    if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
      L109.ag_ALL_Mt_R_C_Y %>%
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        # NOTE: Increases in global net exports are apportioned among regions with positive net exports, according to shares
        group_by(GCAM_commodity, year) %>%
        summarise_at(vars(GlobalNetExpAdj = NegOtherUses_Mt, GlobalOtherUses_Mt = OtherUses_Mt_adj), sum) -> L109.ag_ALL_Mt_glbl_C_Y

     # Subset negative and positive "other uses" separately.
      L109.ag_ALL_Mt_R_C_Y %>%
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               # Positive will be the new adjusted "Other uses"
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               # Assigning negative other net uses to imports, and adjusting global trade to maintain balances
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        left_join(L109.ag_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
        mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt),
               NetExp_Mt_adj2 = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
               # Rebuilding primary agricultural product mass balance table
               NetExp_Mt = NetExp_Mt_adj2,
               Supply_Mt = Prod_Mt - NetExp_Mt,
               OtherUses_Mt = Supply_Mt - Food_Mt - Biofuels_Mt - Feed_Mt) %>%
        gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
        # Only the flow variables
        filter(flow %in% ag_Flow_cols) %>%

        mutate(value = round(value, aglu.DIGITS_CALOUTPUT)) %>%
        spread(flow, value) %>%
        # Re-order the columns
        select() ->
        L109.ag_ALL_Mt_R_C_Y
      }

    # Part 2: Animal commodities
    # List of all flows for animal products
    an_Flow_cols <- c( "Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "OtherUses_Mt" )
    # Name the flows in each table
    L105.an_Prod_Mt_R_C_Y %>%
      mutate(flow = "Prod_Mt") %>%
      # Combine all flow tables
      bind_rows(mutate(L106.an_NetExp_Mt_R_C_Y, flow = "NetExp_Mt")) %>%
      bind_rows(mutate(L105.an_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      # convert to wide format for easier calculations
      spread(flow, value) %>%
      # Calculate the domestic supply and other uses, and re-order the columns
      mutate(Supply_Mt = Prod_Mt - NetExp_Mt,
             OtherUses_Mt = Supply_Mt - Food_Mt) -> L109.an_ALL_Mt_R_C_Y

    if(any(L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
      # Adjusting regional animal product mass balances to remove net negative other uses
      # NOTE: Assigning negative other net uses to imports, and adjusting global trade to maintain balances
      L109.an_ALL_Mt_R_C_Y %>%
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        # NOTE: Increases in global net exports are apportioned among regions with positive net exports, according to shares
        group_by(GCAM_commodity, year) %>%
        summarise_at(vars(GlobalNetExpAdj = NegOtherUses_Mt, GlobalOtherUses_Mt = OtherUses_Mt_adj), sum) -> L109.an_ALL_Mt_glbl_C_Y

      # Subset negative and positive "other uses" separately. Positive will be the new adjusted "Other uses"
      L109.an_ALL_Mt_R_C_Y %>%
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        left_join(L109.an_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
        mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt ),
               NetExp_Mt_adj2 = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
               # Rebuilding animal product mass balance table
               NetExp_Mt = NetExp_Mt_adj2,
               Supply_Mt = Prod_Mt - NetExp_Mt,
               OtherUses_Mt = Supply_Mt - Food_Mt) %>%
        gather(flow, value, --GCAM_region_ID, -GCAM_commodity, -year) %>%
        # Only the flow variables
        filter(flow %in% an_Flow_cols) %>%
        # re-order the columns
        mutate(value = round(value, aglu.DIGITS_CALOUTPUT)) ->
        L109.an_ALL_Mt_R_C_Y
    }

    # Produce outputs
    L109.ag_ALL_Mt_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L109.ag_ALL_Mt_R_C_Y") %>%
      add_precursors("L101.ag_Food_Mt_R_C_Y",
                     "L103.ag_Prod_Mt_R_C_Y",
                     "temp-data-inject/L106.ag_NetExp_Mt_R_C_Y",
                     "temp-data-inject/L108.ag_Feed_Mt_R_C_Y",
                     "temp-data-inject/L108.ag_NetExp_Mt_R_FodderHerb_Y",
                     "temp-data-inject/L122.in_Mt_R_C_Yh") ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L109.an_ALL_Mt_R_C_Y") %>%
      add_precursors("temp-data-inject/L105.an_Food_Mt_R_C_Y",
                     "temp-data-inject/L105.an_Prod_Mt_R_C_Y",
                     "temp-data-inject/L106.an_NetExp_Mt_R_C_Y") ->
      L109.an_ALL_Mt_R_C_Y

    return_data(L109.ag_ALL_Mt_R_C_Y, L109.an_ALL_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
