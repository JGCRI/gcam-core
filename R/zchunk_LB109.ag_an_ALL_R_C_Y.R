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
#' @details This chunk combines all flow tables of GCAM agricultural commodities, calculates mass balances by
#' GCAM region, commodity and year, and adjusts global and regional net exports to remove negative other uses.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC April 2017
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

    L103.ag_Prod_Mt_R_C_Y <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y") %>%
      mutate(year = as.integer(year)) # convert to integer

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

    # Part 1: Primary agricultural goods
    # List of all flows for primary agricultural good balances
    ag_Flow_cols <- c("Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt", "OtherUses_Mt")
    # List of commodities in production table
    L103.ag_Prod_Mt_R_C_Y %>%
      select(GCAM_commodity) %>%
      unique() %>%
      .[["GCAM_commodity"]] -> Primary_commodities
    # List of any commodities (e.g. pasture, residue, scavenging) in feed but not in production table
    L108.ag_Feed_Mt_R_C_Y %>%
      filter(!(GCAM_commodity %in% Primary_commodities)) %>%
      select(GCAM_commodity) %>%
      unique() %>%
      .[["GCAM_commodity"]] -> Feed_commodities

    # Combine all flow tables
    L106.ag_NetExp_Mt_R_C_Y %>%
      bind_rows(L108.ag_NetExp_Mt_R_FodderHerb_Y) %>%
      # Name the flows in each table, and combine all tables
      mutate(flow = "NetExp_Mt") %>%
      bind_rows(mutate(L103.ag_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L101.ag_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      bind_rows(mutate(L108.ag_Feed_Mt_R_C_Y, flow = "Feed_Mt")) %>%
      bind_rows(mutate(L122.in_Mt_R_C_Yh, flow = "Biofuels_Mt")) %>%
      # Get all combinations of each GCAM_commodity and flow, by spreading to wide format
      spread(flow, value) %>%
      # Set missing values in the complete combinations to zero
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # Make sure year is integer
      mutate(year = as.integer(year)) %>%
      # For any feed commodities (e.g. pasture, residue, scavenging) that are not reported in production or trade table,
      # assume all production are domestic, and set production = feed
      mutate(Prod_Mt = if_else(GCAM_commodity %in% Feed_commodities, Feed_Mt, Prod_Mt),
             # Calculate the domestic supply
             Supply_Mt = Prod_Mt - NetExp_Mt,
             # Calculate other uses
             OtherUses_Mt = Supply_Mt - Food_Mt - Feed_Mt - Biofuels_Mt) -> L109.ag_ALL_Mt_R_C_Y

    # Adjust global and regional crop mass balances to remove net negative other uses
    # Assign negative other net uses to imports, and adjust global trade to maintain balances
    # Changes in global net exports are apportioned among regions with positive other uses, according to regional shares

    # First calculate the changes in global net exports
    if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
      L109.ag_ALL_Mt_R_C_Y %>%
        # Subset negative and positive "other uses" separately
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               # Positive will be the new adjusted "other uses", and replace negative with zero
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        group_by(GCAM_commodity, year) %>%
        # Calculate the changes in global net exports = sum of negative other uses, global other uses =  sum of positive other uses
        summarise(GlobalNetExpAdj = sum(NegOtherUses_Mt),
                  GlobalOtherUses_Mt = sum(OtherUses_Mt_adj)) -> L109.ag_ALL_Mt_glbl_C_Y

      # Second, distribute changes in global net exports among regions with positive other uses, according to regional shares
      L109.ag_ALL_Mt_R_C_Y %>%
        # Subset negative and positive "other uses" separately
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               # Positive will be the new adjusted "other uses", and replace negative with zero
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        # Combine with global adjusted other uses and changes in global net exports
        left_join(L109.ag_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
        # Calculate the regional share of global adjusted other uses, the share is zero for regions with negative other uses
        mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt),
               # Allocate the changes in global net exports (total of negative other uses) among regions with positive other uses
               NetExp_Mt = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
               # Rebuild the mass balance table
               Supply_Mt = Prod_Mt - NetExp_Mt,
               OtherUses_Mt = Supply_Mt - Food_Mt - Biofuels_Mt - Feed_Mt) %>%
        gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
        # Select only the flow variables for primary agricultural goods
        filter(flow %in% ag_Flow_cols) %>%
        # Re-order the flow variables so the columns are in the right order
        mutate(flow = factor(flow, levels = ag_Flow_cols),
               value = round(value, aglu.DIGITS_CALOUTPUT)) %>%
        spread(flow, value) ->
        L109.ag_ALL_Mt_R_C_Y
      }

    # Part 2: Animal commodities
    # List of all flows for animal products
    an_Flow_cols <- c("Prod_Mt", "NetExp_Mt", "Supply_Mt", "Food_Mt", "OtherUses_Mt")
    # Name the flows in each table
    L105.an_Prod_Mt_R_C_Y %>%
      mutate(flow = "Prod_Mt") %>%
      # Combine all flow tables
      bind_rows(mutate(L106.an_NetExp_Mt_R_C_Y, flow = "NetExp_Mt")) %>%
      bind_rows(mutate(L105.an_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      # Convert to wide format for easier calculations
      spread(flow, value) %>%
      # Calculate the domestic supply
      mutate(Supply_Mt = Prod_Mt - NetExp_Mt,
             # Calculate other uses
             OtherUses_Mt = Supply_Mt - Food_Mt) -> L109.an_ALL_Mt_R_C_Y

    # Adjust global and regional animal product mass balances to remove net negative other uses
    # Assign negative other net uses to imports, and adjust global trade to maintain balances
    # Changes in global net exports are apportioned among regions with positive other uses, according to shares

    # First calculate the changes in global net exports
    if(any(L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0)) {
      L109.an_ALL_Mt_R_C_Y %>%
        # Subset negative and positive "other uses" separately
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               # Positive will be the new adjusted "other uses", and replace negative with zero
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        group_by(GCAM_commodity, year) %>%
        # Calculate the changes in global net exports = sum of negative other uses, global other uses =  sum of positive other uses
        summarise(GlobalNetExpAdj = sum(NegOtherUses_Mt),
                  GlobalOtherUses_Mt = sum(OtherUses_Mt_adj)) -> L109.an_ALL_Mt_glbl_C_Y

      # Second, distribute changes in global net exports among regions with positive other uses, according to regional shares
      L109.an_ALL_Mt_R_C_Y %>%
        # Subset negative and positive "other uses" separately
        mutate(NegOtherUses_Mt = if_else(OtherUses_Mt < 0, OtherUses_Mt, 0),
               # Positive will be the new adjusted "other uses", and replace negative with zero
               OtherUses_Mt_adj = if_else(OtherUses_Mt >= 0, OtherUses_Mt, 0),
               # Assign negative "other uses" to imports, and calculate the adjusted regional net exports
               NetExp_Mt_adj = NetExp_Mt + NegOtherUses_Mt) %>%
        # Combine with global adjusted other uses and changes in global net exports
        left_join(L109.an_ALL_Mt_glbl_C_Y, by = c("GCAM_commodity", "year")) %>%
        # Calculate the regional share of global adjusted other uses, the share is zero for regions with negative other uses
        mutate(NetExpAdjFrac = if_else(GlobalOtherUses_Mt == 0, 0, OtherUses_Mt_adj / GlobalOtherUses_Mt ),
               # Allocate the changes in global net exports (total of negative other uses) among regions with positive other uses
               NetExp_Mt = NetExp_Mt_adj - NetExpAdjFrac * GlobalNetExpAdj,
               # Rebuild animal product mass balance table
               Supply_Mt = Prod_Mt - NetExp_Mt,
               OtherUses_Mt = Supply_Mt - Food_Mt) %>%
        gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
        # Select only the flow variables for animal products
        filter(flow %in% an_Flow_cols) %>%
        # Re-order the flow variables so the columns are in the right order
        mutate(flow = factor(flow, levels = an_Flow_cols),
               value = round(value, aglu.DIGITS_CALOUTPUT)) %>%
        spread(flow, value) ->
        L109.an_ALL_Mt_R_C_Y
      }

    # Produce outputs
    L109.ag_ALL_Mt_R_C_Y %>%
      add_title("Primary agricultural good mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate primary agricultural good mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
      add_legacy_name("L109.ag_ALL_Mt_R_C_Y") %>%
      add_precursors("L101.ag_Food_Mt_R_C_Y",
                     "L103.ag_Prod_Mt_R_C_Y",
                     "temp-data-inject/L106.ag_NetExp_Mt_R_C_Y",
                     "temp-data-inject/L108.ag_Feed_Mt_R_C_Y",
                     "temp-data-inject/L108.ag_NetExp_Mt_R_FodderHerb_Y",
                     "temp-data-inject/L122.in_Mt_R_C_Yh") ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      add_title("Animal product mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate animal product mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
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
