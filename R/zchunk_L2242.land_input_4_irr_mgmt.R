#' module_aglu_L2242.land_input_4_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2242.LN4_Logit}, \code{L2242.LN4_NodeGhostShare}, \code{L2242.LN4_NodeIsGhostShareRel}. The corresponding file in the
#' original data system was \code{L2242.land_input_4_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2242.land_input_4_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/A_LandNode_logit_irr",
             FILE = "temp-data-inject/L223.LN3_MgdAllocation_bio",
             FILE = "temp-data-inject/L223.LN3_MgdAllocation_crop",
             FILE = "temp-data-inject/L223.LN3_LeafGhostShare",
             FILE = "temp-data-inject/L223.LN3_LeafIsGhostShareRel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2242.LN4_Logit",
             "L2242.LN4_NodeGhostShare",
             "L2242.LN4_NodeIsGhostShareRel"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_LandNode_logit_irr <- get_data(all_data, "aglu/A_LandNode_logit_irr")
    L223.LN3_MgdAllocation_bio <- get_data(all_data, "temp-data-inject/L223.LN3_MgdAllocation_bio")
    L223.LN3_MgdAllocation_crop <- get_data(all_data, "temp-data-inject/L223.LN3_MgdAllocation_crop")
    L223.LN3_LeafGhostShare <- get_data(all_data, "temp-data-inject/L223.LN3_LeafGhostShare")
    L223.LN3_LeafIsGhostShareRel <- get_data(all_data, "temp-data-inject/L223.LN3_LeafIsGhostShareRel")

    names_LN3_Leaf <- c("region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf")
    # L2242.LN4_Logit: Logit exponent, fourth nest
    # NOTE: There are no technologies that are disaggregated to irrigated and rainfed, but not to lo- and hi-input techs
    # This code file only writes out the logit exponent for the irrigated/rainfed node competition
    # Use the cropland and bioenergy allocation tables to establish which region/glu/node combinations are available
    L223.LN3_MgdAllocation_crop %>%
      select(one_of(names_LN3_Leaf)) %>%
      unique %>%
      bind_rows(unique(select(L223.LN3_MgdAllocation_bio, one_of(names_LN3_Leaf)))) %>%
      # What was a leaf for level3 is now a node, as it will have the 4th level nested under it
      rename(LandNode4 = LandLeaf) %>%
      mutate(LandNode4 = sub("Root_Tuber", "RootTuber", LandNode4),
             LandNode4 = sub("biomass_tree", "biomasstree", LandNode4),
             LandNode4 = sub("biomass_grass", "biomassgrass", LandNode4)) %>%
      separate(LandNode4, c("LandNode4", "GLU_name")) %>%
      mutate(logit.year.fillout = min(BASE_YEARS),
             LandNode4 = sub("RootTuber", "Root_Tuber", LandNode4),
             LandNode4 = sub("biomasstree", "biomass_tree", LandNode4),
             LandNode4 = sub("biomassgrass", "biomass_grass", LandNode4)) %>%
      left_join(A_LandNode_logit_irr, by = c("LandNode4" = "LandNode")) %>%
      mutate(LandNode4 = paste(LandNode4, GLU_name, sep = "_")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN4_Logit"]])) %>%
      # Note: The na.omit is intended to get rid of a tailing node-rename table
      # that may be appended to level2 land alloator tables
      na.omit ->
      L2242.LN4_Logit

    # L2241.LN4_NodeGhostShare:
    # Indicate that the bioenergy node is available in future years, and specify the ghost node share
    # These are the same as the values that would have been set as ghost share in the leaves in land input 3.
    # We can just copy that data frame and just rename the LandLeaf column to LandNode.
    L223.LN3_LeafGhostShare %>%
      rename(LandNode4 = LandLeaf) %>%
      # Note: The na.omit is intended to get rid of a tailing node-rename table
      # that may be appended to level2 land alloator tables
      na.omit ->
      L2242.LN4_NodeGhostShare

    L223.LN3_LeafIsGhostShareRel %>%
      rename(LandNode4 = LandLeaf) %>%
      # Note: The na.omit is intended to get rid of a tailing node-rename table
      # that may be appended to level2 land alloator tables
      na.omit ->
      L2242.LN4_NodeIsGhostShareRel

    # Produce outputs
    L2242.LN4_Logit %>%
      add_title("Logit exponent, fourth nest") %>%
      add_units("NA") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2242.LN4_Logit") %>%
      add_precursors("aglu/A_LandNode_logit_irr",
                     "temp-data-inject/L223.LN3_MgdAllocation_bio",
                     "temp-data-inject/L223.LN3_MgdAllocation_crop") ->
      L2242.LN4_Logit

    L2242.LN4_NodeGhostShare %>%
      add_title("irrigated and rainfed bioenergy, fourth nest") %>%
      add_units("NA") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2242.LN4_NodeGhostShare") %>%
      add_precursors("temp-data-inject/L223.LN3_LeafGhostShare") ->
      L2242.LN4_NodeGhostShare

    L2242.LN4_NodeIsGhostShareRel %>%
      add_title("irrigated and rainfed bioenergy, fourth nest") %>%
      add_units("NA") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2242.LN4_NodeIsGhostShareRel") %>%
      add_precursors("temp-data-inject/L223.LN3_LeafIsGhostShareRel") ->
      L2242.LN4_NodeIsGhostShareRel

    return_data(L2242.LN4_Logit, L2242.LN4_NodeGhostShare, L2242.LN4_NodeIsGhostShareRel)
  } else {
    stop("Unknown command")
  }
}
