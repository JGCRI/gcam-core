#' module_aglu_L2242.land_input_4_irr_mgmt
#'
#' Generate logit exponent of the fourth land node that specifies crop commodity and GLU by region,
#' and generate the ghost node share for the bionenergy node.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2242.LN4_Logit}, \code{L2242.LN4_NodeGhostShare}, \code{L2242.LN4_NodeIsGhostShareRel}. The corresponding file in the
#' original data system was \code{L2242.land_input_4_irr_mgmt.R} (aglu level2).
#' @details This chunk generates the logit exponent of the fourth land nest that specifies crop commodity and GLU by region,
#' and the ghost node share for the bionenergy node in future years, and specifies whether the bionenergy ghost node share is relative.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC August 2017
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

    # silence package check notes
   GLU_name <- LandLeaf <- LandNode4 <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_LandNode_logit_irr <- get_data(all_data, "aglu/A_LandNode_logit_irr")
    L223.LN3_MgdAllocation_bio <- get_data(all_data, "temp-data-inject/L223.LN3_MgdAllocation_bio")
    L223.LN3_MgdAllocation_crop <- get_data(all_data, "temp-data-inject/L223.LN3_MgdAllocation_crop")
    L223.LN3_LeafGhostShare <- get_data(all_data, "temp-data-inject/L223.LN3_LeafGhostShare")
    L223.LN3_LeafIsGhostShareRel <- get_data(all_data, "temp-data-inject/L223.LN3_LeafIsGhostShareRel")

    # L2242.LN4_Logit: Logit exponent of the fourth land nest by region
    # There are no technologies that are disaggregated to irrigated and rainfed but not to lo- and hi-input techs,
    # so here we only write out the logit exponent for the irrigated/rainfed node competition.
    # Use the cropland and bioenergy allocation tables to establish which region/GLU/node combinations are available
    L223.LN3_MgdAllocation_crop %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN3_Leaf"]])) %>%
      unique %>%
      bind_rows(unique(select(L223.LN3_MgdAllocation_bio, one_of(LEVEL2_DATA_NAMES[["LN3_Leaf"]])))) %>%
      # What was a leaf for level3 is now a node, as it will have the 4th level nested under it
      rename(LandNode4 = LandLeaf) %>%
      # Modify land node variable, prepare to separate the GLU name
      mutate(LandNode4 = sub("Root_Tuber", "RootTuber", LandNode4),
             LandNode4 = sub("biomass_tree", "biomasstree", LandNode4),
             LandNode4 = sub("biomass_grass", "biomassgrass", LandNode4)) %>%
      separate(LandNode4, c("LandNode4", "GLU_name")) %>%
      mutate(logit.year.fillout = min(BASE_YEARS),
             # Modify land node variable to match in logit exponent values
             LandNode4 = sub("RootTuber", "Root_Tuber", LandNode4),
             LandNode4 = sub("biomasstree", "biomass_tree", LandNode4),
             LandNode4 = sub("biomassgrass", "biomass_grass", LandNode4)) %>%
      # Match in logit exponent values, use left_join instead because the logit.type variable are NAs, drop later
      left_join(A_LandNode_logit_irr, by = c("LandNode4" = "LandNode")) %>%
      mutate(LandNode4 = paste(LandNode4, GLU_name, sep = "_")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN4_Logit"]],"logit.type")) ->
      L2242.LN4_Logit

    # L2242.LN4_NodeGhostShare:
    # Specify ghost node share for bioenergy node in future years (starting with first bio year).
    # These are the same values that would have been set as ghost share in the leaves in land input 3.
    # We can just copy that data frame and just rename the LandLeaf column to LandNode.
    L223.LN3_LeafGhostShare %>%
      rename(LandNode4 = LandLeaf) %>%
      na.omit ->
      L2242.LN4_NodeGhostShare

    # L2242.LN4_NodeIsGhostShareRel:
    # Specify whether bioenergy ghost node share is relative.
    # These are the same values that would have been set in the leaves in land input 3.
    # We can just copy that data frame and just rename the LandLeaf column to LandNode.
    L223.LN3_LeafIsGhostShareRel %>%
      rename(LandNode4 = LandLeaf) %>%
      na.omit ->
      L2242.LN4_NodeIsGhostShareRel

    # Produce outputs
    L2242.LN4_Logit %>%
      add_title("Logit exponent of the fourth land nest by region") %>%
      add_units("NA") %>%
      add_comments("Only write out the logit exponent for the irrigated/rainfed node competition") %>%
      add_legacy_name("L2242.LN4_Logit") %>%
      add_precursors("aglu/A_LandNode_logit_irr",
                     "temp-data-inject/L223.LN3_MgdAllocation_bio",
                     "temp-data-inject/L223.LN3_MgdAllocation_crop") ->
      L2242.LN4_Logit

    L2242.LN4_NodeGhostShare %>%
      add_title("Ghost node share for bioenergy node in future years, the fourth land nest") %>%
      add_units("NA") %>%
      add_comments("These are the same values set as ghost share in the third land nest leaves") %>%
      add_comments("Copy values from the third land nest and rename LandLeafe to LandNode4") %>%
      add_legacy_name("L2242.LN4_NodeGhostShare") %>%
      add_precursors("temp-data-inject/L223.LN3_LeafGhostShare") ->
      L2242.LN4_NodeGhostShare

    L2242.LN4_NodeIsGhostShareRel %>%
      add_title("Whether bioenergy ghost node share is relative, the forth land nest") %>%
      add_units("NA") %>%
      add_comments("These are the same values set in the the third land nest leaves") %>%
      add_comments("Copy values from the third land nest and rename LandLeafe to LandNode4") %>%
      add_legacy_name("L2242.LN4_NodeIsGhostShareRel") %>%
      add_precursors("temp-data-inject/L223.LN3_LeafIsGhostShareRel") ->
      L2242.LN4_NodeIsGhostShareRel

    return_data(L2242.LN4_Logit, L2242.LN4_NodeGhostShare, L2242.LN4_NodeIsGhostShareRel)
  } else {
    stop("Unknown command")
  }
}
