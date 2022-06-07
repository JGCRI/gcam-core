#' USER_MOD_NAME
#'
#' Briefly describe what this chunk does.
#'
CHUNK_NAME_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_MODIFY) { # objects that will be modified by the function
    return(MODIFIED_OBJECTS)
  } else if(command == driver.DECLARE_INPUTS) { # objects that are inputted from the data system, but not modified
    return(INPUTS_PATTERN)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -- most likely will use get_data()
    LOAD_PATTERN

    # ===================================================
    # PROCESSING CODE GOES HERE...

    # ===================================================

    # Produce outputs
    # Note that if the MODIFIED_OBJECTS is an input file, we need to assign the full name, for example:
    # return_modified("energy/A322.subsector_shrwt" = A322.subsector_shrwt)
    return_modified("MODIFIED_OBJECT_1" = MODIFIED_OBJECT_1,
                    "MODIFIED_OBJECT_2" = MODIFIED_OBJECT_2)
  } else {
    stop("Unknown command")
  }
}

