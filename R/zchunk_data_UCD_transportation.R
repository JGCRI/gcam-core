# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_data_UCD_transportation
#'
#' Dedicated data chunk to read UCD transportation database
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{UCD_trn_data_CORE}, \code{UCD_trn_data_SSP1},
#' \code{UCD_trn_data_CORE}, \code{UCD_trn_data_CORE}, \code{UCD_trn_data_CORE}.
#' @details The \code{UCD_trn_data_CORE.csv} file suddenly, in line 1403, stops
#' using trailing commas for blank columns, and then restarts doing so 672 rows later.
#' This generates a warning in the general purpose \code{\link{load_csv_files}}
#' that we'd rather not suppress universally.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread
#' @author BBL June 2017
module_data_UCD_transportation <- function(command, ...) {

  prefix <- "UCD_trn_data_"
  suffixes <- c("CORE", "SSP1", "SSP2", "SSP3", "SSP5")

  if(command == driver.DECLARE_INPUTS) {
    return(NULL)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(paste0(prefix, suffixes))
  } else if(command == driver.MAKE) {

    UCD_trn_data_CORE <- UCD_trn_data_SSP1 <- UCD_trn_data_SSP2 <-
      UCD_trn_data_SSP3 <- UCD_trn_data_SSP5 <- NULL  # silence package check notes

    for(sfx in suffixes) {
      varname <- paste0(prefix, sfx)
      fqfn <- find_csv_file(paste0("energy/", varname), optional = FALSE, quiet = TRUE)

      suppressWarnings(readr::read_csv(fqfn, comment = COMMENT_CHAR, col_types = "ccccccccddddddd")) %>%
        add_title("Core UC Davis transportation database") %>%
        add_units("Various") %>%
        add_comments(paste("Read from", gsub("^.*extdata", "extdata", fqfn))) %>%
        add_legacy_name(varname) %>%
        add_flags(FLAG_NO_OUTPUT) ->
        x

      assign(varname, x)
    }

    return_data(UCD_trn_data_CORE, UCD_trn_data_SSP1, UCD_trn_data_SSP2,
                UCD_trn_data_SSP3, UCD_trn_data_SSP5)
  } else {
    stop("Unknown command")
  }
}
