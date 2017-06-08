#' module_data_UCD_trn
#'
#' Dedicated data chunk to read \code{Maddison_population.csv} file.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{UCD_trn_data}.
#' @details The \code{UCD_trn_data} files have blank columns, which
#' generates a warning message in the general purpose \code{\link{load_csv_files}}
#' that we'd rather not suppress universally.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author RH
#' @export
module_data_UCD_trn <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(NULL)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("UCD_trn_data"))
  } else if(command == driver.MAKE) {

    fqfn <- find_csv_file(paste0("energy/UCD_trn_data_",energy.TRN_SSP), optional = FALSE, quiet = TRUE)
    cn <- c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel",
            "variable", "unit", as.character(seq(2005,2095,15)))
    ct <- paste0(paste(rep("c", length(cn) - 7), collapse = ""), paste(rep("d", length(cn) - 8), collapse = ""))


    suppressWarnings(readr::read_csv(fqfn, comment = COMMENT_CHAR, col_names = cn, col_types = ct, skip = 5)) %>%
      gather(year, value, `2005`:`2095`) %>%
      mutate(year = as.integer(year)) %>%
      add_title("UCD transportation database") %>%
      add_units("Varied") %>%
      add_comments(paste("Read from", gsub("^.*extdata", "extdata", fqfn))) %>%
      add_legacy_name("UCD_trn_data") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_OUTPUT) ->
      UCD_trn_data

    return_data(UCD_trn_data)
  } else {
    stop("Unknown command")
  }
}
