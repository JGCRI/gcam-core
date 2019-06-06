#' A list of column orderings keyed by the ModelInterface header so that we
#' can ensure tables being sent to be converted to XML by the ModelInterface
#' have their columns arranged in the order the ModelInterface is expecting them.
#'
#' @format A list object where [[header]] <- character vector of column names:
"LEVEL2_DATA_NAMES"

#' A list of prebuilt data objects. These are used when the proprietary IEA
#' energy data files are not available, and thus
#' \code{\link{module_energy_LA100.IEA_downscale_ctry}} is not able to run.
#' Its immediate downstream dependencies then used the prebuilt versions of
#' their outputs stored in this object.
#'
#' @format A list object where [[object_name]] <- tibble:
"PREBUILT_DATA"
