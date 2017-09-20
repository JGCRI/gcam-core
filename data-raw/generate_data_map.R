library(devtools)

#' GCAM_DATA_MAP
#'
#' There are two levels of information available from the GCAM data system:
#' chunk dependencies, which are available for "free", i.e. with a fast query to
#' each chunk on the part of \link{\code{chunk_inputs}} and \link{\code{chunk_outputs}},
#' and detailed information on data object-level dependencies. This function is
#' used to generate this latter data, i.e. a tibble of chunk-output-precursor information,
#' which is used by \link{\code{dstrace}} and various other graphing and diagnostic utilities.
#' @author BBL
GCAM_DATA_MAP <- driver(return_data_map_only = TRUE)

# Save the DATA_MAP into the internal data folder
devtools::use_data(GCAM_DATA_MAP, overwrite = TRUE, internal = TRUE)
