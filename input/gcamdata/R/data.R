# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' A list of column orderings keyed by the ModelInterface header so that we
#' can ensure tables being sent to be converted to XML by the ModelInterface
#' have their columns arranged in the order the ModelInterface is expecting them.
#'
#' @format A list object where [[header]] <- character vector of column names:
"LEVEL2_DATA_NAMES"

#' GCAM_DATA_MAP
#'
#' There are two levels of information available from the GCAM data system:
#' chunk dependencies, which are available for "free", i.e. with a fast query to
#' each chunk on the part of \code{chunk_inputs} and \code{chunk_outputs},
#' and detailed information on data object-level dependencies. This function is
#' used to generate this latter data, i.e. a tibble of chunk-output-precursor information,
#' which is used by \code{dstrace} and various other graphing and diagnostic utilities.
#' @author BBL
#' @format A tibble with columns: name (chunk that produces the data), output (name of the data object),
#' precursors (colon seperated list of precursors), title, units, comments, flags (typically metadata for processing)
"GCAM_DATA_MAP"

#' A list of prebuilt data objects. These are used when the proprietary IEA
#' energy data files are not available, and thus
#' \code{\link{module_energy_LA100.IEA_downscale_ctry}} is not able to run.
#' Its immediate downstream dependencies then used the prebuilt versions of
#' their outputs stored in this object.
#'
#' @format A list object where [[object_name]] <- tibble:
"PREBUILT_DATA"
