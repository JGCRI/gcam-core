% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaglu_L100.GTAP_downscale_ctry.R
\name{module_aglu_L100.GTAP_downscale_ctry}
\alias{module_aglu_L100.GTAP_downscale_ctry}
\title{module_aglu_L100.GTAP_downscale_ctry}
\usage{
module_aglu_L100.GTAP_downscale_ctry(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L100.GTAP_LV_milUSD}, \code{L100.GTAP_capital_stock},
\code{L100.GTAPCostShare_ResourceRefine_GCAMReg_share}. The corresponding file in the
original data system was \code{LA100.GTAP_downscale_ctry.R} (aglu level1).
}
\description{
Downscale GTAP region-level land value data to all countries and get GTAP VFA data.
}
\details{
This chunk downscales the GTAP region-level land value to all countries
based on production share by GLU and GTAP commodity class.
}
\author{
RC April 2017 XZ 2023 March
}
