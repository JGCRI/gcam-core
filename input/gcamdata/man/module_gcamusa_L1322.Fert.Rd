% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zgcamusa_L1322.Fert.R
\name{module_gcamusa_L1322.Fert}
\alias{module_gcamusa_L1322.Fert}
\title{module_gcamusa_L1322.Fert}
\usage{
module_gcamusa_L1322.Fert(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L1322.out_Mt_state_Fert_Yh}, \code{L1322.IO_GJkg_state_Fert_F_Yh}, \code{L1322.in_EJ_state_Fert_Yh}. The corresponding file in the
original data system was \code{LA1322.Fert.R} (gcam-usa level1).
}
\description{
Calculate input-output intensity coefficients and input energy for state fertilizer production
}
\details{
Calculate input-output intensity coefficients and input energy for state fertilizer production from state shares of national values.
}
\author{
KD September 2017
}
