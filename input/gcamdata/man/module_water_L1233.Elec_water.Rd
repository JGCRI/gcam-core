% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zwater_L1233.Elec_water.R
\name{module_water_L1233.Elec_water}
\alias{module_water_L1233.Elec_water}
\title{module_water_L1233.Elec_water}
\usage{
module_water_L1233.Elec_water(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L1233.out_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.in_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.wdraw_km3_R_elec}, \code{L1233.wcons_km3_R_elec}, \code{L1233.shrwt_R_elec_cool_Yf}. The corresponding file in the
original data system was \code{L1233.Elec_water.R} (water level1).
}
\description{
Water consumption and withdrawals for electricity.
}
\details{
Categorizes electricity generating technologies by cooling water type, and computes water withdrawals and consumption.
}
\author{
SWDT May 2017
}
