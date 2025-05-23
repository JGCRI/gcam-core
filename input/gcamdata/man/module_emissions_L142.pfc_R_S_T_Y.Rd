% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zemissions_L142.pfc_R_S_T_Y.R
\name{module_emissions_L142.pfc_R_S_T_Y}
\alias{module_emissions_L142.pfc_R_S_T_Y}
\title{module_emissions_L142.pfc_R_S_T_Y}
\usage{
module_emissions_L142.pfc_R_S_T_Y(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L142.pfc_R_S_T_Yh}. The corresponding file in the
original data system was \code{L142.pfc_R_S_T_Y.R} (emissions level1).
}
\description{
Map HFC emission shares by region, sector, technology, gas, and year for years 1971-2008.
}
\details{
First, a table was created that has all historical HFCs values.
Then, regional and sector information was added.  And finally, HFC emission shares
were calculated by summing emissions over region, sector, technology, and gas by year.
}
\author{
CDL June 2017 and YO February 2020
}
