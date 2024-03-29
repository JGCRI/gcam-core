% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zgcamusa_xml_indenergy_emissions.R
\name{module_gcamusa_indenergy_emissions_xml}
\alias{module_gcamusa_indenergy_emissions_xml}
\title{module_gcamusa_indenergy_emissions_xml}
\usage{
module_gcamusa_indenergy_emissions_xml(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{indenergy_emissions_USA.xml}. The corresponding file in the
original data system was \code{indenergy_emissions_USA.xml} (gcamusa XML)
}
\description{
Construct XML data structure for \code{indenergy_emissions_USA.xml}.
}
