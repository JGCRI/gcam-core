% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaglu_L113.ag_storage.R
\name{module_aglu_L113_ag_storage}
\alias{module_aglu_L113_ag_storage}
\title{module_aglu_L113_ag_storage}
\usage{
module_aglu_L113_ag_storage(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L113.ag_Storage_Mt_R_C_Y_adj}.
}
\description{
Preparing and processing agricultural storage data
}
\details{
This chunk calculates ag storage values, losses, and costs by GCAM region and commodity.
}
\author{
XZ 2023
}
