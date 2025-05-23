% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-helpers.R
\name{reduce_mgd_carbon}
\alias{reduce_mgd_carbon}
\title{reduce_mgd_carbon}
\usage{
reduce_mgd_carbon(data, LTfor = c(aglu.FOREST_NODE_NAMES), LTpast = "Pasture")
}
\arguments{
\item{data}{Input data tibble to adjust carbon densities for}

\item{LTfor}{Land_Type name to use for Forest land types}

\item{LTpast}{Land_Type name to use for Pasture land types}
}
\value{
The original table with carbon density adjusted for the managed land types
}
\description{
Reduce the carbon density of a managed land type from its unmanaged land
type's carbon density using constant multipliers
}
