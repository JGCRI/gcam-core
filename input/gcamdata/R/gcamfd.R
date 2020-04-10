#' Calculate food demand using the Edmonds et. al. model
#'
#' The Edmonds model divides food consumption into two categories,
#' \emph{staples}, which represent basic foodstuffs, and \emph{nonstaples},
#' which represent higher-quality foods.  Demand for staples increases at low
#' income, but eventually peaks and begins to decline with higher income.
#' Demand for nonstaples increases with income over all income ranges; however,
#' total (staple + nonstaple) demand saturates asymptotically at high income.
#'
#' @section Usage:
#'
#' To run the interactive version of the model, run the \code{\link{runapp}}
#' function.  This will start the interactive version of the model, which you
#' can use to explore different parameter settings.
#'
#' The API function for running the model is \code{\link{food.dmnd}}.  This
#' function allows you to pass in a parameters structure along with vectors of
#' prices and GDP and to get back a table of quantities and budget fractions.
#'
#' @section Example:
#' \verb{
#' ps <- 0.2
#' pn <- 0.5
#' y <- seq(0.2, 10.0, 0.2)
#' rslt <- food.dmnd(ps, pn, y, samp.params)
#' }
"_PACKAGE"

