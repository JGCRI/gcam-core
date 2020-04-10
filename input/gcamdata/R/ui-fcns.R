#### Layout functions for the GUI app.

#' Constants used in the GUI
#'
#' @name guiconst
#' @keywords internal
NULL

#' xidefault : Default xi values
#' @rdname guiconst
#' @export
xidefault <- c(-0.18, -0.33, 0.21)
#' elasmin : Minimum elasticity for the slider bar
#' @rdname guiconst
#' @export
elasmin <- -2
#' elasmax : Maximum elasticity for the slider bar
#' @rdname guiconst
#' @export
elasmax <- 2
#' elasstep : Elasticity bar step size
#' @rdname guiconst
#' @export
elasstep <- 0.01
#' etastep : Eta slider bar step size
#' @rdname guiconst
#' @export
etastep <- 0.05
#' spacer : Spacer for labels
#' @rdname guiconst
#' @export
spacer <-
    if(requireNamespace('shiny', quietly=TRUE)) {
        shiny::HTML(paste0(rep('&nbsp;',5),collapse=''))
    } else {
        ## I guess this could cause issues if you load the package without shiny
        ## installed, then install it, then try to run the GUI, but Ima risk it.
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
    }


## A possibly-interesting parameter set (for the variable exponent model):
## xi <- c(-0.1, 0.05, 0.01, -0.5)
## eta <- c(-0.1, 1.5)
## y0 <- 0.5
## Q <- c(0.4,0.2)

## Max likelihood params from paper
## xi <- c(-0.18, 0.21, 0.21, -0.33)
## etas :  y0 = 0.171, nu1 = -0.169
## etan : nu1 = 0.49
## A <- c(1.28, 1.14)
## Pm <- 5.05

## Possibly interisting parameter set for the constant exponent model:
## xi <- c(-0.15, 0.1, 0.1, -0.6)
## eta <- c(-0.3, 0.3)
## Q <- c(0.55, 0.05)

#' Input widgets for food demand GUI app
#'
#' @name guiwidgets
#' @keywords internal
NULL

#' @describeIn guiwidgets Input boxes for xi matrix
#' @export
xi.matrix.input <- function()
{
    ## Draw the xi input boxes

    shiny::fluidRow(
        shiny::column(4,
               shiny::numericInput(inputId="xiss", value=xidefault[1],label="\\(\\xi_{ss}\\)",
                            min=elasmin, max=elasmax, step=elasstep)),
        shiny::column(4,
               shiny::numericInput(inputId="xinn", value=xidefault[2], label="\\(\\xi_{nn}\\)",
                            min=elasmin, max=elasmax, step=elasstep)),
        shiny::column(4,
               shiny::numericInput(inputId="xicross", value = xidefault[3], label='\\(\\xi_{\\text{cross}}\\)',
                            min=elasmin, max=elasmax, step=elasstep))
        )
}


#' @describeIn guiwidgets Input boxes for y0
#' @export
y0.input.box <- function()
{
  shiny::tags$table(
    shiny::tags$tr(
      shiny::tags$td(
          shiny::numericInput(inputId='y0val', label='Y\\(_0\\)',width='75px',
                              0.5, 0.1, 10, 0.1))))
}


#' @describeIn guiwidgets Draw input grid for other inputs
#' @param inputids Character vector of parameter identifiers
#' @param defvals Vector of default values
#' @param min Minimum value
#' @param max Maximum value
#' @param step Slider step size
#' @param labels Character vector of labels
#' @export
column.input.table <- function(inputids, defvals, min, max, step, labels=c('Staple','Nonstaple'))
{
    ## Draw an input table with a single column of two values
    ##  inputids and defvals are vectors
    ##  min, max, and step are single values.
    shiny::tags$table(
        shiny::tags$tr(
            shiny::tags$th(spacer),
            shiny::tags$td(shiny::numericInput(inputId=inputids[1], value=defvals[1], label=labels[1],
                                               min=min, max=max, step=step))
            ),
        shiny::tags$tr(
            shiny::tags$th(spacer),
            shiny::tags$td(shiny::numericInput(inputId=inputids[2], value=defvals[2], label=labels[2],
                                               min=min, max=max, step=step))
            )
        )
}


#' @describeIn guiwidgets Draw selector widget for eta
#' @export
eta.selector <- function(id,label2='\\(\\eta=f(Y)\\)',sel=1)
{
  eta.choices <- c(1,2)
  names(eta.choices) <- c('constant \\(\\eta\\)', label2)
  shiny::radioButtons(inputId=id,label='',
                      choices= eta.choices, selected=sel,
                      inline=TRUE)
}
