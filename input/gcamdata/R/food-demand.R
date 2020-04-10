
## Scale factors used in the food demand model below
psscl <- 100
pnscl <- 20

#' Calculate food demand using the Edmonds, et al. model.
#'
#' The Edmonds model divides food consumption into two categories,
#' \emph{staples}, which represent basic foodstuffs, and \emph{nonstaples},
#' which represent higher-quality foods.  Demand for staples increases at low
#' income, but eventually peaks and begins to decline with higher income.
#' Demand for nonstaples increases with income over all income ranges; however,
#' total (staple + nonstaple) demand saturates asymptotically at high income.
#'
#' @section Arguments:
#'
#' Ps and Pn are food prices (staple food price and nonstaple price) in
#' international dollars per 1000 (dietary) calories.  Y is per-capita GDP in
#' international dollars. Ps, Pn, Y may be vectors but must all be the same
#' length.
#'
#' Params is a structure:
#' \describe{
#' \item{xi}{2x2 array of the xi elasticities}
#' \item{A}{Leading coefficients in the quantity calculations}
#' \item{yfunc}{Length-2 list of functions giving Y^eta(Y) (see note below)}
#' \item{Pm}{Price of ``materials''. Loosely speaking, this parameter controls
#' how valuable food is relative to everything else in the economy.}
#' }
#'
#' Note that we don't need elasticity parameters for the materials component
#' because we calculate Qm as a residual.  That is, whatever portion of
#' household budgets is not spent on food is by definition spent on materials.
#'
#' @section Output:
#'
#' The return value is a data frame with the following elements.
#' \describe{
#'  \item{Qs}{Quantity for staple foods (S)}
#'  \item{Qn}{Quantity for nonstaple foods (N)}
#'  \item{Qm}{Quantity for ``materials'' (M).  This quantity
#' represents an aggregate of everything else besides food that consumers buy.}
#'  \item{alpha.s}{Budget fraction for S}
#'  \item{alpha.n}{Budget fraction for N}
#'  \item{alpha.m}{Budget fraction for M}
#' }
#' Demand for staple and nonstaple foods are given in thousands of dietary
#' calories per person per day.  Units for materials demand are unspecified.
#'
#' @section Income Elasticity Functions:
#'
#' For one of the functional forms used for the income behavior, eta(Y), eta
#' blows up, but Y^(eta(Y)) is well behaved.  Therefore, the eta functions need
#' to be able to calculate not just eta(Y), but Y^(eta(Y)), so they can handle
#' the limiting cases.
#'
#' @param Ps Vector of staple food prices.
#' @param Pn Vector of nonstaple food prices.
#' @param Y Vector of per capita income.
#' @param params Model parameters structure (see details)
#' @param rgn Optional name for this calculation. If provided, the region will
#' be included as an extra column in the output data frame.
#' @export
food.dmnd <- function(Ps, Pn, Y, params, rgn=NULL)
{
    Pm <- params$Pm

    ## Normalize income and prices to Pm
    Ps <- Ps/Pm * psscl
    Pn <- Pn/Pm * pnscl
    Y  <- Y/Pm

  # get eta values
  eta.s <- params$yfunc[[1]](Y,FALSE)
  eta.n <- params$yfunc[[2]](Y,FALSE)

  # Get Y^eta values.  We have to let the eta object calculate them because it may
  # need to do something special near Y=0 or Y=1
  yterm.s <- params$yfunc[[1]](Y,TRUE)
  yterm.n <- params$yfunc[[2]](Y,TRUE)

  ## create the equation that we are going to solve for alpha.  Here alpha is
  ## a 2xN vector, where N is the number of Y values.  alpha[1,] == alpha.s
  ## and alpha[2,] == alpha.n
  falpha <- function(alpha) {
    ## Calculate constant-price elasticities, leave in a list for calc1q below
    eps <- mapply(calc1eps, alpha[1,], alpha[2,], eta.s, eta.n, MoreArgs=list(xi=params$xi),
                  SIMPLIFY=FALSE)
    ## Calculate quantities Q[1,] is Qs and Q[2,] is Qn
    Q <- mapply(calc1q, Ps, Pn, Y, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
    ## alpha.out = P*Q/Y
    alpha.out <- alpha
    alpha.out[1,] <- Ps*Q[1,]/Y / psscl
    alpha.out[2,] <- Pn*Q[2,]/Y / pnscl

    ## output of this function is alpha - alpha.out.  Solving for the roots of this
    ## equation will give us a self-consistent alpha
    alpha - alpha.out
  }
  ## Now, use a nonlinear equation solver to find a consistent set of alpha values.
  alphatest <- matrix(0.01, nrow=2, ncol=length(Y))
  ## Solve for alpha
  rslt <- nleqslv::nleqslv(alphatest, falpha, method='Broyden', control=list(maxit=500))

  ## calculate resulting Q values
  alpharslt <- matrix(rslt$x, nrow=2)
  eps <- mapply(calc1eps, alpharslt[1,], alpharslt[2,], eta.s, eta.n, MoreArgs=list(xi=params$xi),
                SIMPLIFY=FALSE)
  qvals <- mapply(calc1q, Ps, Pn, Y, eps, yterm.s, yterm.n, MoreArgs=list(Acoef=params$A))
  qs <- qvals[1,]
  qn <- qvals[2,]
  ## calculate Qm as the budget residual.
    resid <- Y - (Ps*qs/psscl + Pn*qn/pnscl)
    qm <-  resid / Pm
    alpha.m <- resid / Y

    if(is.null(rgn))
        data.frame(Qs=qs, Qn=qn, Qm=qm, alpha.s=alpharslt[1,], alpha.n=alpharslt[2,], alpha.m=alpha.m)
    else
        data.frame(Qs=qs, Qn=qn, Qm=qm, alpha.s=alpharslt[1,], alpha.n=alpharslt[2,], alpha.m=alpha.m, rgn=rgn)
}

#' Calculate the exponents in the demand equation.
#'
#' This is an approximation to the Slutsky equations, inasmuch as we use these
#' as the exponents directly, instead of solving for exponents that produce
#' these values as the elasticities.
#'
#' @param alpha.s Budget fraction for staple foods.
#' @param alpha.n Budget fraction for nonstaple foods.
#' @param eta.s Income elasticity for staple foods.
#' @param eta.n Income elasticity for nonstaple foods.
#' @param xi Matrix of Hicks elasticities
calc1eps <- function(alpha.s, alpha.n, eta.s, eta.n, xi) {

    ## First apply symmetry condition.  This means that the xi.sn
    ## value will be ignored.  Also, set a floor on the terms to
    ## ensure that the function is well-behaved.
    alphamin <- 0.1
    xi[3] <- max(alpha.n, alphamin)/max(alpha.s,alphamin) * xi[2]

    ## Now calculate the epsilon matrix using the Slutsky equation.
    c(xi[1] - alpha.s * eta.s,    # ess
      xi[2] - alpha.s * eta.n,    # ens
      xi[3] - alpha.n * eta.s,    # esn
      xi[4] - alpha.n * eta.n)   # enn
}


#' Calculate demand quantities for a single set of inputs
#'
#' This is a helper function for the \code{mapply} calls in
#' \code{\link{food.dmnd}}
#'
#' @param Ps Price of staple foods
#' @param Pn Price of nonstaple foods
#' @param Y Per-capita income
#' @param eps Matrix of Marshall elasticities
#' @param Ysterm Income term in the demand equation for staples
#' @param Ynterm Income term in the demand equation for nonstaples
#' @param Acoef Leading multiplier parameter.
calc1q <- function(Ps, Pn, Y, eps, Ysterm, Ynterm, Acoef) {
  ## not vectorized:  use mapply
  Qs <- Acoef[1] * Ps^eps[1] * Pn^eps[3] * Ysterm
  Qn <- Acoef[2] * Ps^eps[2] * Pn^eps[4] * Ynterm

  ## Check the budget constraint
  alpha.s <- Ps*Qs/Y / psscl
  alpha.n <- Pn*Qn/Y / pnscl
  alpha.t <- alpha.s + alpha.n
  food.budget <- 1                      # maximum budget fraction for total food.
  if(alpha.t > 1) {
      ## Food consumption exceeds the budget constraint; reduce
      ## consumption to stay within budget.  Reduce nonstaples first,
      ## since they will normally be a less efficient source of
      ## calories than staples.
      if(alpha.s < food.budget) {
          alpha.n <- food.budget-alpha.s
      }
      else {
          alpha.n <- 0
          alpha.s <- food.budget
      }
      Qs <- alpha.s * Y/Ps
      Qn <- alpha.n * Y/Pn
  }
  c(Qs, Qn)
}

#' Generate an income elasticity function with constant income elasticity.
#'
#' These functions generate a function that can be used as the income elasticity
#' functions for staple or nonstaple foods.  Staple and nonstaple foods each
#' have their own functional forms, given by \code{eta.s} and \code{eta.n}.
#' There is also a constant elasticity function that can be used for testing,
#' though it is not formally part of the model design.
#'
#' #' Income elasticity functions have the following signature:
#' \itemize{
#'  \item{\code{function(Y, calcQ)}}
#' }
#' where Y is the per-capita GDP, and calcQ is a flag indicating whether the
#' function should calcluate the income quantity term or the income elasticity.
#'
#' @param eta0 Value of the constant elasticity.
#' @return A function suitable for use as either \code{eta.s} or \code{eta.n} in
#' the food demand model.
#' @export
eta.constant <- function(eta0) {
  ## Return an eta function where eta is constant with a specified value.  This still
  ## uses the calcQ interface described below
  function(Y, calcQ=FALSE) {
    if(calcQ) {
      Y^eta0
    }
    else {
      eta0
    }
  }
}


#' @describeIn eta.constant Generate an income elasticity function for staple foods.
#'
#' @param nu1 Income elasticity at Y=1
#' @param y0 Value of Y for which elasticity = 0 (this is generally the peak of
#' the curve).
#' @param mc.mode If true, then treat the first two parameters not as nu1 and
#' y0, but as direct specifications of k and lambda.  This flag is necessary
#' becaue nu1 and y0, while more intuitive to work with, are not a complete
#' specification of the parameter space. (That is, there are valid models that can only
#' be specified in terms of k and lambda.)
#' @export
eta.s <- function(nu1, y0, mc.mode=FALSE) {
    if(mc.mode) {
        lam <- nu1
        k <- y0
    }
    else {
        ## validate inputs.  Elasticity goes from + to -, so if y0<1,
        ## nu1<0.  If y0>1, nu1>0.  If these conditions are violated,
        ## then the model doesn't make sense.  To protect against
        ## this, we interpret only the magnitude of nu1 as meaningful,
        ## and we set the sign automatically
        if(y0<=1) {
            ## see below for special handling when y0 = 1
            nu1 <- -abs(nu1)
        }
        else {
            nu1 <- abs(nu1)
        }

        ## We need to caclulate the coefficients k and lambda.  Q = (kY)^(lambda/Y)
        e <- exp(1.0)
        k <- e/y0
        if(abs(1-y0) > 1e-4) {
            lam <- nu1/(1-log(k))
        }
        else {
            ## This case is problematic.  Any value of lambda will
            ## give the requisite value at Y=1, but the shape
            ## parameter is completely undefined.  This is the price
            ## we pay for letting the user specify the shape in more
            ## natural terms.  In this case we reinterpret the nu1
            ## input as the elasticity at Y=e so as to give a
            ## well-defined result.  It's not ideal, but short of
            ## forcing users to calculate k and lambda, it's the best
            ## we can do.  In a MCMC calculation, we'll work with k
            ## and lambda directly.
            lam <- -abs(nu1)*e
        }
    }
    ## Limit as Y->0 of the logarithmic derivative of this function is
    ## not well behaved.  However, the quantity is very small for k*Y
    ## < ~1e-3 anyhow, so we can replace this segment with a linear
    ## relation with little change in behavior.
    y1 <- 1e-3/k
    Qy1 <- (k*y1)^(lam/y1) / y1       # match-up condition:  qty at y=y1, divided by y
    scl <- k^(-lam)                   # scale factor gives Y(1) = 1.
    function(Y,calcQ=FALSE) {
        if(calcQ) {
            scl * ifelse(Y>y1, (k*Y)^(lam/Y),
                         Qy1*Y)
        }
        else {
            ifelse(Y>y1,
                   lam*(1-log(k*Y))/Y,
                   lam*(1-log(k*y1))/y1) # logarithmic derivative of the linear
                                        # segment of Q.
        }
    }
}

#' @describeIn eta.constant Generate an income elasticity function for nonstaple foods.
#' @export
eta.n <- function(nu1) {
    ## Return a function for calculating eta_n or Y^eta_n.  Which one
    ## gets calculated is controlled by the parameter 'calcQ'.

    ## We don't have a mc.mode parameter for this function because it
    ## is well-behaved when specified in terms of nu1, so we just
    ## stick with that.

    ## Arguments:
    ##   nu1 : elasticity at Y=1.  Evidently, k == 2*nu1
    k <- 2*nu1

    function(Y, calcQ=FALSE) {
        e.k <- exp(-k)
        delta <- 1-Y
        scl <- 1/e.k
        if (calcQ) {
            scl*ifelse(abs(delta)>1.0e-3/k,
                       Y^(k/(delta)),
                       e.k - 0.5*k*e.k*delta + 1.0/24.0*e.k * k*(3*k-8)*delta*delta)
        }
        else {
            k * ifelse(Y<1e-4, 1,
                       ifelse(abs(delta) > 1.0e-3/k,
                              1/delta + Y*log(Y)/(delta*delta),
                              0.5 + 1/6*delta + 1/12*delta*delta + 1/20 * delta^3))
        }
    }
}


#' Calculate actual elasticities using numerical derivatives.
#'
#' Given a set of prices and incomes, and model parameters,calculate the
#' elasticities using numerical derivatives.  Optionally, you can pass the model
#' results for the base values, if you've already calculated them.
#'
#' The inputs Ps, Pn, and Y can be vectors, but if they are, they must all be
#' the same length
#'
#' @param Ps Staple food prices
#' @param Pn Nonstaple food prices
#' @param Y Per-capita income
#' @param params Model parameters.  See description in \code{\link{food.dmnd}}.
#' @param basedata Model results for the base values of Ps, Pn, and Y.
#' @export
calc.elas.actual <- function(Ps,Pn,Y, params, basedata=NULL)
{
    if(is.null(basedata)) {
        basedata <- food.dmnd(Ps, Pn, Y, params)
    }

    ## size of finite difference step
    h <- 0.001

    ## Calculate Ps elasticities
    psdelta <- Ps + h
    psh <- 1.0/(psdelta - Ps)           # Using psdelta-ps instead of h helps with roundoff error.
    psdata <- food.dmnd(psdelta, Pn, Y, params)
    eps.ss <- (psdata$Qs - basedata$Qs) * psh * Ps/basedata$Qs
    eps.ns <- (psdata$Qn - basedata$Qn) * psh * Ps/basedata$Qn
    eps.ms <- (psdata$Qm - basedata$Qm) * psh * Ps/basedata$Qm

    ## Calculate Pn elasticities
    pndelta <- Pn + h
    pnh <- 1.0/(pndelta - Pn)
    pndata <- food.dmnd(Ps, pndelta, Y, params)
    eps.sn <- (pndata$Qs - basedata$Qs) * pnh * Pn/basedata$Qs
    eps.nn <- (pndata$Qn - basedata$Qn) * pnh * Pn/basedata$Qn
    eps.mn <- (pndata$Qm - basedata$Qm) * pnh * Pn/basedata$Qm

    ## Calculate Pm elasticities.  Note that Pm is passed through the
    ## parameters structure.
    Pm <- params$Pm
    pmdelta <- Pm + h
    pmh <- 1.0/(pmdelta-Pm)
    ptemp <- params
    ptemp$Pm <- pmdelta
    pmdata <- food.dmnd(Ps, Pn, Y, ptemp)
    eps.sm <- (pmdata$Qs - basedata$Qs) * pmh * Pm/basedata$Qs
    eps.nm <- (pmdata$Qn - basedata$Qn) * pmh * Pm/basedata$Qn
    eps.mm <- (pmdata$Qm - basedata$Qm) * pmh * Pm/basedata$Qm

    ## Calculate income elasticities
    ydelta <- Y + h
    yh <- 1.0/(ydelta - Y)
    ydata <- food.dmnd(Ps, Pn, ydelta, params)
    eta.s <- (ydata$Qs - basedata$Qs) * yh * Y/basedata$Qs
    eta.n <- (ydata$Qn - basedata$Qn) * yh * Y/basedata$Qn
    eta.m <- (ydata$Qm - basedata$Qm) * yh * Y/basedata$Qm

    data.frame(ess=eps.ss, ens=eps.ns, ems=eps.ms, esn=eps.sn, enn=eps.nn, emn=eps.mn,
               esm=eps.sm, enm=eps.nm, emm=eps.mm, etas=eta.s, etan=eta.n, etam=eta.m)

}


#' Calculate the actual Hicks elasticities using the Slutsky equation.
#'
#' Given actual price and income elasticities (i.e., calculated using finite
#' difference derivatives), compute the corresponding Hicks elasticities.
#'
#' @param eps Elasticity values calculated by \code{\link{calc.elas.actual}}
#' @param alpha.s Budget fraction for staples
#' @param alpha.n Budget fraction for nonstaples
#' @param alpha.m Budget fraction for materials
#' @export
calc.hicks.actual <- function(eps, alpha.s, alpha.n, alpha.m)
{
    xi.ss <- eps$ess + alpha.s * eps$etas
    rslt <- data.frame(xi.ss=xi.ss)
    rslt$xi.sn <- eps$esn + alpha.n * eps$etas
    rslt$xi.sm <- eps$esm + alpha.m * eps$etas

    rslt$xi.ns <- eps$ens + alpha.s * eps$etan
    rslt$xi.nn <- eps$enn + alpha.n * eps$etan
    rslt$xi.nm <- eps$enm + alpha.m * eps$etan

    rslt$xi.ms <- eps$ems + alpha.s * eps$etam
    rslt$xi.mn <- eps$emn + alpha.n * eps$etam
    rslt$xi.mm <- eps$emm + alpha.m * eps$etam

    rslt$xi.sn.wt <- rslt$xi.sn * alpha.s
    rslt$xi.ns.wt <- rslt$xi.ns * alpha.n

    rslt
}


#' Tabulate food demand by year for a model.
#'
#' Tabulate food demand by year for in input model using the observed prices and
#' incomes for a given region.  If \code{region} is \code{NULL}, do it for all
#' regions and concatenate the result.
#'
#' This function is intended to generate model predictions that can be compared
#' to observed consumption.
#'
#' @param obsdata Table of observed prices and incomes.
#' @param params Model parameter structure, as described in
#' \code{\link{food.dmnd}}
#' @param bc Vector of regional bias correction factors.  Optional. If
#' omitted, no bias correction will be applied.
#' @param region Region to apply the analysis to.
#' @export
food.dmnd.byyear <- function(obsdata, params, bc=NULL, region=NULL)
{
    . <- GCAM_region_name <- s_usd_p1000cal <- ns_usd_p1000cal <-
        gdp_pcap_thous2005usd <- s_cal_pcap_day_thous <- ns_cal_pcap_day_thous <-
            NULL

    if(is.null(region)) {
        ## run this function for all regions and collect the results
        ## into a single table.
        levels(obsdata$GCAM_region_name) %>%
            lapply(. %>% food.dmnd.byyear(obsdata, params, bc, .)) %>%
            do.call(rbind, .)
    }
    else {
        ## columns to keep in input data.
        selcols <- c('year', 'Ps', 'Pn', 'Y', 'Qs.Obs', 'Qn.Obs')
        if(!is.null(obsdata$obstype))
            selcols <- c(selcols, 'obstype')
        dplyr::filter(obsdata, GCAM_region_name==region) %>%
            dplyr::mutate(Ps=0.365*s_usd_p1000cal, Pn=0.365*ns_usd_p1000cal, Y=gdp_pcap_thous2005usd,
                   Qs.Obs=s_cal_pcap_day_thous, Qn.Obs=ns_cal_pcap_day_thous) %>%
            dplyr::select_(.dots=selcols) -> indata
        rslt <- as.data.frame(food.dmnd(indata$Ps, indata$Pn, indata$Y, params))
        if(!is.null(bc))
            apply.bias.corrections(rslt, bc)
        rslt$year <- indata$year
        rslt$rgn <- region
        rslt$Qs.Obs <- indata$Qs.Obs
        rslt$Qn.Obs <- indata$Qn.Obs
        if(!is.null(indata$obstype))
            rslt$obstype <- indata$obstype
        rslt
    }
}

#' Tabulate food demand by per-capita-income
#'
#' Tabulate staple and nonstaple demand as a function of per-capita GDP.  This
#' function uses observed prices for the comparison
#'
#' @param obsdata Data frame of observed prices and incomes.
#' @param params Model parameter structure.  See notes in
#' \code{\link{food.dmnd}}.
#' @param region Name of a single region to tabulate.  If \code{NULL}, tabulate all
#' regions and concatenate the tables.
#' @export
food.dmnd.byincome <- function(obsdata, params, region=NULL)
{
    . <- GCAM_region_name <- s_usd_p1000cal <- ns_usd_p1000cal <-
        gdp_pcap_thous2005usd <- Ps <- Pn <- s_cal_pcap_day_thous <-
            ns_cal_pcap_day_thous <- region <- Qs <- Qn <- pcGDP <- NULL
    `Staple Residual` <- `Nonstaple Residual` <- NULL

    if(is.null(region)) {
        levels(obsdata$GCAM_region_name) %>% lapply(. %>% food.dmnd.byincome(obsdata, params, .)) %>%
            do.call(rbind, .)
    }
    else {
        od <- dplyr::filter(obsdata, GCAM_region_name == region) %>%
            dplyr::mutate(Ps=0.365*s_usd_p1000cal, Pn=0.365*ns_usd_p1000cal) %>%
                dplyr::select(Y=gdp_pcap_thous2005usd, Ps, Pn,
                       obs.qs=s_cal_pcap_day_thous,obs.qn=ns_cal_pcap_day_thous)
        food.dmnd(od$Ps, od$Pn, od$Y, params) %>%
            dplyr::mutate(region=simplify.region(region), pcGDP=od$Y,
                   `Staple Residual`=Qs-od$obs.qs,
                   `Nonstaple Residual`=Qn-od$obs.qn) %>%
            dplyr::select(region, pcGDP, `Staple Quantity`=Qs, `Nonstaple Quantity`=Qn,
                   `Staple Residual`, `Nonstaple Residual`) %>%
            reshape2::melt(id=c('region','pcGDP'))
    }
}

#' Convert the lambda and ks parameters to nu1 and y0
#'
#' The staple income elasticity has two formulations of its parameters.  The one
#' that uses nu1 (income elasticity at Y==1) and y0 (value of Y for which the
#' elasticity is zero) is more intuitive, but it is incomplete.  That is, theere
#' are some valid models that simply cannot be expressed this way.  The model
#' can alternatively be expressed in terms of the lambda and ks parameters in
#' its equations.  This function allows an easy conversion from the latter
#' representation to the former.
#'
#' @param df Data frame of lambda and ks parameters.
#' @export
lamks2nu1y0 <- function(df)
{
    ## convert the eta.s k and lambda parameters to nu1 and y0
    if(!('lambda' %in% names(df)) || !('ks' %in% names(df))) {
        warning('data frame does not contain lambda & ks vars.')
    }
    else {
        e <- exp(1.0)
        ## add y0 and nu1 columns
        df$y0 <- e / df$ks
        df$eps1s <- df$lambda * (1-log(df$ks))
        ## drop ks and lambda columns.  Arrange for LL to still be at the end
        lltmp <- df$LL
        df$ks <- NULL
        df$lambda <- NULL
        df$LL <- NULL
        df$LL <- lltmp
    }
    df
}


#' Create a merged dataset with training and test data, each labeled accordingly
#'
#' @param obs.trn Data frame of observations in the training set.
#' @param obs.tst Data frame of observations in the testing set.
#' @export
merge_trn_tst <- function(obs.trn, obs.tst)
{
    obs.trn$obstype <- 'Training'
    obs.tst$obstype <- 'Testing'
    rbind(obs.trn, obs.tst)
}


#' Prepare observations for use in the model.
#'
#' Convert units on prices and rename columns with long, wordy names to make
#' them easier to work with.
#'
#' @param obs Data frame of observed data.
#' @export
prepare.obs <- function(obs)
{
    s_usd_p1000cal <- ns_usd_p1000cal <- GCAM_region_name <-
        gdp_pcap_thous2005usd <- s_cal_pcap_day_thous <- ns_cal_pcap_day_thous <-
            NULL

    ## Convert units on prices and rename certain columns so they
    ## aren't such a pain to work with.
    dplyr::mutate(obs,
           Ps=0.365*s_usd_p1000cal, # convert daily cost in dollars to annual cost in thousands of dollars.
           Pn=0.365*ns_usd_p1000cal) %>%
      dplyr::rename(rgn=GCAM_region_name, Y=gdp_pcap_thous2005usd,
               Qs=s_cal_pcap_day_thous, Qn=ns_cal_pcap_day_thous)
}


## Helper function for compute.bias.corrections
compute.bc.rgn <- function(obs, params)
{
    year <- NULL

    ## compute the bias corrections for an individual region.  Bias
    ## corrections are *multiplied* by model data to get adjusted
    ## data.
    yrmax <- max(obs$year)
    yrmin <- yrmax-10
    obs <- dplyr::filter(obs, year > yrmin)

    mod <- food.dmnd(obs$Ps, obs$Pn, obs$Y, params)

    c(s=mean(obs$Qs)/mean(mod$Qs), n=mean(obs$Qn)/mean(mod$Qn))

}

#' Compute regional bias corrections.
#'
#' Compute regional bias correction for a set of parameters and a training set of observations.
#' Bias correction factors are \emph{multiplied} by model data to get bias
#' corrected model output.
#'
#' @param params Model parameter structure (described in
#' \code{\link{food.dmnd}})
#' @param obs.trn Data frame of training observations.
#' @export
compute.bias.corrections <- function(params, obs.trn)
{
    . <- NULL

    obs <- prepare.obs(obs.trn)
    obs <- split(obs, obs$rgn)
    params$bc <- sapply(obs, . %>% compute.bc.rgn(params))
}


## Helper function for apply.bias.corrections
apply.bc.rgn <- function(mod, bc)
{
    mod$Qs <- mod$Qs * bc['s',mod$rgn]
    mod$Qn <- mod$Qn * bc['n',mod$rgn]
    mod
}

#' Apply bias corrections to model outputs
#'
#' @param mod Data frame of model outputs
#' @param bc Vector of regional bias correction factors
#' @export
apply.bias.corrections <- function(mod, bc)
{
    . <- NULL
    ## make this function work whether or not the column names have been converted.
    rgn <- if(is.null(mod$rgn))
        mod$GCAM_region_name
    else
        mod$rgn
    split(mod, mod$rgn) <- lapply(split(mod, mod$rgn),
                                  . %>% apply.bc.rgn(bc))
    mod
}

#' Sample values for the demand model.
#'
#' These variables give examples of the data structures used in the model, as
#' well as baseline price, GDP and parameter values that should produce
#' reasonable results.
#'
#' @name testdata
NULL

#' y.vals: Logarithmically-spaced pcGDP values
#'
#' y.vals: Logarithmically-spaced pcGDP values
#' @rdname testdata
#' @export
y.vals <- 10^seq(-1,log10(50), length.out=20)

#' Ps.vals: Evenly spaced Ps values
#'
#' Ps.vals: Evenly spaced Ps values
#' @rdname testdata
#' @export
Ps.vals <- 10^seq(-2,log10(2.5),length.out=20)
#' Pn.vals: Evenly spaced Pn values
#'
#' Pn.vals: Evenly spaced Pn values
#' @rdname testdata
#' @export
Pn.vals <- 10^seq(-2, log10(2.5), length.out=20)
#' Pm.vals: Evenly spaced Pm values
#'
#' Pm.vals: Evenly spaced Pm values
#' @rdname testdata
#' @export
Pm.vals <- rep(1.0, length(Ps.vals))


#' samp.params: Example model parameter structure
#'
#' samp.params: Example model parameter structure
#' @rdname testdata
#' @export
samp.params <- list(A=c(0.3, 0.1),
                    yfunc=c(eta.s(-0.15,0.6), eta.n(1.0)),
                    xi=matrix(c(-0.05, 0.1, 0.1, -0.5), nrow=2),
                    Pm=1
                    )

#' x1: Monte carlo parameter vector
#'
#' x1: Monte carlo parameter vector.  This vector encodes the same model as
#' \code{samp.params}, but using the Monte Carlo version of the parameter
#' formulation for the staple income elasticity model.
#' @rdname testdata
#' @export
x1 <- c(0.3, 0.1, -0.05, 0.1, -0.5, 1.0, 0.2936423, 4.5304697, 1)
#' x0: Parameters used to generate the test data
#'
#' x0: Parameters used to generate the test data
#' @rdname testdata
#' @export
x0 <- c(0.5, 0.35, -0.03, 0.01, -0.4, 0.5, 0.1442695, 5.436564, 1)
