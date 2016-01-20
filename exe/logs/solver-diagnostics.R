require(ggplot2)
require(reshape)
require(RColorBrewer)
require(graphics)
### Create a collection of tables with the following structure
### list: one element for each GCAM model period.  for each period:
###      list:  one element for each variable type, for each variable
###           data frame: values for each market by iteration number

### Note that the ordering (and possibly number) of the markets
### changes each time the solver exits and restarts.
read.trace.log <- function(filename) {
    ## unfortunately, the number of markets in play fluctuates as
    ## markets are added and subtracted from the solvable set. 200
    ## seems to be about the largest it gets.
    dummy <- read.table(filename,sep=',', strip.white=TRUE, skip=3, fill=TRUE,nrows=3)
    nmkt <- ncol(dummy) - 4  # first 4 columns are period, iter, mode, name. Rest are markets
    varn <- seq(1,nmkt)
    colnames   <- c("period", "iter", "mode", "name", varn)
    colclasses <- c("factor", "integer", "factor", "factor", rep("numeric", nmkt))
    rawdata <- read.table(filename, sep=',', strip.white=TRUE, skip=3, fill=TRUE,
                          col.names=colnames, colClasses=colclasses)
    data.by.period <- split(rawdata, rawdata$period)
    lapply(data.by.period,
           function(df) {
               df$period <- NULL
               dfs <- split(df, df$name)
               lapply(dfs, function(d) {d$name <- NULL;d})
           }) 
}

fxcolormap <- function(n=51, controlpts=c(-10,-3,0,3,10)) {
    ### Create colormap for visualizing f(x) values.
    ###
    ### The hue will be piecewise over the four intervals defined by the
    ###  five control points (they need not be equally spaced).  The saturation
    ###  will ramp down from 1 to 0 on the interval from the first to the third 
    ###  control points, then back up to 1 on the rest of the interval.  If 
    ###  centered on 0 (like the default) this will result in a colormap where
    ###  the magnitude of f(x) is represented by the saturation and the hue localizes  
    ###  the value to one of four intervals.
    ###
    ### Inputs:
    ###   n - number of steps in the colormap
    ### controlpts - intervals over which the hue and saturation change.
    ###
    ### Return value:  Vector of colors
    xlo <- controlpts[1]
    x1  <- controlpts[2]
    xm  <- controlpts[3]
    x2  <- controlpts[4]
    xhi <- controlpts[5]
    
    x = seq(0,n-1) * (xhi-xlo)/(n-1) + xlo
    ## Hue is piecewise constant on four intervals
    h1 <- ifelse(x<x1, 0, 90/360)
    h2 <- ifelse(x>x2, 240/360, 180/360)
    H <- ifelse(x<xm, h1, h2)

    ## Use "option 2 for the saturation"
    eps <- 1.0e-8        # protect against roundoff giving vaues slightly larger than 1
    S <- ifelse(x<xm, 1-(x-xlo)/(xm-xlo), (x-xm)/(xhi-xm+eps))

    ## Constant 1 for value
    hsv(H, S, 1)
}

fxtransform <- function(x) {
    ### Transform F(x) values for better visualization
    ###
    ### This transformation is a log scale for small values that switches to a linear 
    ###  scale for larger values.  Magnitudes less than
    ###  the solution tolerance are flushed to zero.  Magnitudes greater than
    ###  that but < 1 are divided by the tolerance, have the base-10 log taken, and
    ###  have their sign preserved.  Magnitudes > 1 are presented linearly (shifted
    ###  to be continuous with the small scale), and magnitudes >10 are clamped.
    ftol  <- 1.0e-3                     # threshold for considering a market "solved"
    signx <- sign(x)
    magx  <- abs(x)
    xx    <- ifelse(magx < ftol, 0,
                    ifelse(magx < 1, log10(magx)-log10(ftol),
                           ifelse(magx < 10, (magx-1)-log10(ftol), 9-log10(ftol))))
    signx*xx                            # return value
}

### Return a transform function that takes the magnitude and clips it to a maximum value
clipped.mag.transform <- function(maxmag=10) {
    function(x) {
        magx <- abs(x)
        ifelse(magx>maxmag, maxmag, magx)
    }
}

### Return a transform that clamps the values to two bounds
clamp.transform <- function(xmin=-100, xmax=100) {
    function(x) {
        pmax(xmin, pmin(x, xmax))
    }
}

### Return a transform function that gives sign(x)*log(x/xmin).
### x-values less than xmin are flushed to zero, and x-values greater
### than xmax are clipped to xmax
signed.log.transform <- function(xmin=1e-4, xmax=100) {
    function(x) {
        signx <- sign(x)
        absx  <- pmax( pmin(abs(x), xmax), xmin)
        signx * log10(absx/xmin)
    }
}

### Transform deltax and deltafx values for better visualization
deltatransform <- function(x, maxmag=10) {
    magx <- abs(x)
    pmin(magx, maxmag)
}

heatmap.gcam <- function(data, xform=identity, colors=c("white","blue"), title="", breaks=waiver()) {
    ### Plot one of the traces in the solver-data-log as a heat map
    ###
    ### Inputs:  
    ###     data  - data frame containing the trace we want to plot (see read.trace.log())
    ###    xform  - transform to apply to the data (for easier interpretation).  Default is
    ###             identity (i.e., no transformation)
    ###    colors - colormap, a vector of color values
    ###    title  - string to print as the title of the plot
    ###    breaks - controls the breaks in the scale legend (see ggplot2::discrete_scale).
    ###             The default here usually does the right thing.
    nmkt <- ncol(data) - 2
    niter <- max(data$iter)
    dm    <- melt(data, id=c("mode","iter"))
    ## turns out this next line only works because 'variable' was
    ## turned into a factor, and the values we want (1..n) are
    ## precisely the integer indices of the factor.  Whatever.  I'll
    ## take it.
    dm$component <- as.integer(dm$variable)   # This variable is the market id
    dm$value <- xform(as.numeric(dm$value))   # This one is the value of whatever we're plotting for that market.

    ggplot(data=dm, aes(x=component, y=iter, fill=value)) + geom_raster() +
        scale_fill_gradientn(colours=colors, na.value="black", breaks=breaks) +
            ggtitle(title) +
                scale_x_continuous(breaks=seq(10,nmkt,10)) + scale_y_continuous(breaks=seq(0,niter,20))
}
  
### calculate a total derivative from deltax and deltafx
calc.total.deriv <- function(deltafx, deltax) {
  dfxmat <- as.matrix(cast(melt(deltafx, id=c("iter","mode")), iter~variable))
  dxmat  <- as.matrix(cast(melt(deltax, id=c("iter","mode")), iter~variable))
  
  as.data.frame(dfxmat/dxmat)
}

####
#### The rest of these functions call heatmap.gcam with some tuned values that
#### (in my experience) produce readily interpretable plots.
####

### create a heat map of a single variable for a single period (i.e.,
### the bottom-level table in the list created by read.trace.log).
### This version is tuned for looking at fx.
heatmap.fx <- function(data, title="", breaks=c(-12, -7, -3, 0, 3, 7, 12)) {
    qp <- fxtransform(quantile(as.matrix(data[-c(1,2)]), c(0,1),na.rm=TRUE))
    q0 <- qp[1]   
    q2 <- 0.0
    q1 <- 0.5*(q0+q2)
    q4 <- qp[2]
    q3 <- 0.5*(q2+q4)    
    
    cp <- c(q0,q1,q2,q3,q4)
    heatmap.gcam(data, xform=fxtransform, colors=fxcolormap(n=51,controlpts=cp), 
                 title=title, breaks=breaks)
}

### heat map for deltax and deltafx
heatmap.delta <- function(data, title="", maxmag=5) {
    heatmap.gcam(data, xform=clipped.mag.transform(maxmag), title=title)
}

### heatmap for total derivative
heatmap.dfdx <- function(data, title="") {
    heatmap.gcam(data, xform=clamp.transform(-100,100),
                 colors=fxcolormap(101,c(-100, -20, 0, 20, 100)), title=title)
}


