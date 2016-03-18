# Running the Diagnostic Scripts

## GCAM diagnostics

1. Run the model verification queries on the output of at least one scenario. It is recommended to running the batch queries to generate CSV files directly instead of XLS.  If writing directly to CSV you can skip to step 5.
2. Split runs into separate sheets, and put all queries within the same sheet (don't generate figures.) A typical 14-region scenario takes 15 minutes to write the data out, and a typical 30-region run takes about 30 minutes.
3. Open the XLS file that was created, and save each scenario as its own CSV. Insert a row above each query, put the name of the query above the column called "scenario" (column B), and delete the column called "title". This will put the data into a format similar to what the R code is expecting.
4. Open the .CSV files in a text editor, and delete the string of commas trailing the query name at the top of each table. Every query will have the same number of trailing commas so a find and replace is pretty simple.
5. Open the diagnostics.R script. If R was not already open, this will by default set the working directory to the correct path. Otherwise, set the working directory to this folder (setwd() function, or select from a drop-down menu).
6. Set the FILES to the correct names of the CSV files in the folder that was created above. Any number of scenarios can be set here; the default is to use two, for a pair-wise comparison.
7. Set the BASE_SCENARIO_NAME to whatever the "reference" is going to be. This is the name of the scenario in the model output, not the file name, though for convenience it may make sense to name the CSV files exactly according to their scenario names.
8. Set the OUTPUT_DIR where the figures will be placed. By default this is figures.
9. Run the script:
source("diagnostics.R")


## Solver diagnostics

The solver diagnostics scripts use detailed data logged by the solver
to explore the solver's progress (hopefully) toward a solution.  If a
period fails to solve, you can use this information to try to figure
out which markets were misbehaving and why.

### Setup

To use the solver diagnostics you first need to change the log
configuration to produce the diagnostic output, since it is turned off
by default.  The solver logs can be quite large, so you should turn
them on only when you are trying to diagnose a problem.  To turn on
the logging output, edit the log configuration file (`log_conf.xml` by
default).  There are two logs that need to be turned on:
```xml
	<Logger name="solver-data-log" type="PlainTextLogger">
		<FileName>logs/solver-data-log.txt</FileName>
		<printLogWarningLevel>0</printLogWarningLevel>
		<minLogWarningLevel>6</minLogWarningLevel>
		<minToScreenWarningLevel>6</minToScreenWarningLevel>
		<headerMessage>{date}:{time}</headerMessage>
	</Logger>
	<Logger name="solver-data-key" type="PlainTextLogger">
		<FileName>logs/solver-data-key.txt</FileName>
		<printLogWarningLevel>0</printLogWarningLevel>
		<minLogWarningLevel>6</minLogWarningLevel>
		<minToScreenWarningLevel>6</minToScreenWarningLevel>
		<headerMessage>{date}:{time}</headerMessage>
	</Logger>
```
The first of these will hold the solver data; the second provides a
key that translates market ID numbers to market names.  Both logs
produce their output at log level 0 (`DEBUG`), so change the
`minLogWarningLevel` for both of these logs to 0.  Leave everything
else alone.


### Reading and working with the log data

Once you're logging the diagnostic data, you will be able to use the
analysis functions to examine the data.  These functions are located
in the `solver-diagnostics.R` file in this directory.  They are
designed to be used in an interactive R session, so they don't run as
a script to save a fixed set of plots to files like the gcam
diagnostics do, though one could use them as building blocks for such
scripts.

To use the data, start an R session, preferably in the subdir with
the logger data.  Source the `solver-diagnostics.R` file in your
session to load the analysis functions.  The `read.trace.log` function
reads the log data and formats it into tables for analysis:
```R
> alldata <- read.trace.log('solver-data-log.txt', 'solver-data-key.txt')
```
This function takes two arguments.  The first is the name of the log
file with solver data.  The second is the name of the log file with
the key for translating market ID numbers to market names.  This key
file is technically optional; if you don't supply it all the markets
will be named "UNKNOWN".

The data read in will be stored in a master list with one entry for
each period.
```R
> names(alldata)
 [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21"
> per10 <- alldata[[10]]
```
Each period's entry is a list of tables, one for each variable
recorded.
```R
> names(per10)
[1] "deltafx"  "deltax"  "demand"  "diagB"  "fx"  "price"  "supply"  "x"
```
Finally, the table for each variable holds the data recorded for that
variable.
```R
> head(per10$supply)
   period iter variable mktid solvable      value               mktname
1:     10    0   supply     1     TRUE  85.971000            globalcoal
2:     10    0   supply     2     TRUE 140.093000       globalcrude oil
3:     10    0   supply     3     TRUE 205.286000     globalnatural gas
4:     10    0   supply     4     TRUE   0.330125 USAunconventional oil
5:     10    0   supply     5     TRUE   0.289339         globaluranium
6:     10    0   supply     6     TRUE 215.478000         globalbiomass
```
The three gcam variables, **price**, **supply**, and **demand** have
data for all markets, solvable and unsolvable.  The "solvable" column
in the table indicates whether or not a market is solvable.  The
remaining variables are used internally by the solver and will contain
entries only for the solvable markets.  All of these values will be
from the *beginning* of the iteration, except for the "delta"
variables, which represent a change from the beginning to the end of
the iteration.
+ **x**: The input values for the solver function.  These are related
  to the market prices, but they will generally be scaled to approximately
  unit value.  They may also be logarithmic, though we don't often use
  this capability in the solver anymore.
+ **fx**:  The output values for the solver function.  These are
  related to the market excess demands, but like the prices they may
  be scaled.
+ **deltax**:  The change in x over the iteration.
+ **deltafx**: Change in fx over the iteration.
+ **diagB**: The diagonal element of the Jacobian matrix.  This is the
  partial derivative of the excess demand in a market w.r.t. the price
  of that same market.
  
In addition to these variables, there are functions to compute
additional derived variables.  Once computed, these variables can be
used and referred to in the same way as the recorded variables.

Most of the functions supplied in the diagnostics will take as an
argument either the data for an entire period (`per10` in our
example), or the data for a single variable for a period
(`per10$supply`, for example).

### Heatmap plots

The `heatmap.gcam()` function produces a heatmap plot from an input
dataset.  Markets are plotted on the x-axis (by market ID), and
solver iterations are plotted on the y-axis.  Each cell at location
*(i,j)* is colored by the value of market *i* at iteration *j*.  Any
of the variables described in the previous section can be used as the
variable to plot.  The arguments to `heatmap.gcam` are:
+ **data**: The data for the variable to plot.  Example:
  `per10$price`.  This argument is the only mandatory argument; all of
  the others have defaults.
+ **xform**: A transform to apply to the data, generally so as to make
  the plot easier to interpret.  There are a variety of functions in
  the package to generate transforms; they are described in their own
  section below.  The default is the identity transform, which leaves
  the data values unchanged.
+ **colors**: Colormap.  This is a vector of color values to use in
  mapping the numerical data to colors.
+ **title**: Title to display at the top of the plot.
+ **breaks**: controls the breaks in the scale legend.  You can almost
  always use the default here.
+ **solvable.only**: for price, supply, and demand, the default is to
  plot all markets, solvable and unsolvable.  Setting this argument
  `TRUE` causes only the solvable markets to be plotted.  This
  argument has no effect for the solver-internal variables because
  they only have data for solvable markets.

The return value is a ggplot plot object.  If you print this object,
either explicitly, or implicitly by executing the function from a
command prompt without assigning its result to a variable, R will
display the plot.  The object can also be assigned to a variable to be
displayed later or modified using the usual ggplot modifiers.

The package also supplies several convenience function that call
`heatmap.gcam()` with values for the optional arguments that produce
readily interpretable plots.  All of these functions require the data
argument and accept the title argument.  Some of them accept
additional (always optional) arguments, which are noted in their
descriptions.
+ **heatmap.fx**:  For plotting fx values.  Accepts the optional
  argument `ftol`, which should be set to the solution tolerance used
  in the run.  The default value for `ftol` is 1e-3.
+ **heatmap.delta**:  For plotting deltax and deltafx values.  Accepts
  the optional parameter `maxmag`, which is used to initialize a
  `clipped.mag.transform` (described below).  The default value for
  `maxmag` is 5.
+ **heatmap.dfdx**:  for plotting total derivitaves.  This is a derived
  variable, described below.

### Line plots

The `plotvars` function makes line plots of variable values by
iteration.  These plots are presented as a grid of panels, with each
panel containing the plot for one variable.  Each line shows the
values for a single market.  Because there are so many markets, you
will need to specify a vector of market ID numbers to include in the
plot.  Only the first two arguments are mandatory.
+ **data**: Data for an entire period.
+ **mktids**: Vector of market id numbers.  Including more than about
  10 will usually result in a plot with too many lines to see any of
  them clearly.  Using more than 12 will cause line colors to repeat.
  Several functions described below return a vector suitable to use
  for this argument.
+ **title**: Title to display at the top of the plot.
+ **skip**: Number of iterations to skip at the beginning of the
  period.  The first few iterations often have transient behavior that
  you might want to exclude from the plots.  The default is not to
  skip any iterations.
+ **transforms**: Named list of transforms to apply to one or more
  variables in the data.  The items in the list must be named for the
  variables they will be applied to.  For example:  
  ```R
  plotvars(mydata, somemarkets,
     transforms = list(fx=fxtransform(), deltafx=cmt3, deltax=cmt3))
  ```
  This will apply the default `fxtransform` (q.v. below) to the **fx**
  variable and `cmt3` (presumably defined elsewhere by the user) to
  both deltafx and deltax.  Other variables will not be transformed.
+ **use.names**:  If `TRUE`, display market names in the plot legend.
  Otherwise, display market ID numbers.  The default is to display
  names.
  
### Analysis and information functions

The functions in this section allow you to find markets with specified
characteristics or get further information about the data.

* **mkt.names(perdata, mktids)**:  Translate market ID numbers to names.
  * *perdata*: Full dataset for a period.
  * *mktids*: Vector of market ids to look up.
* **mkt.grep(perdata, pattern, ...)**: Find market IDs for markets whose
  names match the given regular expression.  The return value is an
  integer vector of matching market IDs.  The names attribute of the
  vector is set to the names of the matching markets.  It is *not*
  necessary to strip this attribute from the vector before passing it
  to a function expecting a vector of market IDs.
  * *perdata*: Full dataset for a period.
  * *pattern*: Character string containing the pattern to search
    for.
  * *...*: Any additional arguments will be passed to the R `grep`
    function.  Particularly useful examples include `ignore.case` and `fixed`.  
  ```R
  > example <- mkt.grep(per10,'africa.*biomassoil',ignore.case=T)
  > example
    Africa_Easternregional biomassOil Africa_Northernregional biomassOil Africa_Southernregional biomassOil  Africa_Westernregional biomassOil    South Africaregional biomassOil 
                               566                                649                                732                                815                               2397 
  > as.integer(example)
  [1]  566  649  732  815 2397
  ```
  * **final.mkt.extremes(vardata, nmkt, final.iter, findmax)**: Find
    markets with maximum (or, optionally, minimum) absolute values of
    the input variable in the final iteration.  A named vector of
    market ID numbers (suitable for use in functions like `plotvars`)
    is returned.  All of the arguments beyond the first are optional.  
	* *vardata*: Data for a single variable to use in the analysis.
	* *nmkt*: Number of markets to return.  The default is 5.
	* *final.iter*: If set to `FALSE`, do the calculation on the
      penultimate iteration instead of the final iteration.  The
      default is to use the final iteration.  This option is left over
      from early versions and is now mostly obsolete.  
	* *findmax*: If `TRUE`, find the markets with the largest absolute
      values of the input variable.  Otherwise find the markets with
      the smallest values.  The default is to find maximum values.  
  Example:  
  ```R
  > final.mkt.extremes(per10$fx)
              Indiageothermal               Chinageothermal            Pakistangeothermal       Europe_Non_EUgeothermal South KoreaDDGS and feedcakes 
                          234                           155                           298                           208                           379 
  ```
  These five markets were had the five largest (in absolute value)
  f(x) values, making them the furthest from solving.
  * **overall.mkt.extremes(vardata, nmkt, skip, findmax)**: Find the
    markets with the absolute values of the L2 norm of the input
    variable over all iterations.  The return value is a named vector
    of market ID numbers.  All of the arguments beyond the first are
    optional.  
	* *vardata*: Data for a single variable to use in the analysis.
	* *nmkt*: Number of markets to return.  The default is 5.
	* *skip*: Number of iterations to skip at the beginning.  The
      default is 0.  
	* *findmax*: If `TRUE` find the markets with the largest L2 norm.
      Otherwise find the markets with the smallest values.  The
      default is to find the maximum.  

Example:  
  ```R
  > overall.mkt.extremes(per10$fx, skip=5)
                 Indiageothermal                  Chinageothermal South America_Northerngeothermal          Europe_Non_EUgeothermal         MexicoDDGS and feedcakes 
                             234                              155                              336                              208                              275 
  ```
  
  
### Transformation functions

Often it is convenient to apply some transformation to a variable that
we are analyzing.  For example, we might apply a log transformation,
or a transformation that clamps values to some range of interest.  Any
function that takes a vector of input values and returns a vector of
transformed values may be used in any of the analysis functions that
expect a transformation.  The solver diagnostics package defines
several custom transformations that have proven to be useful for
displaying solver trace data.  

The transforms are supplied as generator functions that take one or
more optional arguments that tune the behavior of the transform.  The
generator function returns a closure function that is suitable for use
anywhere a transform is expected.
* **fxtransform(ftol)**: Hybrid signed log-linear function.  Values
  with magnitude less than `ftol` are flushed to zero.  Magnitudes
  between ftol and 1 are log-transformed, retaining their sign.
  Magnitudes between 1 and 10 are presented linearly, and magnitudes
  greater than 10 are clamped.
  * *ftol*: Solution tolerance.  Default = 1e-3.
* **clipped.mag.transform(maxmag)**: Absolute value transform, with
  magnitudes greater than `maxmag` clamped.
  * *maxmag*: Maximum magnitude for the output.  Default = 10.
* **clamp.transform(xmin, xmax)**: Clamp values to a specified range.
  Values between the bounds are unchanged.
  * *xmin*: Lower bound.  Default = -100.
  * *xmax*: Upper bound.  Default = 100.
* **signed.log.transform(xmin, xmax)**:  Return sign(x)*log(x/xmin).
  * *xmin*: Minimum magnitude.  Values with smaller magnitudes are
    flushed to zero.  Default = 1e-3.
  * *xmax*: Maximum magnitude.  Larger values are clamped.  Default
    = 100.
Example:
```R
> mytransform <- signed.log.transform(xmin = 1e-5, xmax=1)
> heatmap.gcam(per10$deltax,xform=mytransform, colors=c("red","white","blue"))
```

These commands will produce a heatmap of the `deltax` variable.
Magnitudes below 1e-5 will be flushed to zero; those above 1 will be
set to 1.  The output values will run from -5 (values <= -1), shown in
red, to +5 (values >=1), shown in blue.  Values near the midrange
of the data will be shown in white.  (Note that although the limits
are symmetric around 0, the data need not be, so there is no guarantee
that the middle color will correspond to white.)

### Other notes

Output is currently logged only from the Broyden's Method solver.
Other solvers, such as the Bisection solver, happen invisibly to the
solver diagnostics.  Each time the Broyden solver exits the iteration
count is incremented with no data output.  These iteration gaps show
as gaps in the heatmap plots.  You will often observe dramatic
jumps in data after one of these events.  The line graphs just draw a
straight line through these gaps, so they aren't apparent in those
plots.
