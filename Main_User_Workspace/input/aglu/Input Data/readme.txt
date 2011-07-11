Page Kyle
April 29, 2011

*****To run the R scripts, first set the "working directory" to your workspace/input/aglu/Input Data*****
In R for Windows, this is under File -> change dir...
Alternatively, if R is launched by double-clicking the .RData file, the working directly will automatically be set correctly.


All data tables (objects) are named; the table names are designed to indicate all information that is contained in the files.

Generally, objects created in R are labeled in the following order: sector_datatype_unit_region_commodity_[other features]_year

Notations are documented here; exceptions are noted below:

Sectors
ag	agricultural product
an	animal product
LC	land cover
LCi	land cover, incomplete in that non-existent combinations are not written out
For	forest

Data types
Prod	production
NetExp	net exports (positive values = net exporter, negative values = net importer)
Supply	domestic supply (production minus net exports; also equal to the sum of all forms of consumption)
Food	consumption by humans
Feed	use as feed
OtherUses	All other uses: includes processing losses, energy/industrial uses, seed, waste, stock changes, and other uses.
ALL	full mass balance table: production, net exports, supply, and all tracked uses (typically consumption by humans, use as feed, and other net uses)
HA	harvested area
Yield	yield (output per unit of harvested land)
EcYield	economic yield (output per unit of cropland)
IO	input-output. FeedIO indicates units of feed per unit of animal production.
LT	land type
LV	Land Value

Units
Mt	megatonnes (same as teragrams)
t	tonnes
Pcal	petacalories
Mcal	megacalories
Ha	hectares
kHa	kilohectares
bm2	billion square meters, or thousand square kilometers
km2	square kilometers
kgm2	kilograms per square meter
kcalg	kilocalories per gram
USD75	1975 $
milUSD75	million 1975$

Regions
R	by GCAM region

Commodities
C	by GCAM commodity
Cc	by GCAM commodity, with cotton kept separate
[commodity name]	indicates a table with only the listed commodity
Cnf	by GCAM commodity, not including fodder crops

Other Features
S	by system (mixed, pastoral)
F	by feed type (feedcrop, fodderherb_residue, pasture_foddergrass, or scavenging_other)

Years
year	all years from source dataset written out
Y	by model year (1990, 2005)
Yh	by historical year, from 1700 to 2005 unless otherwise noted.
Yh1975 	by historical year, stopping in 1975 (1700 to 1975).
AEZ	by AEZ

Additional suffixes
adj	adjusted from original data table of the same name. Some of the data tables not written out as CSVs are differentiated as adj1, adj2...
x	all csv files are ended by the number of the R code file that created them (e.g. LC_bm2_R_26.csv is created by code file #26)

Additional notation used in variable (column) naming in R objects
addl	additional
frac	fraction
pos	positive values only (negative values set to 0)
neg	negative values only (positive values set to 0)
_R	indicates that the values in this vector are specific to a given region
_C	indicates that the values in this vector are specific to a given commodity
_Y	indicates that the values are specific to a given year

The excel workbooks in this folder compile the data from R. To update them, open the file, and go to Data -> Edit Links. Rather than click "update links" (which won't work b/c files are csv's), highlight all of the files in the window, and select "Open Source". This will open all csv's linked by the workbook, and will automatically update the data.

Note that many CSVs in the Rdata_out folder are not imported into any Excel workbooks; these CSVs are written out for diagnostic purposes only.