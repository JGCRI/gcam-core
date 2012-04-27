This folder contains an estimate of global emissions trends, complied from various sources, for two purposes:

1) Produce a historical input file for the MAGICC+ component of GCAM.
2) Enable comparisons of near-term GCAM results with historical trends.

The historical trends are, in general constructed by using country emissions data where available (OECD countries) and using EDGAR data where these were not available. EDGAR trends are used to interpolate where needed. While compiling data from a more exhaustive list of sources would be preferable (was was done for the RCP process), this methodology was used because it can easily be updated with a minimum of effort.

In some cases this may lead to biases. The recent EDGAR SO2 data for China, for example, are much higher than, for example, the more detailed estimate of Lu and Streets (2011). 2005 - 2008 SO2 emissions are actually likely to be flater than shown in these trends.

Under normal GCAM operation, point to the Default_Emissions_Input.xml file. Historical emissions up to 2005 will be read into MAGICC. GCAM emissions will be used from 2010 onward (with interpolation in-between).

GCAM can also be run in a "MAGICC only" mode by creating an input with the last historical year parameter set to 2100. It can be useful to also add Delete Regions.xml to the input set in this case so that only the climate model runs.

______________________________________________________________________
DATA DETAILS
The "Input Data" folder contains data for six gases:  CH4, CO, N2O, NOx, NMVOC and SO2.  EDGAR data are emissions estimates while UNFCCC data are individually complied for each country.  There are also other data sources for specific countries (EPA emissions data for the US, for example).  All of this data are in "Raw Data" spreadsheets.

The raw data are linked to the "Tot Em (Intermediate)" sheets.  Individual country names are aggregated based on IEA country names.  The IEA names are always the same regardless of the database that is being considered.  Values are combined into the IEA country names in the "Tot Em" sheets using SUMIF functions.  The original country names for each database may differ, but the same 143 IEA country names are always used in all "Tot Em" sheets.

Ratio to EDGAR.xls calculates the ratio between the data source (UNFCCC or Other) and EDGAR.  Countries that have a green font color are special cases.  They have "Other" data supplementing and/or replacing UNFCCC data and may have additional values outside the 1990-2005 range as well, depending on where data may exist.

Ratio Interpolation.xls uses the "Ratio to EDGAR" values for OECD countries where they exist.  Light blue boxes represent existing values, while pale green boxes are interpolated values.  Interpolation values equal to the last known value are carried back or forward in time.  There is also a Other to UNFCCC ratio column to show that the Other values are close to the reported UNFCCC values in most cases.  All other countries are hidden and given a value of 1.00 for each year.  These countries therefore, in effect, always use EDGAR values.  EDGAR Ratio Analysis.xls compares these ratios for CO and draws conclusions about USA and UK ratios, where consistent country-estimates are available back to 1970.

The "Tot Em - Composite" sheet uses the same formula for all countries in all years:  EDGAR emissions * the ratio, where the ratio is derived either from UNFCCC or an "Other" source.

Core model global emissions are queried and located in Tot Em - Core Model.xls.  These numbers are compared to the emissions values that are in the composite results sheet.  The "Data Comparison" folder also contains graphs of composite emissions compared to individual GCAM and RCP data points.

