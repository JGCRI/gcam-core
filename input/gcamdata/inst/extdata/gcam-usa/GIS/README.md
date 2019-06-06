# README.md

Documentation for these GIS files, from Page Kyle 7 December 2017 (see https://github.com/JGCRI/gcamdata/pull/844#discussion_r155365135 ):

The HDD and CDD data in the model were all produced by Yuyu, and we don't have any archive of the raw data, processing code, etc. You can see from the file names that the future scenarios are from the GFDL climate model, A2 scenario; the historical ones are also from a climate model as well (as opposed to historical or re-analysis data), but the fact that they only go up to 2008 means that the dataset was produced in 2011. And I don't know which climate model was used--my guess is CCSM3. Here's an excerpt from some documentation I wrote up in 2012:

Historical gridded population data for the US are from HYDE (Klein Goldewikj et al. 2011), which has population at the 5’ scale. Global gridded population data for future scenarios are from the IIASA GGI database. These scenarios are available at 0.5 degree latitude by longitude at a 10-year resolution from 2000 to 2100. Historical and future climate data are from three climate models, depending on the scenario being run. These models include CCSM3 (Collins et al. 2006), GISS (Russell et al. 2000), and Hadley (Gordon et al. 2000). These population and climate data are used to generate historical heating and cooling degree days (HDD and CDD), as well as future scenarios.

The method for calculating heating and cooling degree days by state using gridded data of temperature and population over time is documented in Zhou et al. (in review); a brief summary follows. First, the monthly temperature data and estimates of the daily standard deviation in each grid cell, derived from analysis of historical climate data, are used to calculate the monthly HDD and CDD for each grid cell. This step is necessary because climate model output for future scenarios is in monthly timesteps, whereas heating and cooling degree days are calculated from daily temperature. Then, these monthly HDD and CDD are aggregated to annual values.Then, the population-weighted HDD and CDD over a given geographical area (in this case, a state) are calculated as follows:

```
HDD = sum[j](HDD[j]P[j]) / sum[j](P[j])
CDD = sum[j](CDD[j]P[j]) / sum[j](P[j])
```

This is simply the population (_P_)-weighted HDD or CDD over a region that has _j_ grid cells.
