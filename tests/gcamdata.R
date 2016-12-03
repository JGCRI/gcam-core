library(testthat)

# In this rewrite, we're not going to put X's in front of years,
# nor are we going to spend time unnecessarily reshaping datasets
# (i.e. wide to long and back). But we still need to be able to
# verify old versus new datasets! So record what forms data are in
# so it can be reshaped to old form by test code

LONG_NO_X_FORM <- "L100.gdp_mil90usd_ctry_Yh"


test_check("gcamdata")
