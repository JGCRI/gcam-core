# pipeline-helpers.R

# Pipeline shortcuts we use a lot
# (Conceptually analagous to helper functions)

# Make sure year and value are numeric, and within historical years
PH_year_value_historical <- function(d) {
  d %>%
    mutate(year = as.numeric(year),
           value = as.numeric(value)) %>%
    filter(year %in% HISTORICAL_YEARS)
}
