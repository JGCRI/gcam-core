devtools::load_all()
library(rgcam)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)

# Update the productivity assumption values by querying GCAM output for the calibrated values.
#
# Note that anytime a user runs with "FixedGDP-Path" set to 1 the model will calibrate a total
# factor productivity to match the exogenous GDP pathway that they described.  The gcamdata system
# supports generating a number of GDP pathway "scenarios", typically SSPs.  In addition the the
# gcamdata system will combine user assumptions about total factor productivity, defined in
# `socioeconomics/gcam_macro_TFP_open`., with the GDP "scenarios" so that users can also run
# "FixedGDP-Path" set to 0.
#
# However, the productivity assumptions may not exactly replicate the fixed GDP pathways if
# other assumptions or structure has changed significantly.  In which case the generated GDP
# pathways may diverge from their orignaly pathway.  In such a case a user may want to
# re-calibrate the total factor productivity assumptions.
#
# This is where this script fits in.  It will query a set of XMLDB results for new total factor
# productivity values and update the `socioeconomics/gcam_macro_TFP_open` assumptions file.  Given
# gcamdata supports multiple scenarios we set up a map gcamdata scenario name to the user's scenario
# name and path to the database where new values can be found.  By default this map is set up to
# replicate the structure of the GCAM validation runs: scenarios are named GCAM_<SSP> and each
# one is written to its own XMLDB (using the configuration option `append-scenario-name="1`).  However,
# users do not need to have new results for all scenarios.  Instead this script will do partial updates
# to `socioeconomics/gcam_macro_TFP_open` for which ever scenarios a user does indeed have results for.
# In addition we will tag the new results with `git describe` to help users understand from which version
# of GCAM were the productivity values in gcam_macro_TFP_open last updated.
#
# NOTE: this script only goes as far as updated the `socioeconomics/gcam_macro_TFP_open` assumptions file.
# A user will still need to re-run the gcamdata system (ideally with driver_drake to speed up the process)
# to produce the updated XML inputs for GCAM.

# Set up a table to map GCAM DB output including:
# - The scenario name in the DB
# - The path and name of the database to find the scenario
# And map that to the socioeconomic scenario names used in gcamdata
tibble(db_scenario_name = c(paste0("gSSP", 1:5), paste0("GCAM_SSP", 1:5), "GCAM3")) %>%
  mutate(ds_scenario_name = gsub('GCAM_', '', db_scenario_name),
         db_path = "../../output",
         db_name = paste0("database_basexdb", db_scenario_name)) ->
  SCENARIO_DB_MAP
# The Core "GCAM" scenario is really gSSP2
SCENARIO_DB_MAP[SCENARIO_DB_MAP$ds_scenario_name == "gSSP2", c("db_scenario_name", "db_name")] <- list("db_scenario_name" = "GCAM", "db_name" = "database_basexdbGCAM")

# The MI query which will be used to get the new productivity values
PRODUCTIVITY_QUERY <-
'<supplyDemandQuery title="total productivity">
    <axis1 name="account">account[@name]</axis1>
    <axis2 name="Year">nationalAccount[@year]</axis2>
    <xPath buildList="true" dataName="productivity" group="false" sumAll="false">nationalAccount/account[@name="total-factor-productivity"]/node()</xPath>
    <comments/>
</supplyDemandQuery>
'

# We will use `git describe` to tag the values to help users understand
# when the productivity values were last updated
GIT_DESCRIPTION <- system2("git", "describe", stdout = TRUE)

# Find and load the existing productivity dataset as we may only be updating
# a subset of the scenarios
DS_PROD_INPUT_NAME <- "socioeconomics/gcam_macro_TFP_open"
DS_PROD_INPUT_FN <- find_csv_file(DS_PROD_INPUT_NAME, FALSE)
DS_PROD_INPUT_HEADER <- find_header(DS_PROD_INPUT_FN)
DS_PROD_INPUT <- load_csv_files(DS_PROD_INPUT_NAME, FALSE)[[1]]

# query the DBs to get new productivity data
query_db <- function(db_scenario_name, ds_scenario_name, db_path, db_name) {
  # expect errors as we expect partial updates (i.e., not all DBs will exist)
  tryCatch(
  {
    # turn off validatedb given we are already checking for errors
    # to reduce the number of messages
    localDBConn(db_path, db_name, validatedb = FALSE) %>%
      runQuery(PRODUCTIVITY_QUERY, db_scenario_name, c()) %>%
      spread(account, value) %>%
      # don't worry about checking for no results as the tryCatch will
      # handle that case too
      mutate(scenario = ds_scenario_name,
             year = as.integer(year),
             gcam.version = GIT_DESCRIPTION) %>%
      # TODO: keep the rundate?
      select(scenario, region, year, productivity = `total-factor-productivity`, gcam.version)
  },
  # in case of error just the return an empty tibble with the correct columns
  error = function(e) {
    warning(paste0("No results for: ", db_scenario_name, " in ", db_path, "/", db_name))
    tibble(scenario = character(),
           region = character(),
           year = integer(),
           productivity = numeric(),
           gcam.version = character())
  }
  )
}

# loop over scenarios and call query_db, then combine results into a single tibble
SCENARIO_DB_MAP %>%
  group_by_all() %>%
  mutate(data = list(query_db(db_scenario_name, ds_scenario_name, db_path, db_name))) %>%
  pull(data) %>%
  bind_rows() ->
  updated_data

# update the values in the existing input file
DS_PROD_INPUT %>%
  filter(!scenario %in% unique(updated_data$scenario)) %>%
  bind_rows(updated_data) ->
  new_productivity_input

# save the updated data back keeping the header information
conn <- file(DS_PROD_INPUT_FN, open = "w")
cat(DS_PROD_INPUT_HEADER, file = conn, sep = "\n")
write.csv(new_productivity_input, file = conn, quote = FALSE, row.names = FALSE)
close(conn)
