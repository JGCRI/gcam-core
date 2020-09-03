# Test driver_drake

context("driver_drake")

# Setup
hasdrake <- FALSE
if(require('drake')){
  hasdrake <- TRUE
  test_cache <- new_cache()
}
xml_dir <- paste0(normalizePath(file.path("../..", XML_DIR), winslash = "/"), "/" )

test_that("driver_drake runs with no errors",{
  if(!hasdrake) {
    skip("No drake package - skipping test")
    }
  expect_error(driver_drake(stop_after = c("module_water_L145.water_demand_municipal"), xmldir = xml_dir, cache = test_cache), NA)
})

test_that("catches bad input", {
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   expect_error(driver_drake(return_inputs_of = 1))
   expect_error(driver_drake(return_outputs_of = 1))
   expect_error(driver_drake(return_data_names = 1))
   expect_error(driver_drake(return_data_map_only = 1))
   expect_error(driver_drake(return_plan_only = 1))
   expect_error(driver_drake(quiet = 1))
 })

test_that("plan is a dataframe",{
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   plan <- driver_drake(stop_before = c("module_water_L145.water_demand_municipal"), return_plan_only = TRUE)
   expect_is(plan, "data.frame")
})

test_that("load_from_cache works", {
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   data <- load_from_cache(outputs_of("module_water_L145.water_demand_municipal"))

   if(!require("gcamdata.compdata", quietly = TRUE)) {
     # We couldn't get the "OLD" outputs from the gcamdata.compdata repo
     # so we will skip this test
     skip("gcamdata.compdata package not available")

   } else {

     # function to round digits
     DIGITS <- 3
     round_df <- function(x, digits = DIGITS) {

       integer_columns <- sapply(x, class) == "integer"
       x[integer_columns] <- lapply(x[integer_columns], as.numeric)

       numeric_columns <- sapply(x, class) == "numeric"
       x[numeric_columns] <- round(x[numeric_columns], digits)

       return(x)
     } # end round_df

     # Loop over tibbles in data
     for(data_name in names(data)) {

       old_data <- get_comparison_data(data_name)
       expect_is(old_data, "data.frame", info = paste("No comparison data found for", data_name))

       # Compare old data and data from cache
       expect_equivalent(round_df(old_data), round_df(data[[data_name]]))
       }
   }
})

test_that("outdated targets are correct", {
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
  # Get drake history before
  hist_orig <- drake_history(cache = test_cache)

  # Get row and last build time of target L145.municipal_water_R_W_Yh_km3
  row <- tail(which(hist_orig$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  origtime_mw <- hist_orig[[row, 3]]

  # for target Maddison_population
  row <- tail(which(hist_orig$target == "Maddison_population"), n=1)
  origtime_rr <- hist_orig[[row, 3]]

  # Copy file that we edit to get it back later
  example_file <- find_csv_file("water/FAO_municipal_water_AQUASTAT", FALSE)[[1]]
  file.copy(from = example_file, to = paste0(example_file, ".bak"))

  # Duplicate last row of file (with nonzero value)
  cat("\nZimbabwe,181,Municipal water withdrawal,4251,2017,0.4877,,", file = example_file, append = TRUE)

  # Run driver_drake
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)

  # Get build history and save time of last build of downstream target
  hist1 <- drake_history(cache = test_cache)
  row <- tail(which(hist1$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  time_mw1 <- hist1[[row, 3]]

  # of other target
  row <- tail(which(hist1$target == "Maddison_population"), n=1)
  time_rr1 <- hist1[[row, 3]]

  # Compare
  expect_equal(origtime_rr, time_rr1)
  expect_false(identical(origtime_mw, time_mw1))

  # Do it again
  # Duplicate last row of file (with zero)
  cat("\nZimbabwe,181,Municipal water withdrawal,4251,2017,0,,", file = example_file, append = TRUE)
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)

  hist2 <- drake_history(cache = test_cache)
  row <- tail(which(hist2$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  time_mw2 <- hist2[[row, 3]]

  # Test to see if its changed since previous build
  expect_equal(time_mw1,time_mw2)
  identical(time_mw1,time_mw2)

  # Get file back and remove copy, rerun driver_drake
  file.rename(paste0(example_file, ".bak"), example_file)
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)
}) # end test



