# Test driver_drake

context("driver_drake")

#setup
devtools::load_all()
library(drake)
test_cache <- new_cache()
xml_dir <- paste0(normalizePath(file.path("../..", XML_DIR), winslash = "/"), "/" )

###########
 test_that("driver_drake runs with no errors",{
   expect_error(driver_drake(stop_after = c("module_water_L145.water_demand_municipal"), xmldir = xml_dir, cache = test_cache), NA)
 })

##########
 test_that("catches bad input", {
   expect_error(driver_drake(return_inputs_of = 1))
   expect_error(driver_drake(return_outputs_of = 1))
   expect_error(driver_drake(return_data_names = 1))
   expect_error(driver_drake(return_data_map_only = 1))
   expect_error(driver_drake(return_plan_only = 1))
   expect_error(driver_drake(quiet = 1))
 })

 ###########
 test_that("plan is a dataframe",{
   plan <- driver_drake(stop_before = c("module_water_L145.water_demand_municipal"), return_plan_only = TRUE)
   expect_is(plan, "data.frame")
  })


 ############
 test_that("load_from_cache works", {
   data <- load_from_cache(outputs_of("module_water_L145.water_demand_municipal"))

   if(!require("gcamdata.compdata", quietly = TRUE)) {
     # We couldn't get the "OLD" outputs from the gcamdata.compdata repo
     # so we will skip this test
     skip("gcamdata.compdata package not available")

   } else {

     #function to round digits
     DIGITS <- 3
     round_df <- function(x, digits = DIGITS) {

       integer_columns <- sapply(x, class) == "integer"
       x[integer_columns] <- lapply(x[integer_columns], as.numeric)

       numeric_columns <- sapply(x, class) == "numeric"
       x[numeric_columns] <- round(x[numeric_columns], digits)

       # expect_equivalent no longer accepts rows in different order but we
       # do want to allow this so we will sort all columns before testing
       # arrange_columns <- select(x, dplyr::everything())
       # arrange(x, arrange_columns)

       return(x)
     } #end round_df

     #loop over tibbles in data
     for(data_name in names(data)) {

       old_data <- get_comparison_data(data_name)
       expect_is(old_data, "data.frame", info = paste("No comparison data found for", data_name))

       #compare old data and data from cache - ignore order
       expect_true(all.equal(round_df(old_data), round_df(data[[data_name]]), ignore_col_order = TRUE, ignore_row_order = TRUE))
     }
   }
 })

#######
test_that("outdated targets are correct", {
  #get drake history before
  hist_orig <- drake_history(cache = test_cache)

  #get row and then time of last build of target L145.municipal_water_R_W_Yh_km3
  row <- tail(which(hist_orig$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  origtime_mw <- hist_orig[[row, 3]]

  #for target Maddison_population
  row <- tail(which(hist_orig$target == "Maddison_population"), n=1)
  origtime_rr <- hist_orig[[row, 3]]

  #copy file that we edit to get it back later
  example_file <- find_csv_file("water/FAO_municipal_water_AQUASTAT", FALSE)[[1]]
  file.copy(from = example_file, to = paste0(example_file, ".bak"))

  #Duplicate last row of file (with nonzero value)
  cat("\nZimbabwe,181,Municipal water withdrawal,4251,2017,0.4877,,", file = example_file, append = TRUE)

  #run driver_drake
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)

  #get build history and save time of last build of downstream target
  hist1 <- drake_history(cache = test_cache)
  row <- tail(which(hist1$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  time_mw1 <- hist1[[row, 3]]

  #of other target
  row <- tail(which(hist1$target == "Maddison_population"), n=1)
  time_rr1 <- hist1[[row, 3]]

  #compare
  expect_equal(origtime_rr, time_rr1)
  expect_false(identical(origtime_mw, time_mw1))

  ##do it again
  #Duplicate last row of file (with ZERO)
  cat("\nZimbabwe,181,Municipal water withdrawal,4251,2017,0,,", file = example_file, append = TRUE)
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)

  hist2 <- drake_history(cache = test_cache)
  row <- tail(which(hist2$target == "L145.municipal_water_R_W_Yh_km3"), n=1)
  time_mw2 <- hist2[[row, 3]]

  #test to see if its changed since previous build
  expect_equal(time_mw1,time_mw2)
  identical(time_mw1,time_mw2)

  #get file back and remove copy, rerun driver_drake
  file.rename(paste0(example_file, ".bak"), example_file)
  driver_drake(stop_after = "module_water_L145.water_demand_municipal", xmldir = xml_dir, cache = test_cache)
}) #end test



