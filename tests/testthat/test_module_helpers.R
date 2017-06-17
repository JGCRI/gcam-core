# Test the module helper utilities

context("module_helpers")

test_that("set_water_input_name", {

  # Bad inputs
  expect_error(set_water_input_name(1, character(), tibble()))
  expect_error(set_water_input_name(character(), 1, tibble()))
  expect_error(set_water_input_name(character(), character(), 1))
  expect_error(set_water_input_name(character(), character(), tibble(), 1))

  not_irr <- "not_irrigation"
  water_mapping <- tibble(water.sector = c(IRRIGATION, not_irr),
                          supplysector = c("water_td_irr", "water_td_pri"))

  # Mapped GLU
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type <- c(MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2],
                  MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  GLU <- c("GLU01", "GLU02", "GLU03", "GLU04")
  res <- expect_is(set_water_input_name(water_sector, water_type, water_mapping, GLU), "character")
  expect_equal(length(res), length(water_sector))

  # No GLU, but no mapped water types in irrigation
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type <- c("x", "y", MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  res <- expect_is(set_water_input_name(water_sector, water_type, water_mapping), "character")
  expect_equal(length(res), length(water_sector))
  expect_equal(water_type[1:2], res[1:2])   # should have original names

  # No GLU - error
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type = c(MAPPED_WATER_TYPES[1], "y", MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  expect_error(set_water_input_name(water_sector, water_type, water_mapping))
})

test_that("rename_SO2", {

  # Bad inputs
  expect_error(rename_SO2(1, tibble()))
  expect_error(rename_SO2(tibble(), 1))
  expect_error(rename_SO2(tibble(), tibble(), 1))

  so2_map <- tibble(region = letters[1:3], SO2_name = 1:3)

  for(awb in c(TRUE, FALSE)) {
    # No SO2 in x - no change to x
    x <- tibble(region = so2_map$region, Non.CO2 = "x")
    expect_silent(res <- rename_SO2(x, so2_map, is_awb = awb))
    expect_equal(res, x)

    # No SO2_AWB when awb TRUE; no SO2 when awb FALSE
    x <- tibble(region = so2_map$region, Non.CO2 = paste0("SO2", if_else(awb, "", "_AWB")))
    expect_silent(res <- rename_SO2(x, so2_map, is_awb = awb))
    expect_equal(res, x)

    # region and SO2 name looked up correctly
    x <- tibble(region = rev(so2_map$region), Non.CO2 = paste0("SO2", if_else(awb, "_AWB", "")))
    expect_silent(res <- rename_SO2(x, so2_map, is_awb = awb))
    expect_equal(nrow(res), nrow(x))
    expect_identical(res$Non.CO2, paste0(rev(so2_map$SO2_name), if_else(awb, "_AWB", "")))
  }
})

test_that("set_years", {
  expect_error(set_years(1))
  d <- tibble(x = c("start-year", "final-calibration-year", "final-historical-year",
                    "initial-future-year", "initial-nonhistorical-year", "end-year"))
  d1 <- set_years(d)
  expect_identical(d1$x, as.character(c(min(BASE_YEARS),
                                        max(BASE_YEARS),
                                        max(HISTORICAL_YEARS),
                                        min(FUTURE_YEARS),
                                        min(MODEL_YEARS[MODEL_YEARS > max(HISTORICAL_YEARS)]),
                                        max(FUTURE_YEARS))))
})

test_that("set_traded_names", {
  expect_error(set_traded_names(1, TRUE))
  expect_error(set_traded_names(tibble(), 1))
  # There should be a 'traded' column
  expect_warning(set_traded_names(tibble(trad = 0), letters, apply_selected_only = FALSE))

  # correctly replaces region name
  d <- tibble(traded = gcam.USA_CODE + 0:2, region = paste0("r", 0:2))
  d1 <- set_traded_names(d, letters, apply_selected_only = TRUE)
  expect_identical(d1$region, as.character(c(letters[gcam.USA_CODE], "r1", "r2")))  # "a", r1, r2
  d1 <- set_traded_names(d, letters, apply_selected_only = FALSE)
  expect_identical(d1$region, rep(letters[gcam.USA_CODE], 3))  # "a", "a", "a"

  # correctly replaces subsector column
  d$subsector = paste0("ss", seq_len(nrow(d)))
  d1 <- set_traded_names(d, letters, apply_selected_only = TRUE)
  expect_identical(d1$subsector, c("r0 ss1", "ss2", "ss3"))

  # correctly replaces technology column
  d$technology = paste0("t", seq_len(nrow(d)))
  d1 <- set_traded_names(d, letters, apply_selected_only = TRUE)
  expect_identical(d1$technology, c("r0 t1", "t2", "t3"))
})
