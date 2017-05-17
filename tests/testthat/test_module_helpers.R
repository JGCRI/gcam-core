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
