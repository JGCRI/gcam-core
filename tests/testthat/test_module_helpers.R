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
  # There should be a 'traded' column if apply_selected_only
  expect_warning(set_traded_names(tibble(trad = 0), letters, apply_selected_only = TRUE))

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

test_that("write_to_all_regions", {
  expect_error(write_to_all_regions(1, "x", tibble()))
  expect_error(write_to_all_regions(tibble(), 1, tibble()))
  expect_error(write_to_all_regions(tibble(), 1, 1))
  expect_error(write_to_all_regions(tibble(), "x", tibble(), has_traded = 1))
  expect_error(write_to_all_regions(tibble(), "x", tibble(), apply_selected_only = 1))
  expect_error(write_to_all_regions(tibble(), "x", tibble(), set_market = 1))

  # Fills out data correctly
  d <- tibble(x = 1)
  nms <- c("logit.year.fillout", "price.exp.year.fillout", "market.name", "GCAM_region_ID", "region")
  grn <- tibble(region = c("x", "y"), GCAM_region_ID = 1:2)
  d1 <- write_to_all_regions(d, nms, grn)
  expect_equal(nrow(d1), nrow(grn) * nrow(d))
  expect_identical(names(d1), nms)

  # Replaces fillout and market fields correctly
  d <- tibble(logit.year.fillout = NA, price.exp.year.fillout = NA, market.name = NA)
  d1 <- write_to_all_regions(d, nms, grn)
  expect_identical(d1$market.name, d1$region)

  # Handles has_traded correctly
  d <- tibble(traded = 1)
  nms <- c("traded", "GCAM_region_ID", "region")
  d1 <- write_to_all_regions(d, nms, grn, has_traded = TRUE)
  expect_equal(nrow(d1), nrow(grn) * nrow(d))
})

test_that("add_node_leaf_names", {
  d <- tibble(Land_Type = 1:2, GLU = c("G1", "G2"))
  nt <- tibble(LN1 = 1, LN2 = 2, UnmanagedLandLeaf = d$Land_Type)
  ln <- "UnmanagedLandLeaf"

  expect_error(add_node_leaf_names(1, nt))
  expect_error(add_node_leaf_names(d, 1))
  expect_error(add_node_leaf_names(d, nt, 1))
  expect_error(add_node_leaf_names(d, nt, "1"))  # leaf name not in nesting_table
  expect_error(add_node_leaf_names(d, nt, ln, LT_name = 1))
  expect_error(add_node_leaf_names(d, nt, ln, LT_name = "1"))  # LT_name not in data
  expect_error(add_node_leaf_names(d, nt, ln, append_GLU = 1))

  d1 <- add_node_leaf_names(d, nt, ln)
  expect_equal(ncol(d) + 2, ncol(d1))
  expect_equal(nrow(d), nrow(d1))
  expect_true(ln %in% names(d1))
  expect_true("LandAllocatorRoot" %in% names(d1))

  d1 <- add_node_leaf_names(d, nt, ln, "LN1")
  expect_equal(ncol(d) + 3, ncol(d1))
  expect_true("LN1" %in% names(d1))
  expect_equal(d1$LN1, paste(nt$LN1, d$GLU, sep = aglu.LT_GLU_DELIMITER))

  d1 <- add_node_leaf_names(d, nt, ln, "LN1", append_GLU = FALSE)
  expect_equal(ncol(d) + 3, ncol(d1))
  expect_true("LN1" %in% names(d1))
  expect_equal(d1$LN1, nt$LN1)  # no GLU appended

  d1 <- add_node_leaf_names(d, nt, ln, "LN1", "LN2", append_GLU = FALSE)
  expect_true("LN1" %in% names(d1))
  expect_true("LN2" %in% names(d1))
  expect_equal(d1$LN1, nt$LN1)
  expect_equal(d1$LN2, nt$LN2)

  d$Irr_Rfd <- c("I1", "I2")
  d1 <- add_node_leaf_names(d, nt, ln, "LN1", "LN2", append_GLU = FALSE)
  expect_equal(d1[[ln]], paste(d$Land_Type, d$Irr_Rfd, sep = aglu.IRR_DELIMITER))
})


test_that("append_GLU", {
  expect_error(append_GLU(1))
  expect_silent(append_GLU(tibble()))

  d <- tibble(GLU = "GLU000", x = 1, y = 2)
  expect_equal(d, append_GLU(d))
  d1 <- append_GLU(d, "x")
  expect_equal(d1$x, paste(d$x, d$GLU, sep = aglu.LT_GLU_DELIMITER))
  d1 <- append_GLU(d, "x", "y")
  expect_equal(d1$x, paste(d$x, d$GLU, sep = aglu.LT_GLU_DELIMITER))
  expect_equal(d1$y, paste(d$y, d$GLU, sep = aglu.LT_GLU_DELIMITER))
  expect_error(append_GLU(d, "z"))
})

test_that("replace_GLU", {
  d <- tibble(GLU = "GLU000")
  map <- tibble(GLU_name = "x", GLU_code = "GLU000")
  badmap1 <- tibble(GLU_name = "x", GLU_code = c("GLU000", "GLU001"))
  badmap1 <- tibble(GLU_name = c("x", "y"), GLU_code = "GLU000")

  expect_error(replace_GLU(1, map))
  expect_error(replace_GLU(tibble(), map))  # no GLU column in d
  expect_error(replace_GLU(d, tibble()))  # no GLU_code and GLU_name columns in map
  expect_error(replace_GLU(d, badmap1))
  expect_error(replace_GLU(d, badmap2))
  expect_error(replace_GLU(d, GLU, GLU_pattern = 1))

  d1 <- replace_GLU(d, map)
  expect_equal(dim(d), dim(d1))
  expect_equal(d1$GLU, "x")  # one way...

  d1 <- replace_GLU(tibble(GLU = "x"), map)
  expect_equal(d1$GLU, "GLU000") # ...and the other
})
