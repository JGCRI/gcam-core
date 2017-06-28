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

test_that("get_logit_fn_tables", {
  expect_error(get_logit_fn_tables(1, "x", "x", tibble()))
  expect_error(get_logit_fn_tables(tibble(), 1, "x", tibble()))
  expect_error(get_logit_fn_tables(tibble(), "x", 1, tibble()))
  expect_error(get_logit_fn_tables(tibble(), "x", "x", 1))
  expect_error(get_logit_fn_tables(tibble(), "x", "x", include_equiv_table = 1))
  expect_error(get_logit_fn_tables(tibble(), "x", "x", write_all_regions = 1))
  expect_error(get_logit_fn_tables(tibble(), "x", "x", default_logit_type = 1))

  # Structure should be a list with names corresponding to logit types
  d <- tibble(logit.type = c(NA, gcam.LOGIT_TYPES))
  BH <- "BH"
  DLT <- 1
  NAMES <- "logit.type"
  grn <- tibble(region = c("x", "y"), GCAM_region_ID = 1:2)
  d1 <- get_logit_fn_tables(d, names = NAMES, base_header = BH, GCAM_region_names = grn,
                            write_to_all_regions = FALSE,
                            default_logit_type = gcam.LOGIT_TYPES[DLT],
                            include_equiv_table = FALSE)
  expect_type(d1, "list")
  expect_identical(names(d1), gcam.LOGIT_TYPES)

  for(i in seq_along(d1)) {
    # Each list element should have 'header' and 'data' sections
    expect_identical(names(d1[[i]]), c("header", "data"))
    # Header should be correctly named
    expect_identical(d1[[i]]$header, paste0(BH, gcam.LOGIT_TYPES[i]))
    expect_identical(names(d1[[i]]$data), NAMES)
    # Data section should have one row, except for the default, which
    # because of NA in `d` above should hvae two
    if(i == DLT) {
      expect_equal(nrow(d1[[i]]$data), 2)
    } else {
      expect_equal(nrow(d1[[i]]$data), 1)
    }
  }

  # The EQUIV_TABLE should be properly structured
  d1 <- get_logit_fn_tables(d, names = "logit.type", base_header = BH, GCAM_region_names = grn,
                            default_logit_type = gcam.LOGIT_TYPES[DLT],
                            write_to_all_regions = FALSE,
                            include_equiv_table = TRUE)
  expect_identical(names(d1), c(gcam.EQUIV_TABLE, gcam.LOGIT_TYPES))
  expect_identical(d1[[gcam.EQUIV_TABLE]]$header, gcam.EQUIV_TABLE)
  d2 <- d1[[gcam.EQUIV_TABLE]]$data
  # Should be a single row and columns for group.name, dummy-logit-tag, then the other tags
  expect_equal(dim(d2), c(1, length(gcam.LOGIT_TYPES) + 2))
  expect_true(all(gcam.LOGIT_TYPES %in% d2))

  # write_to_all_regions gets called OK
  d1 <- get_logit_fn_tables(d, names = "logit.type", base_header = BH, GCAM_region_names = grn,
                            default_logit_type = gcam.LOGIT_TYPES[DLT],
                            write_to_all_regions = TRUE,
                            include_equiv_table = TRUE)
  expect_identical(names(d1), c(gcam.EQUIV_TABLE, gcam.LOGIT_TYPES))
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
