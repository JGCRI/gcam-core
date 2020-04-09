# Test the module helper utilities

context("module_helpers")

test_that("set_water_input_name", {

  # Bad inputs
  expect_error(set_water_input_name(1, character(), tibble()))
  expect_error(set_water_input_name(character(), 1, tibble()))
  expect_error(set_water_input_name(character(), character(), 1))
  expect_error(set_water_input_name(character(), character(), tibble(), 1))

  not_irr <- "not_irrigation"
  water_mapping <- tibble(water.sector = c(water.IRRIGATION, not_irr),
                          supplysector = c("water_td_irr", "water_td_pri"))

  # Mapped GLU
  water_sector <- c(water.IRRIGATION, water.IRRIGATION, not_irr, not_irr)
  water_type <- c(water.MAPPED_WATER_TYPES[1], water.MAPPED_WATER_TYPES[2],
                  water.MAPPED_WATER_TYPES[1], water.MAPPED_WATER_TYPES[2])
  GLU <- c("GLU01", "GLU02", "GLU03", "GLU04")
  res <- expect_is(set_water_input_name(water_sector, water_type, water_mapping, GLU), "character")
  expect_equal(length(res), length(water_sector))

  # No GLU, but no mapped water types in irrigation
  water_sector <- c(water.IRRIGATION, water.IRRIGATION, not_irr, not_irr)
  water_type <- c("x", "y", water.MAPPED_WATER_TYPES[1], water.MAPPED_WATER_TYPES[2])
  res <- expect_is(set_water_input_name(water_sector, water_type, water_mapping), "character")
  expect_equal(length(res), length(water_sector))
  expect_equal(water_type[1:2], res[1:2])   # should have original names

  # No GLU - error
  water_sector <- c(water.IRRIGATION, water.IRRIGATION, not_irr, not_irr)
  water_type = c(water.MAPPED_WATER_TYPES[1], "y", water.MAPPED_WATER_TYPES[1], water.MAPPED_WATER_TYPES[2])
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
  expect_identical(d1$x, c(min(MODEL_BASE_YEARS),
                           max(MODEL_BASE_YEARS),
                           max(HISTORICAL_YEARS),
                           min(MODEL_FUTURE_YEARS),
                           min(MODEL_YEARS[MODEL_YEARS > max(HISTORICAL_YEARS)]),
                           max(MODEL_FUTURE_YEARS)))

  # Handles an empty tibble
  expect_silent(set_years(tibble()))
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

test_that("get_ssp_regions", {
  # catches bad input
  expect_error(get_ssp_regions(1, tibble(), "1"))
  expect_error(get_ssp_regions(tibble(), 1, "1"))
  expect_error(get_ssp_regions(tibble(), tibble(), 1))
  expect_error(get_ssp_regions(tibble(), tibble(), "1", ssp_filter = 1))
  expect_error(get_ssp_regions(tibble(), tibble(), "1", year_filter = "1"))

  ssp <- formals(get_ssp_regions)$ssp_filter
  yr <- formals(get_ssp_regions)$year_filter

  pcGDP <- tibble(GCAM_region_ID = 1:3,
                  scenario = ssp,
                  year = eval(yr),
                  value = gdp_deflator(1990, eval(yr)) *
                    c(aglu.LOW_GROWTH_PCGDP - 1,
                      aglu.HIGH_GROWTH_PCGDP + 1,
                      mean(c(aglu.LOW_GROWTH_PCGDP, aglu.HIGH_GROWTH_PCGDP))))

  reg_names <- tibble(GCAM_region_ID = 1:3, region = letters[1:3])

  # Should return a single low/high/medium value when year and scenario match
  expect_equal(get_ssp_regions(pcGDP, reg_names, income_group = "low"), reg_names$region[1])
  expect_equal(get_ssp_regions(pcGDP, reg_names, income_group = "high"), reg_names$region[2])
  expect_equal(get_ssp_regions(pcGDP, reg_names, income_group = "medium"), reg_names$region[3])

  # Should error with unknown income_group
  expect_error(get_ssp_regions(pcGDP, reg_names, income_group = "zzz"))

  # No matches if we change year and/or scenario
  expect_equal(get_ssp_regions(pcGDP, reg_names, income_group = "low", year_filter = eval(yr) - 1), character(0))
  expect_equal(get_ssp_regions(pcGDP, reg_names, income_group = "high", ssp_filter = paste0(ssp, "x")), character(0))
})

test_that("fill_exp_decay_extrapolate", {
  # test basic functionality
  test_data <- tibble(technology = "lawnmower", `1984` = 900, `1986` = 1000, improvement.rate = 0.4, improvement.max = 0.6)
  test_years <- c(1985, 2000)
  test_data %>% fill_exp_decay_extrapolate(test_years) ->
    calc_data
  expect_equal(unique(calc_data$year), test_years)
  expect_equal(calc_data %>% filter(year == 1985) %>% select(value), tibble(value = 950))
  expect_equal(calc_data %>% filter(year == 2000) %>%
                 select(value), tibble(value = 1000 * 0.6 + (1000-1000 * 0.6) * (1.0-0.4) ^ (2000 - 1986)))

  # test improvment.rate/max error checking
  expect_error(test_data %>% mutate(improvement.rate = NA) %>% fill_exp_decay_extrapolate(test_years))
  expect_error(test_data %>% mutate(improvement.max = NA) %>% fill_exp_decay_extrapolate(test_years))
  expect_error(test_data %>% select(-improvement.rate) %>% fill_exp_decay_extrapolate(test_years))
  expect_error(test_data %>% select(-improvement.max) %>% fill_exp_decay_extrapolate(test_years))
  test_data %>% mutate(improvement.rate = NA, improvement.max = NA) %>%
    fill_exp_decay_extrapolate(test_years) ->
    calc_data
  expect_equal(calc_data %>% filter(year == 1985) %>% select(value), tibble(value = 950))
  expect_equal(calc_data %>% filter(year == 2000) %>% select(value), tibble(value = as.numeric(NA)))

  # test shadowing
  tibble(technology="riding mower", `1984` = 1400, `1986` = 1500, improvement.rate = 0.5,
         improvement.max = 0.9, improvement.shadow.technology = "lawnmower") %>%
    bind_rows(test_data) ->
    test_data
  test_data %>% fill_exp_decay_extrapolate(test_years) ->
    calc_data
  expect_equal(calc_data %>% filter(technology == "lawnmower", year == 1985) %>%
                 select(value), tibble(value = 950))
  expect_equal(calc_data %>% filter(technology == "lawnmower", year == 2000) %>%
                 select(value), tibble(value = 1000 * 0.6 + (1000 - 1000 * 0.6) * (1.0 - 0.4) ^ (2000 - 1986)))
  expect_equal(calc_data %>% filter(technology == "riding mower", year == 1985) %>%
                 select(value), tibble(value = 1450))
  expect_equal(calc_data %>% filter(technology == "riding mower", year == 2000) %>%
                 select(value), tibble(value = (1000 * 0.6+(1000 - 1000 * 0.6)*(1.0-0.4) ^ (2000 - 1986)) +
                                         (500 * 0.9 + (500 - 500 * 0.9) * (1.0-0.5) ^ (2000 - 1986))))

  # test all three cases (no extrap, extrap no shadow, extrap shadow) together
  test_data %>%
    bind_rows(tibble(technology="goats", `1984` = 200, `1986` = 100)) ->
    test_data
  test_data %>% fill_exp_decay_extrapolate(test_years) ->
    calc_data
  expect_equal(calc_data %>% filter(technology == "lawnmower", year == 1985) %>%
                 select(value), tibble(value = 950))
  expect_equal(calc_data %>% filter(technology == "lawnmower", year == 2000) %>%
                 select(value), tibble(value = 1000 * 0.6+(1000-1000 * 0.6) * (1.0 - 0.4) ^ (2000 - 1986)))
  expect_equal(calc_data %>% filter(technology == "riding mower", year == 1985) %>%
                 select(value), tibble(value = 1450))
  expect_equal(calc_data %>% filter(technology == "riding mower", year == 2000) %>%
                 select(value), tibble(value = (1000 * 0.6 + (1000-1000 * 0.6) * (1.0-0.4) ^ (2000 - 1986)) +
                                         (500 * 0.9+(500-500 * 0.9)*(1.0-0.5) ^ (2000-1986))))
  expect_equal(calc_data %>% filter(technology == "goats", year == 1985) %>%
                 select(value), tibble(value = 150))
  expect_equal(calc_data %>% filter(technology == "goats", year == 2000) %>%
                 select(value), tibble(value = as.numeric(NA)))

  # ensure error with invalid shadow technology
  test_data[!is.na(test_data$improvement.shadow.technology), "improvement.shadow.technology"] <- "scythe"
  test_data %>%
    mutate(improvement.shadow.technology = if_else(!is.na(improvement.shadow.technology),
                                                   "scythe", as.character(NA))) ->
    test_data
  expect_error(test_data %>% fill_exp_decay_extrapolate(test_years))
})

test_that("downscale_FAO_country", {
  # catches bad input
  expect_error(downscale_FAO_country(1, "1", 1993))
  expect_error(downscale_FAO_country(tibble(), 1, 1993))
  expect_error(downscale_FAO_country(tibble(), "1", "1993"))
  expect_error(downscale_FAO_country(tibble(), "1", 1993, years = 2000:2002))

  d <- tibble(item = "item", element = "element", countries = c("x", "y", "z"),
              `1998` = c(0, 0, 3), `1999` = c(0, 0, 2), `2000` = c(1, 2, 0), `2001` = c(2, 1, 0))
  cn <- "z"
  dy <- 2000L
  yrs <- 1998:2001

  dnew <- gcamdata:::downscale_FAO_country(d, country_name = cn, dissolution_year = dy, years = yrs)
  expect_equal(ncol(d), ncol(dnew))
  expect_equal(nrow(d) - 1, nrow(dnew))  # post-dissolution country should be gone
  # ungrouped return
  expect_equal(dplyr::groups(dnew), list())

  # Pre-dissolution year should be calculated from ratio of dissolution year data
  expect_equal(dnew[as.character(yrs[yrs < dy])], tibble(`1998` = c(1, 2), `1999` = c(2/3, 4/3)))

  # Post-dissolution year should be untouched
  expect_equal(dnew[as.character(yrs[yrs >= dy])], d[1:2, as.character(yrs[yrs >= dy])])
})

test_that("write_to_all_states", {
  # catches bad input
  expect_error(write_to_all_states(1, "1"))
  expect_error(write_to_all_states(tibble(x = 1), 1))

  # check that write_to_all_states works as it should
  d <- tibble(region = 1, y = 2)
  dout <- write_to_all_states(d, names = c("region", "y"))
  expect_equal(dim(dout), c(length(gcamusa.STATES), ncol(d)))
  expect_identical(names(dout), names(d))
  # region column is now not required
  d <- tibble(y = 2)
  dout1 <- write_to_all_states(d, names = c("region", "y"))
  expect_identical(dout, dout1)

  # overwrites logit.year.fillout and price.exp.year.fillout with start-year
  d <- tibble(region = 1, logit.year.fillout = "logit.year.fillout",
              price.exp.year.fillout = "price.exp.year.fillout")
  dout <- write_to_all_states(d, names = c("region", "logit.year.fillout", "price.exp.year.fillout"))
  expect_equal(dim(dout), c(length(gcamusa.STATES), ncol(d)))
  expect_identical(dout$logit.year.fillout, rep(min(MODEL_BASE_YEARS), nrow(dout)))
  expect_identical(dout$price.exp.year.fillout, rep(min(MODEL_BASE_YEARS), nrow(dout)))

  # ungrouped return
  expect_equal(dplyr::groups(dout), list())
})

test_that("set_subsector_shrwt", {
  # catches bad input
  expect_error(set_subsector_shrwt(1))

  # needs to have region, supplysector, subsector, year
  expect_error(set_subsector_shrwt(tibble()))

  # needs to have a value column for aggregation (usually `calOutputValue`)
  d <- tibble(region = 1, supplysector = 1, subsector = 1, year = 1)
  expect_error(set_subsector_shrwt(d))

  # should produce subs.share.weight of 0 where calOutputValue = 0
  d <- tibble(region = 1, supplysector = 1, subsector = 1, year = 1, calOutputValue = 0)
  dout <- set_subsector_shrwt(d)
  expect_identical(names(dout), c(names(d), "subs.share.weight"))
  expect_equal(dout$subs.share.weight, 0)

  # should produce subs.share.weight of 1 where calOutputValue = 1
  d <- tibble(region = 1, supplysector = 1, subsector = 1, year = 1, calOutputValue = 1)
  dout <- set_subsector_shrwt(d)
  expect_equal(dout$subs.share.weight, 1)

  # ungrouped return
  expect_equal(dplyr::groups(dout), list())
})
