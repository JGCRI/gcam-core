# Test the module helper utilities

context("module_helpers")

test_that("get_water_inputs_for_mapping", {

  # Bad inputs
  expect_error(get_water_inputs_for_mapping(1, tibble()))
  expect_error(get_water_inputs_for_mapping(tibble(), 2))

  not_irr <- "not_irrigation"
  water_mapping <- tibble(water.sector = c(IRRIGATION, not_irr),
                          supplysector = c("water_td_irr", "water_td_pri"))

  # Mapped GLU
  d <- tibble(water.sector = c(IRRIGATION, IRRIGATION, not_irr, not_irr),
              water_type = c(MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2],
                             MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2]),
              GLU = c("GLU01", "GLU02", "GLU03", "GLU04"))
  res <- expect_is(get_water_inputs_for_mapping(d, water_mapping), "character")
  expect_equal(length(res), nrow(d))

  # No GLU, but no mapped water types in irrigation
  d <- tibble(water.sector = c(IRRIGATION, IRRIGATION, not_irr, not_irr),
              water_type = c("x", "y", MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2]))
  res <- expect_is(get_water_inputs_for_mapping(d, water_mapping), "character")
  expect_equal(length(res), nrow(d))
  expect_equal(d$water_type[1:2], res[1:2])   # should have original names

  # No GLU - error
  d <- tibble(water.sector = c(IRRIGATION, IRRIGATION, not_irr, not_irr),
              water_type = c(MAPPED_WATER_TYPES[1], "y",
                             MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2]))
  expect_error(get_water_inputs_for_mapping(d, water_mapping))
})
