# Test the module helper utilities

context("module_helpers")

test_that("get_water_inputs_for_mapping", {

  # Bad inputs
  expect_error(get_water_inputs_for_mapping(1, character(), tibble()))
  expect_error(get_water_inputs_for_mapping(character(), 1, tibble()))
  expect_error(get_water_inputs_for_mapping(character(), character(), 1))
  expect_error(get_water_inputs_for_mapping(character(), character(), tibble(), 1))

  not_irr <- "not_irrigation"
  water_mapping <- tibble(water.sector = c(IRRIGATION, not_irr),
                          supplysector = c("water_td_irr", "water_td_pri"))

  # Mapped GLU
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type <- c(MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2],
                  MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  GLU <- c("GLU01", "GLU02", "GLU03", "GLU04")
  res <- expect_is(get_water_inputs_for_mapping(water_sector, water_type, water_mapping, GLU), "character")
  expect_equal(length(res), length(water_sector))

  # No GLU, but no mapped water types in irrigation
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type <- c("x", "y", MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  res <- expect_is(get_water_inputs_for_mapping(water_sector, water_type, water_mapping), "character")
  expect_equal(length(res), length(water_sector))
  expect_equal(water_type[1:2], res[1:2])   # should have original names

  # No GLU - error
  water_sector <- c(IRRIGATION, IRRIGATION, not_irr, not_irr)
  water_type = c(MAPPED_WATER_TYPES[1], "y", MAPPED_WATER_TYPES[1], MAPPED_WATER_TYPES[2])
  expect_error(get_water_inputs_for_mapping(water_sector, water_type, water_mapping))
})
