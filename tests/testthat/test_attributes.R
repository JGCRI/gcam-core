# Test the attribute utilities

context("attributes")

test_that("commenting system works", {
  cmnt <- "test"
  d <- tibble::tibble()
  expect_true(is.null(get_comments(d)))
  expect_error(add_comments(d, 1))

  c1 <- get_comments(d)
  d <- add_comments(d, cmnt)
  expect_gt(length(get_comments(d)), length(c1))
  expect_equal(get_comments(d), cmnt)
})

test_that("units system works", {
  unit <- "test"
  d <- tibble::tibble()
  expect_true(is.null(get_units(d)))
  expect_error(add_units(d, 1))

  u1 <- get_units(d)
  d <- add_units(d, unit)
  expect_gt(length(get_units(d)), length(u1))
  expect_equal(get_units(d), unit)
})

test_that("flag system works", {
  flag <- "test"
  d <- tibble::tibble()
  expect_true(is.null(get_flags(d)))

  c1 <- get_flags(d)
  d <- add_flags(d, flag)
  expect_gt(length(get_flags(d)), length(c1))
  expect_equal(get_flags(d), flag)
})

test_that("title system works", {
  title <- "test"
  d <- tibble::tibble()
  expect_true(is.null(get_title(d)))
  expect_error(add_title(d, 1))

  d <- add_title(d, title)
  expect_equal(get_title(d), title)
  expect_error(add_title(d, "test"))
})

test_that("legacy_name system works", {
  ln <- "test"
  d <- tibble::tibble()
  expect_true(is.null(get_legacy_name(d)))
  expect_error(add_legacy_name(d, 1))

  d <- add_legacy_name(d, ln)
  expect_equal(get_legacy_name(d), ln)
  expect_error(add_legacy_name(d, "test"))
})

test_that("add_precursors works", {
  p1 <- "p1"
  p2 <- "p2"

  x <- tibble()
  expect_true(is.null(get_precursors(x)))

  x <- add_precursors(x, p1)
  expect_equal(p1, get_precursors(x))

  x <- add_precursors(x, p2)
  expect_equal(c(p1, p2), get_precursors(x))
})

test_that("same_precursors_as works", {
  x <- add_precursors(tibble(), "p1")
  y <- same_precursors_as(tibble(), x)
  expect_equal(get_precursors(x), get_precursors(y))
})
