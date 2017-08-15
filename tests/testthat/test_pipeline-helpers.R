# Test global constants for proper capitalization convention
# E.g. "XXX" and "driver.XXX" are OK, but "xxx" is not

context("pipeline-helpers")


test_that("approx_fun works", {
  expect_error(approx_fun("a", 1))
  expect_error(approx_fun(1, "a"))

  year <- 1:5
  value <- c(2, 4, NA, 8, 10)
  z <- approx_fun(year, value)
  expect_true(all(is.numeric(z)))
  expect_equal(z[3], 6)

  expect_error(approx_fun(year, value, rule = 3))
})

test_that("PH_year_value_historical works", {
  tibble(year = 1900:2100, value = "1") %>%
    PH_year_value_historical ->
    x

  expect_true(all(x$year %in% HISTORICAL_YEARS))
  expect_is(x$value, "numeric")
})

test_that("change_iso_code works", {
  ## test with default column name
  d <- tibble(iso = c("dum", "bad", "bad", "bad", "dum"),
              `2005` = rep(23, 5))
  d %>% change_iso_code("bad", "gud") -> d2
  expect_equal(d2$iso, c("dum","gud","gud","gud","dum"))
  expect_equal(d2[["2005"]], d[["2005"]])

  ## test with nonstandard column name
  names(d) <- c("COUNTRY","2005")
  d %>% change_iso_code("bad", "gud", col = "COUNTRY") -> d3
  expect_equal(d3$COUNTRY, d2$iso)
  expect_equal(d3[["2005"]], d2[["2005"]])
})

test_that("standardize_iso works", {
  ## test default column name
  d <- tibble(iso = c("BAD","GUD","DUM"), `2005` = c(1, 2, 3))
  d %>% standardize_iso() -> d2
  expect_equal(d2$iso, c("bad", "gud", "dum"))

  ## test nonstandard column name
  names(d) <- c("COUNTRY", "2005")
  d %>% standardize_iso(col = "COUNTRY") -> d3
  expect_true("iso" %in% names(d3))
  expect_true("2005" %in% names(d3))
  expect_equal(d3$iso, d2$iso)
  expect_equal(d3[["2005"]], d2[["2005"]])
})

test_that("protect and unprotect integer cols work", {
  d <- tibble(iso = c("bad", "dum"),
              `2005` = c(123.45, NA),
              `2050` = c(867, 5309))
  d2 <-protect_integer_cols(d)
  expect_equal(names(d2), c("iso", "X2005", "X2050"))
  d2 %>% dplyr::select_if(function(col) { !any(is.na(col)) }) %>%
    unprotect_integer_cols -> d3
  expect_equal(names(d3), c("iso", "2050"))
})

test_that("left_join_error_no_match works", {

  # Basic join works
  x <- tibble(year = 2000:2002)
  y <- tibble(year = 2000:2002, value = 1:3)
  expect_silent(left_join_error_no_match(x, y, by = "year"))
  expect_identical(left_join_error_no_match(x, y, by = "year"),
                   left_join(x, y, by = "year"))

  # Catches duplicate matches
  y <- tibble(year = c(2000, 2001, 2001, 2002), value = 1:4)
  expect_error(left_join_error_no_match(x, y, by = "year"))

  # Catch no match (NAs)
  y <- tibble(year = c(2000, 2002), value = 1:2)
  expect_error(left_join_error_no_match(x, y, by = "year"))

  # ignore_columns works
  y <- tibble(year = 2000:2002, value = 1:3, logit.type = NA)
  expect_error(left_join_error_no_match(x, y, by = "year"))
  expect_silent(left_join_error_no_match(x, y, by = "year", ignore_columns = "logit.type"))
})

test_that("left_join_keep_first_only works", {
  x <- tibble(iso = c('gud', 'bad', 'ugy'),
              year = c(2000, 2000, 2001),
              value = c(1.1, 2.2, 3.3))
  y <- tibble(iso = c('gud', 'bad', 'ugy', 'ugy'),
              year = c(2000, 2000, 2000, 2001),
              coef = c(1, 2, 3, 4))

  ## test explicit by arg is required
  expect_error(left_join_keep_first_only(x,y))

  ## test matching on one column
  r1 <- left_join_keep_first_only(x, y, by = 'iso')
  expect_equal(r1,
               tibble(iso = x$iso,
                      year.x = x$year,
                      value = x$value,
                      year.y = c(2000, 2000, 2000),
                      coef = c(1,2,3)
               ))

  ## test matching on two columns with only one match
  r2 <- left_join_keep_first_only(x, y, by = c('iso', 'year'))
  expect_equal(r2, mutate(x, coef = c(1, 2, 4)))

  ## test matching on two columns with two matches
  x2 <- bind_rows(x,
                  tibble(iso = 'ugy',
                         year = 2000,
                         value = 4.4))
  ## This case is weird.  Using it in practice is probably an error, but
  ## nevertheless it works as advertised
  r3 <- left_join_keep_first_only(x2, y, by = 'iso')
  expect_equal(r3,
               mutate(x2, year.x = year, year.y = rep(2000, 4), coef = c(1, 2, 3, 3)) %>%
                 select(-year))
  ## more sensible usage
  r4 <- left_join_keep_first_only(x2, y, by = c('iso','year'))
  expect_equal(r4,
               mutate(x2, coef = c(1, 2, 4, 3)))

  ## test named vector to rename columns
  y2 <- rename(y, country = iso)
  expect_silent(r5 <- left_join_keep_first_only(x2, y2, by = c(iso = 'country', 'year')))
  expect_equal(r4, r5)

})

test_that("fast_left_join produces results equivalent to left_join", {
  x <- as.integer(100*rnorm(100))
  y <- rnorm(100)
  z1 <- rnorm(100)
  z2 <- rnorm(100)

  A <- tibble(x=x, y=y, z1=z1)
  B <- tibble(x=x, y=y, z2=z2)

  ## Two join columns, no duplicate unjoined columns
  ABdp <- dplyr::left_join(A, B, by=c('x','y')) %>% arrange(x,y)
  ABdt <- fast_left_join(A, B, by=c('x','y')) %>% arrange(x,y)

  expect_equal(ABdp, ABdt)

  ## One join column, y is a duplicate
  ABdp <- dplyr::left_join(A, B, by='x') %>% arrange(x, y.x, y.y)
  ABdt <- fast_left_join(A, B, by='x') %>% rename(y.x = i.y, y.y = y) %>%
    arrange(x, y.x, y.y)

  expect_equal(ABdp, ABdt)
})



