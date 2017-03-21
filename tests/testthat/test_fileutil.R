# Test the file utilities

context("fileutil")


test_that("error with bad input", {
  expect_error(load_csv_files(TRUE))
  expect_error(load_csv_files(1))
  expect_error(find_csv_file(TRUE))
  expect_error(find_csv_file(1))
  expect_error(find_csv_file(c("h","i")))
  expect_error(save_chunkdata(1))
  expect_error(save_chunkdata(empty_data()))
  expect_error(save_chunkdata(write_inputs = 1))
})

test_that("handle empty input", {
  x <- load_csv_files(character(0))
  expect_is(x, "list")
  expect_length(x, 0)
  expect_error(file_csv_file(character(0)))
})

test_that("nonexistent file", {
  expect_error(load_csv_files("SDFKJFDJKSHGF", quiet = TRUE))
  expect_error(find_csv_file("SDFKJFDJKSHGF", quiet = TRUE))
})

test_that("loads test file", {
  fn <- "tests/cars.csv"
  fqfn <- system.file("extdata", fn, package = "gcamdata")
  f1 <- readr::read_csv(fqfn, col_types = "dd")
  expect_output(find_csv_file(fn, quiet = FALSE))
  expect_silent(find_csv_file(fn, quiet = TRUE))
  f2 <- load_csv_files(fn, quiet = TRUE)[[1]]
  expect_equal(f1, f2)
})

test_that("save_chunkdata saves", {
  df <- tibble::tibble(x = 1:3)
  all_data <- add_data(return_data(df), empty_data())
  td <- tempdir()
  save_chunkdata(all_data, outputs_dir = td)

  out <- file.path(td, "df.csv")
  expect_true(file.exists(out))
  df2 <- readr::read_csv(out)
  expect_equal(df, df2)
})

test_that("save_chunkdata does comments and flags", {
  cmnts <- c("this", "is", "a", "test")
  df <- add_comments(tibble::tibble(x = 1:3), cmnts)
  all_data <- add_data(return_data(df), empty_data())
  td <- tempdir()
  save_chunkdata(all_data, outputs_dir = td)

  out <- file.path(td, "df.csv")
  expect_true(file.exists(out))
  lines1 <- readLines(out)
  expect_equal(length(lines1), nrow(df) + length(cmnts) + 1)
  expect_equal(lines1[1:length(cmnts)], paste(COMMENT_CHAR, cmnts))

  flags <- c("FLAG1", "FLAG2")
  df <- add_flags(df, flags)
  all_data <- add_data(return_data(df), empty_data())
  save_chunkdata(all_data, outputs_dir = td)
  lines2 <- readLines(out)
  expect_equal(length(lines2), nrow(df) + length(cmnts) + 1 + 1)
  expect_equal(lines2[1], paste(flags, collapse = " "))
})
