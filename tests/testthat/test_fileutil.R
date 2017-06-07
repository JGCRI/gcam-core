# Test the file utilities

context("fileutil")


test_that("error with bad input", {
  expect_error(load_csv_files(TRUE))
  expect_error(load_csv_files(1))
  expect_error(find_csv_file(TRUE))
  expect_error(load_csv_files("hi", 1))
  expect_error(load_csv_files("hi", c(TRUE, FALSE)))
  expect_error(find_csv_file(1))
  expect_error(find_csv_file(c("h", "i")))
  expect_error(find_csv_file("hi"))
  expect_error(save_chunkdata(1))
  expect_error(save_chunkdata(empty_data()))
  expect_error(save_chunkdata(write_inputs = 1))
})

test_that("handle empty input", {
  x <- load_csv_files(character(0), optionals = logical(0))
  expect_is(x, "list")
  expect_length(x, 0)
  expect_error(find_csv_file(character(0), optionals = FALSE))
})

test_that("nonexistent file", {
  expect_error(load_csv_files("SDFKJFDJKSHGF", optionals = FALSE, quiet = TRUE))
  expect_silent(x <- load_csv_files("SDFKJFDJKSHGF", optionals = TRUE, quiet = TRUE))
  expect_true(is.na(x))
  expect_error(find_csv_file("SDFKJFDJKSHGF", optional = FALSE, quiet = TRUE))
  expect_silent(x <- find_csv_file("SDFKJFDJKSHGF", optional = TRUE, quiet = TRUE))
  expect_null(x)
})

test_that("loads test file", {
  fn <- "tests/cars.csv"
  fqfn <- system.file("extdata", fn, package = "gcamdata")
  f1 <- readr::read_csv(fqfn, col_types = "dd", comment = COMMENT_CHAR)
  expect_output(find_csv_file(fn, optional = FALSE, quiet = FALSE))
  expect_silent(find_csv_file(fn, optional = FALSE, quiet = TRUE))
  f2 <- load_csv_files(fn, optionals = FALSE, quiet = TRUE)[[1]]
  expect_equal(f1, f2)

  expect_output(load_csv_files(fn, optionals = FALSE, quiet = FALSE))

  # Did header metadata get parsed?
  expect_is(get_title(f2), "character")
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

  # Handles NAs in data list?
  NAdf <- NA
  all_data <- add_data(return_data(NAdf), all_data)
  expect_silent(save_chunkdata(all_data, outputs_dir = td))
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
  expect_equal(lines2[1], paste(COMMENT_CHAR, paste(flags, collapse = " ")))
})


test_that("extract_header_info works", {

  # Test data
  x <- c("# File: file",
         "# Title: title",
         "#Units: units",
         "# Dupe: dupe1",
         "# Dupe: dupe2",
         "# Description: desc1",
         "# desc2",
         "# Source: source1",
         "# source2",
         "data,start",
         "1,2")
  # Extract label that's there
  expect_equal(extract_header_info(x, "File:", "filename"), "file")
  # Label no space
  expect_equal(extract_header_info(x, "Units:", "filename"), "units")
  # Label not present, not required
  expect_null(extract_header_info(x, "XXXXX:", "filename", required = FALSE))
  # Label not present, is required
  expect_error(extract_header_info(x, "XXXXX:", "filename", required = TRUE))
  # Duplicated label
  expect_error(extract_header_info(x, "Dupe:", "filename"))
  # Multiline label terminated by another label
  expect_equal(extract_header_info(x, "Description:", "filename", multiline = TRUE), c("desc1", "desc2"))
  # Multiline label terminated by data
  expect_equal(extract_header_info(x, "Source:", "filename", multiline = TRUE), c("source1", "source2"))
})


test_that("parse_csv_header works", {

  # Test data; save to tempfile
  x <- c("# File: file",
         "# Title: title",
         "#Units: units",
         "# Description: desc1",
         "# desc2",
         "# Source: source",
         "data,start",
         "1,2")
  tf <- tempfile()
  x[1] <- paste("# File:", basename(tf))
  obj_original <- tibble()
  expect_error(parse_csv_header(obj_original, tf))  # file doesn't exist yet
  writeLines(x, tf)
  expect_true(file.exists(tf))

  obj <- parse_csv_header(obj_original, tf)
  expect_equivalent(obj, obj_original)
  expect_equal(get_title(obj), "title")
  expect_equal(get_units(obj), "units")
  expect_equal(get_comments(obj), c("desc1", "desc2"))
  expect_equal(get_reference(obj), "source")

  # GZ'd file
  if(require(R.utils)) {
    gztf <- R.utils::gzip(tf, remove = FALSE)
    if(file.exists(gztf)) {
      obj <- parse_csv_header(obj_original, gztf)
      expect_equal(get_title(obj), "title")
      file.remove(gztf)
    }
  }

  # Zipped file
  ztf <- paste0(tf, ".zip")
  utils::zip(ztf, tf, extras = "-jq")   # junk paths, quiet
  if(file.exists(ztf)) {
    obj <- parse_csv_header(obj_original, ztf)
    expect_equal(get_title(obj), "title")
    file.remove(ztf)
  }

  # File without required data
  writeLines(x[1], tf)
  expect_error(parse_csv_header(obj_original, tf, enforce_requirements = TRUE))

  # File with wrong filename
  correctx1 <- x[1]
  x[1] <- "# File: xxxx"
  writeLines(x, tf)
  expect_error(parse_csv_header(obj_original, tf, enforce_requirements = TRUE))

  # File with Excel-quote error
  x[1] <- correctx1
  x[3] <- '"# Excel,is,stupid"'
  writeLines(x, tf)
  expect_error(parse_csv_header(obj_original, tf))

  file.remove(tf)
})
