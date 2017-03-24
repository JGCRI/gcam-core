# Test the CSV to XML conversion utilities

context("xml")

test_that("default MI header exists in the package", {
  conv_test <- create_xml("test.xml")

  expect_true(file.exists(conv_test$mi_header))
})

test_that("bogus MI header causes error", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  # TODO: need to find a way to get messages from Java (issue )
  # conv_test <- create_xml("test.xml", "bogus.txt")
  #
  # expect_message(run_xml_conversion(conv_test), regex="java.io.FileNotFoundException: bogus.txt", all=F)
})

test_that("converting without adding data causes error", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  # TODO: need to find a way to get messages from Java (See issue #102)
  # conv_test <- create_xml("test.xml")
  #
  # expect_message(run_xml_conversion(conv_test), regex="java.lang.NullPointerException", all=F)
})

test_that("converting bogus data causes error", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  # TODO: need to find a way to get messages from Java (See issue #102)
  # create_xml("test.xml") %>%
  #   add_xml_data("bogus", "InterestRate") -> conv_test
  #
  # expect_error(run_xml_conversion(conv_test))
})

test_that("can convert single table", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- data.frame(region = "USA", interest.rate = "1.0")
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") %>%
    run_xml_conversion()

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><interest-rate>1.0</interest-rate></region></world></scenario>')
})

test_that("can convert multiple table", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- data.frame(region = "USA", interest.rate = "1.0")
  data2 <- data.frame(region = "USA", PrimaryFuel = "shoes", PrimaryFuelCO2Coef = 0.007653)
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") %>%
    add_xml_data(data2, "CarbonCoef") %>%
    run_xml_conversion()

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse="") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><interest-rate>1.0</interest-rate><PrimaryFuelCO2Coef name="shoes">0.007653</PrimaryFuelCO2Coef></region></world></scenario>')
})

test_that("get warning for missing header", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- data.frame(region = "USA", interest.rate = "1.0")
  data2 <- data.frame(region = "USA", PrimaryFuel = "shoes", PrimaryFuelCO2Coef = 0.007653)
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") %>%
    add_xml_data(data2, "Will_Not_Find") ->
    conv_test
  # TODO: need to find a way to get messages from Java (See issue #102)
  #expect_message(run_xml_conversion(conv_test), regex = "Warning: skipping table: Will_Not_Find!", all = FALSE)
  run_xml_conversion(conv_test)

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><interest-rate>1.0</interest-rate></region></world></scenario>')
})

