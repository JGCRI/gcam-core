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
  # expect_message(run_xml_conversion(conv_test), regex = "java.io.FileNotFoundException: bogus.txt", all = FALSE)
})

test_that("converting without adding data causes error", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  # TODO: need to find a way to get messages from Java (See issue #102)
  # conv_test <- create_xml("test.xml")
  #
  # expect_message(run_xml_conversion(conv_test), regex = "java.lang.NullPointerException", all = FALSE)
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
  data1 <- tibble(region = "USA", interest.rate = "1.0")
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
  data1 <- tibble(region = "USA", interest.rate = "1.0")
  data2 <- tibble(region = "USA", PrimaryFuelCO2Coef.name = "shoes", PrimaryFuelCO2Coef = 0.007653)
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") %>%
    add_xml_data(data2, "CarbonCoef") %>%
    run_xml_conversion()

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><interest-rate>1.0</interest-rate><PrimaryFuelCO2Coef name="shoes">0.007653</PrimaryFuelCO2Coef></region></world></scenario>')
})

test_that("get warning for missing header", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", interest.rate = "1.0")
  data2 <- tibble(region = "USA", PrimaryFuelCO2Coef.name = "shoes", PrimaryFuelCO2Coef = 0.007653)
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") %>%
    # Be sure to set column_order_lookup to NULL since automatic
    # column reordering won't work with unknown headers
    add_xml_data(data2, "Will_Not_Find", column_order_lookup = NULL) ->
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

test_that("automatic column re-ordering works after add", {
  test_fn <- "test.xml"
  data1 <- tibble(interest.rate = "1.0", region = "USA")
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate") ->
    conv_test

  expect_identical(names(conv_test$data_tables[[1]]$data), c("region", "interest.rate"))
})

test_that("column_order_lookup=NULL skips column reordering", {
  test_fn <- "test.xml"
  data1 <- tibble(interest.rate = "1.0", region = "USA")
  create_xml(test_fn) %>%
    add_xml_data(data1, "InterestRate", NULL) ->
    conv_test

  expect_identical(names(conv_test$data_tables[[1]]$data), c("interest.rate", "region"))
})

test_that("automatic column re-ordering fails for unknown header", {
  test_fn <- "test.xml"
  data1 <- tibble(interest.rate = "1.0", region = "USA")
  conv_test <- create_xml(test_fn)
  expect_error(add_xml_data(data1, "InterestRate", "Will_Not_Find"))
})

test_that("LandNode rename works", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", LandAllocatorRoot = "root", LandNode1 = "node1",
                  LandNode2 = "node2", LandLeaf = "leaf", year = 2017, allocation = 875.34)
  create_xml(test_fn) %>%
    add_xml_data(data1, "LN2_MgdAllocation") %>%
    add_rename_landnode_xml() %>%
    run_xml_conversion()

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><LandAllocatorRoot name="root"><LandNode name="node1"><LandNode name="node2"><LandLeaf name="leaf"><landAllocation year="2017">875.34</landAllocation></LandLeaf></LandNode></LandNode></LandAllocatorRoot></region></world></scenario>')
})

test_that("add_node_equiv_xml works", {
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", supplysector = "sector", subsector = "sub",
                  pass.through.technology = "ptech")
  data2 <- tibble(region = "USA", supplysector = "sector", subsector = "sub",
                  technology = "ptech", year = 2017, share.weight = 0.56)
  create_xml(test_fn) %>%
    add_node_equiv_xml("technology") %>%
    add_xml_data(data1, "PassThroughTech") %>%
    add_xml_data(data2, "TechShrwt") %>%
    run_xml_conversion()

  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><supplysector name="sector"><subsector name="sub"><pass-through-technology name="ptech"><period year="2017"><share-weight>0.56</share-weight><CO2 name="CO2"/></period></pass-through-technology></subsector></supplysector></region></world></scenario>')
})

test_that("add_logit_tables_xml works", {
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", supplysector = "sector", subsector = "sub",
                  logit.year.fillout = 1900, logit.exponent = -3,
                  logit.type=gcam.LOGIT_TYPES[2])
  create_xml(test_fn) %>%
    add_logit_tables_xml(data1, "SubsectorLogit") ->
    logit.xml

  # We should get back length(gcam.LOGIT_TYPES)+2 tables:
  # 1) XML_NODE_EQUIV
  # 2) A table for each gcam.LOGIT_TYPES
  # 3) The table to read in the logit.exponent from data1
  expect_equal(length(logit.xml$data_tables), length(gcam.LOGIT_TYPES)+2)
  create_xml(test_fn) %>%
    add_node_equiv_xml("discrete-choice") ->
    node_equiv.xml
  expect_identical(logit.xml$data_tables[[1]], node_equiv.xml$data_tables[[1]])
  expect_equal(nrow(logit.xml$data_tables[[2]]$data), 0)
  expect_equal(nrow(logit.xml$data_tables[[3]]$data), 1)
  expect_identical(names(logit.xml$data_tables[[2]]$data),
                   LEVEL2_DATA_NAMES[[paste("SubsectorLogit", gcam.LOGIT_TYPES[1], sep = "_")]])
  expect_identical(names(logit.xml$data_tables[[length(gcam.LOGIT_TYPES)+2]]$data),
                   LEVEL2_DATA_NAMES[["SubsectorLogit"]])

  # Do not perform the rest of the checks if java is not enabled
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  run_xml_conversion(logit.xml)
  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><supplysector name="sector"><subsector name="sub"><absolute-cost-logit><logit-exponent fillout="1" year="1900">-3</logit-exponent></absolute-cost-logit></subsector></supplysector></region></world></scenario>')
})

test_that("add_logit_tables_xml sets default logit", {
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", supplysector = "sector", subsector = "sub",
                  logit.year.fillout = 1900, logit.exponent = -3,
                  logit.type=NA)
  create_xml(test_fn) %>%
    add_logit_tables_xml(data1, "SubsectorLogit") ->
    logit.xml

  # We should get back length(gcam.LOGIT_TYPES)+2 tables:
  # 1) XML_NODE_EQUIV
  # 2) A table for each gcam.LOGIT_TYPES
  # 3) The table to read in the logit.exponent from data1
  expect_equal(length(logit.xml$data_tables), length(gcam.LOGIT_TYPES)+2)
  create_xml(test_fn) %>%
    add_node_equiv_xml("discrete-choice") ->
    node_equiv.xml
  expect_identical(logit.xml$data_tables[[1]], node_equiv.xml$data_tables[[1]])
  expect_equal(nrow(logit.xml$data_tables[[2]]$data), 1)
  expect_equal(nrow(logit.xml$data_tables[[3]]$data), 0)
  expect_identical(names(logit.xml$data_tables[[2]]$data),
                   LEVEL2_DATA_NAMES[[paste("SubsectorLogit", gcam.LOGIT_TYPES[1], sep = "_")]])
  expect_identical(names(logit.xml$data_tables[[length(gcam.LOGIT_TYPES)+2]]$data),
                   LEVEL2_DATA_NAMES[["SubsectorLogit"]])

  # Do not perform the rest of the checks if java is not enabled
  if(!isTRUE(getOption("gcamdata.use_java"))) {
    skip("Skipping test as global option gcamdata.use_java is not TRUE")
  }
  run_xml_conversion(logit.xml)
  expect_true(file.exists(test_fn))
  test_xml <- readLines(test_fn)
  unlink(test_fn)
  test_xml %>%
    gsub("^\\s+|\\s+$", "", .) %>%
    paste(., collapse = "") ->
    test_xml
  expect_identical(test_xml, '<?xml version="1.0" encoding="UTF-8"?><scenario><world><region name="USA"><supplysector name="sector"><subsector name="sub"><relative-cost-logit><logit-exponent fillout="1" year="1900">-3</logit-exponent></relative-cost-logit></subsector></supplysector></region></world></scenario>')
})

test_that("add_logit_tables_xml fails when not given a logit.type column", {
  test_fn <- "test.xml"
  data1 <- tibble(region = "USA", supplysector = "sector", subsector = "sub",
                  logit.year.fillout = 1900, logit.exponent = -3)
  expect_error(create_xml(test_fn) %>%
    add_logit_tables_xml(data1, "SubsectorLogit") ->
    logit.xml)
})
