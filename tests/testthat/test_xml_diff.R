# test xml validation functions

context("XML diff")

basefile <- 'test-data/modeltime.xml'

test_that('Identical XML files are equivalent', {
  expect_silent(cmp_xml_files(basefile, basefile, raw=TRUE))
})

test_that('Files differing only in whitespace are equivalent',{
  expect_silent(cmp_xml_files(basefile, 'test-data/modeltime-nows.xml', raw=TRUE))
})

test_that("Ordering of child nodes doesn't matter", {
  expect_silent(cmp_xml_files(basefile, 'test-data/modeltime-rearrange.xml', raw=TRUE))
})

test_that("Rounding errors don't matter", {
  expect_silent(cmp_xml_files('test-data/rounding-test1.xml',
                              'test-data/rounding-test2.xml', raw=TRUE))
})

test_that('Changed tag is detected', {
  expect_warning({rslt <-
                  cmp_xml_files(basefile, 'test-data/modeltime-chtag.xml', raw=TRUE)},
                 'had status 3')
  expect_equal(attr(rslt, 'status'), 3)
  expect_equal(length(rslt), 48)
  expect_equal(rslt[1], "At: [('scenario', ''), ('modeltime', '')]")
  expect_equal(rslt[3], "\t\tfinal-calibration-year")
  expect_equal(rslt[10], "\t\tend-year")
})

test_that('Changed node value and attribute value are detected', {
  expect_warning({rslt <-
                  cmp_xml_files(basefile, 'test-data/modeltime-chval.xml', raw=TRUE)},
                 'had status 3')
  expect_equal(attr(rslt, 'status'), 3)
  expect_equal(length(rslt), 32)
  expect_equal(rslt[1], "At: [('scenario', ''), ('modeltime', '')]")
  expect_equal(rslt[7], "\t\t2010")
  expect_equal(rslt[14], "\t\t2015")
  expect_equal(rslt[22], "\t\t['5']" )
  expect_equal(rslt[29], "\t\t['1']")
})

test_that('Dropped child node is detected',{
  expect_warning({rslt <-
                  cmp_xml_files(basefile, 'test-data/modeltime-dropval.xml', raw=TRUE)},
                 'had status 3')
  expect_equal(attr(rslt, 'status'), 3)
  expect_equal(length(rslt), 5)
  expect_equal(rslt[1], "At: [('scenario', '')]")
  expect_equal(rslt[2], "\tnode: modeltime  name= ")
  expect_equal(rslt[3], "\tLeft:  4 child nodes")
  expect_equal(rslt[4], "\tRight: 3 child nodes")
  expect_equal(rslt[5], "")
})
