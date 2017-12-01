# test xml validation functions

context("XML diff")

basefile <- 'test-data/modeltime.xml'

test_that('Python is available', {
  skip_on_os('windows')
  cmd <- system2('which', 'python', stdout=TRUE)
  expect_true(file.exists(cmd), "Can't find python runtime for XML diff.")
})

test_that('Identical XML files are equivalent', {
  skip_on_os('windows')
  expect_silent({rslt <- cmp_xml_files(basefile, basefile, raw=TRUE)})
  expect_equal(attr(rslt, 'status'), 0)
})

test_that('Files differing only in whitespace are equivalent',{
  skip_on_os('windows')
  expect_silent({rslt <- cmp_xml_files(basefile, 'test-data/modeltime-nows.xml', raw=TRUE)})
  expect_equal(attr(rslt, 'status'), 0)
})

test_that("Ordering of child nodes doesn't matter", {
  skip_on_os('windows')
  expect_silent({rslt <- cmp_xml_files(basefile, 'test-data/modeltime-rearrange.xml', raw=TRUE)})
  expect_equal(attr(rslt, 'status'), 0)
})

test_that("Rounding errors don't matter", {
  skip_on_os('windows')
  expect_silent({rslt <- cmp_xml_files('test-data/rounding-test1.xml',
                                       'test-data/rounding-test2.xml', raw=TRUE)})
  expect_equal(attr(rslt, 'status'), 0)
})

test_that('Changed tag is detected', {
  skip_on_os('windows')
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
  skip_on_os('windows')
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
  skip_on_os('windows')
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

test_that('standard output mode is equivalent to raw mode', {
  skip_on_os('windows')
  expect_true(cmp_xml_files(basefile, basefile))
  expect_false(cmp_xml_files(basefile, 'test-data/modeltime-chval.xml'))
  expect_error(cmp_xml_files(basefile, 'no-such-file.xml'), "Can't find file")
})

test_that('bulk testing works', {
    skip_on_os('windows')
    olddir <- tempfile()
    newdir <- tempfile()
    dir.create(olddir)
    dir.create(newdir)

    testfiles <- list.files('test-data','\\.xml$', full.names=TRUE)
    expect_gte(length(testfiles), 1)

    for (file in testfiles) {
        fn <- basename(file)
        file.copy(file, file.path(olddir, fn))
        file.copy(file, file.path(newdir, fn))
    }

    expect_equal(run_xml_tests(olddir, newdir), 0)

    ## make one of the files disagree
    file.copy(file.path(newdir, 'modeltime-chval.xml'),
              file.path(newdir, 'modeltime.xml'), overwrite=TRUE)

    expect_warning({nd <- run_xml_tests(olddir, newdir)},
                   'files had discrepancies')
    expect_equal(nd, 1)

    ## no old files to test is a warning
    emptydir <- tempfile()
    dir.create(emptydir)
    expect_warning(run_xml_tests(emptydir, newdir),
                   'No XML files')

    unlink(olddir, recursive=TRUE)
    unlink(newdir, recursive=TRUE)
    unlink(emptydir, recursive=TRUE)
})


