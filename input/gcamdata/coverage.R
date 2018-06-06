## Script to run the coverage tests.  This is needed for running the tests on
## travis CI because the travis_wait command mangles the arguments you need to
## put the commands inline in the after_success section

library(covr)
Sys.setenv("gcamdata.is_coverage_test" = "TRUE")
x <- package_coverage(function_exclusions = "^module_")
cat("coverage = ", percent_coverage(x), " %\n")
codecov(coverage = x)
