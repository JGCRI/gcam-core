## Script to run the coverage tests.  This is needed for running the tests on
## travis CI because the travis_wait command mangles the arguments you need to
## put the commands inline in the after_success section

library(covr)
x <- package_coverage()
cat("coverage = ", percent_coverage(x), " %\n")
codecov(coverage = x)
