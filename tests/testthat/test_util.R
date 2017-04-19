context('util')

test_that('screening for use of forbidden functions works', {
  testgood <- function(d1, d2) {
    # This function should pass because we only mention
    # match and ifelse in comments and strings
    foo <- gcamdata::left_join_error_no_match(d1, d2)
    message('The "match" above passes because it is part of left_join_error_no_match')
  }

  testbad <- function(x, d) {
    # This function makes no sense, but it does use all of the currently forbidden functions,
    # including match
    y <- ifelse(x<0,0, x)
    z <- ifelse(x>10, 10, x)
    dd <- melt(d)
    df <- cast(dd)
    dfm <- merge(d,dd)
    dx <- cbind(d,x)
    wtf <- rbind(d,df)
    gtfo <- match(x,y)
  }

  expect_equal(gcamdata:::screen_forbidden(testgood), character())
  tb <- gcamdata:::screen_forbidden(testbad)
  expect_equal(tb[,1], c('(?<!error_no_)match', 'ifelse', 'ifelse', 'melt', 'cast', 'rbind',
                          'cbind', 'merge'))
})
