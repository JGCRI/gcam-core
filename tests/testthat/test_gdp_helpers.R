## Test the gdp join function

context('gdp-join')

## set up test data
past <- tibble(year=c(2000,2005,2010)) %>%
  tidyr::crossing(iso=c('xxl', 'xxp'))  %>%
  mutate(gdp=if_else(iso=='xxl', 10*year, 5*year))

future <- tibble(year=rep(c(2005, 2010, 2015), 2), iso=c(rep('xxl',3), rep('xxp',3)),
                 gdp=c(cumprod(rep(1.05,3)), cumprod(rep(1.02,3)) ) ) %>%
  tidyr::crossing(scenario=c('gud','bad')) %>%
  mutate(gdp=if_else(scenario == 'gud', gdp, gdp*0.9^((year-2005)/5)))

test_that('Joining time series with future scenarios works', {
  rslt <- join.gdp.ts(past, future, 'iso')
  expect_equal(sort(names(rslt)), c('gdp','iso','scenario','year'))
  expect_equal(nrow(rslt), 16)
  expect_equal(unique(rslt$year), c(2000, 2005, 2010, 2015))
  expect_equal( (filter(rslt, year==2000) %>% arrange(scenario, iso))[['gdp']],
                c(20000, 10000, 20000, 10000) )
  gdp2010 <- (filter(rslt, year==2010) %>% arrange(scenario, iso))[['gdp']]
  expect_equal( gdp2010, c(20100, 10050, 20100, 10050) )
  expect_equal( (filter(rslt, year==2015) %>% arrange(scenario, iso))[['gdp']],
                c(0.9*1.05, 0.9*1.02, 1.05, 1.02) * gdp2010)
})

test_that('Joining time series without future scenarios works', {
  fut <- filter(future, scenario=='gud') %>% select(-scenario)
  rslt <- join.gdp.ts(past, fut, 'iso')
  expect_equal(sort(names(rslt)), c('gdp', 'iso', 'year'))
  expect_equal(nrow(rslt), 8)
  gdp2010 <- (filter(rslt, year==2010) %>% arrange(iso))[['gdp']]
  expect_equal( gdp2010, c(20100, 10050) )
  expect_equal( (filter(rslt, year==2015) %>% arrange(iso))[['gdp']],
                c(1.05, 1.02) * gdp2010)
})
