library(sars2app)
library(testthat)
library(lubridate)

Arima_state_result_names = c("fit", "pred", "tsfull", "dates29", "time.from.origin", "call", 
"state", "origin", "MAorder", "Difforder", "ARorder", "max_date")

context("basic Arima runs")

nyd = nytimes_state_data()

n1 = Arima_by_state(nyd)

test_that("returned object has expected fields", {
 expect_true(all(Arima_state_result_names %in% names(n1)))
})

test_that("latest default date close to current date", {
 expect_true((as_date(Sys.Date()) - max(n1$dates29)) < 3)
})

context("date control for lookback works")

n2 = Arima_by_state(nyd, max_date="2020-05-15")

test_that("max_date setting works", {
 expect_true((as_date("2020-05-15") == as_date(max(n2$dates29))))
})
