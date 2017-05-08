library(testthat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(capstone)

test_that("clean data", {
  
  
  dat <- eq_clean_data()
  
  d1 <- dat[(dat$COUNTRY == "TURKEY") & (dat$DATE == as.Date("2036-07-10")),]
  
  expect_that(d1$DEATHS, equals(1000))
  expect_that(d1$YEAR, equals(2036))

})

#Map function tests
test_that("eq_map", {
  mapa<- eq_map()
  expect_that(digest(mapa), is_a("leaflet"))
})

