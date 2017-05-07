<<<<<<< HEAD
library(testthat)
library(dplyr)
library(lubridate)
library(ggplot2)
=======
>>>>>>> origin/master
library(capstone)

test_that("clean data", {
  
  filename <- system.file("data", "signif.txt.tsv", package = "capstone")
  raw_data <- read_delim(filename, delim = "\t")
  
  dat <- eq_clean_data()
  
  d1 <- dat[(dat$COUNTRY == "TURKEY") & (dat$DATE == as.Date("2036-07-10")),]
  
  expect_that(d1$DEATHS, equals(1000))
  expect_that(d1$YEAR, equals(2036))

})

#Map function tests
test_that("eq_map", {
<<<<<<< HEAD
  mapa<- eq_map()
  expect_that(digest(mapa), is_a("leaflet"))
})

=======
  require(digest)
  mapa<- eq_map()
  expect_that(digest(mapa), equals("ef17429b1ba0c4a0ad1d4b9b9976e75e"))
})
>>>>>>> origin/master
