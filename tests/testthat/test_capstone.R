test_that("filename is valid",{
  expect_that(eq_data_read("file_not_here"),throws_error())
})

test_that("eq_data_read returns a tbl_df", {
  filename <- system.file("data","earthquakes_data.txt.zip",package="capstone")
  expect_is(eq_data_read(filename), "tbl_df")
})


#Map function tests
test_that("eq_map creates leaflet visualization", {
  filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
 
  expect_that(eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000) %>%
                eq_map(annot_col = "datetime"), is_a("leaflet"))
})