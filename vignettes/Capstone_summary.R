## ----eval = FALSE--------------------------------------------------------
#  library(devtools)
#  install_github("rainiercito/Mastering_Software_Development_in_R")
#  library(capstone)

## ----eval = FALSE--------------------------------------------------------
#  #set the location of the file
#  filename<-system.file("data","earthquakes_data.txt.zip",package="captsone")
#  #read the data only
#  eq_data <- eq_data_read(filename)
#  #have a clean data set after readining it
#  eq_clean <- eq_clean_data(eq_data_read(filename))
#  #have a clean data set after cleaning the Location_Name Column
#  eq_location_clean(eq_clean_data(eq_data_read(filename)))

## ----eval = FALSE--------------------------------------------------------
#  #set the location of the file
#  filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#  #generate timeline for USA earthquakes after 2000
#  readr::read_delim(filename, delim = "\t") %>%
#  eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#  dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("MEXICO","USA", "JORDAN"))%>%
#  ggplot() +
#  geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))

## ----eval = FALSE--------------------------------------------------------
#  filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#  eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#  dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("MEXICO","USA", "JORDAN"))%>%
#  ggplot() +
#  geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
#  geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))

## ----eval=FALSE----------------------------------------------------------
#  filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#  eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
#  eq_map(annot_col = "DATE")

## ----eval = FALSE--------------------------------------------------------
#  filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#  eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
#  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#  eq_map(annot_col = "popup_text")

