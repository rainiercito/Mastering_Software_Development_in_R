#' Mastering Software Development in R Specialization Capstone Project
#' Coursera Capstone Project
#' The overall goal of the capstone project is to integrate the skills you have developed 
#' over the courses in this Specialization and to build a software package that can be used 
#' to work with the NOAA Significant Earthquakes dataset.
#' Victor Rainier Cruz Perez
#' rainiercp@gmail.com

#'Documentation at http://rpubs.com/rainiero/Master_Software_Developing

#'Use the Use_vignette once to generate the vignette folder in the package's skeleton where the vignette documentation
#'made with rmarkdown will be made
#'use_vignette("capstoneproject")
#'Use the use_testthat() once to generate the test folder in the package's skeleton where the test scripts will be placed
#'
#'use_testthat()
#'

#' Function for reading the NOAA earthquake data file
#'
#' @param filename The name of the NOAA earthquake data file
#' @return This function returns tbl_df object (earthquake data) 
#' @note The function will stop If the filename does not exist (error message)
#' @import dplyr
#' @importFrom readr read_delim
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_data_read(filename)
#' }
#'
#' @export
eq_data_read <- function(filename) {
  
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_delim(filename, delim='\t',progress = FALSE)
  })
  dplyr::tbl_df(data)
  
}



#' Parameters and libraries needed for reading and cleaning the Earthquake data function
#' @param datfram is the dataframe that contains location names written in Uper case
#' @return a dataframe which contains the Eathquake data filtered required for mapping in a timeline the data
#' @importFrom tidyr unite
#'@examples
#'\dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_clean_data(eq_data_read(filename))
#' }
#'
#' @export
eq_clean_data<-function(datafram){
  COUNTRY <-NULL
  LOCATION_NAME <-NULL
  LATITUDE <-NULL
  LONGITUDE<-NULL
  YEAR<-NULL
  MONTH<-NULL
  DAY<-NULL
  HOUR<-NULL
  EQ_MAG_ML <-NULL
  DEATHS<-NULL
  datetime<-NULL
  #raw_data <- readr::read_delim("/Users/rainier/Desktop/CursoR2/capstone/signif.txt.tsv", 
   #                        col_names=T,delim = "\t",na = "-99")
   
  raw_data <-datafram 
  # "subset to the specific columns that will be required..." 
  clean_data <- raw_data %>%
   # dplyr::filter(FLAG_TSUNAMI != "Tsu") %>%       # taking out the Tsunami's datapoints
    dplyr::select(COUNTRY,LOCATION_NAME, LATITUDE, LONGITUDE,YEAR, MONTH, DAY, HOUR, EQ_MAG_ML,DEATHS) %>%
    dplyr::mutate_each(funs(gsub(".*:", "", LOCATION_NAME)),LOCATION_NAME)%>%
    dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE))%>%
    tidyr::unite(datetime, YEAR, MONTH, DAY, HOUR) %>%
    dplyr::mutate(datetime = lubridate::ymd_h(datetime))%>%
    dplyr::mutate(DEATHS=as.numeric(DEATHS))
  rm(raw_data)
  #returning the cleaned data
  eq_location_clean(clean_data)
  
}

#' Funcion for title case the Earthquake's Location Data-Name
#' @param datfram is the dataframe that contains location names written in Uper case
#' @return a dataframe which contains the Eathquake data filtered required for mapping in a timeline the data and the Tittle Case Location
#' @importFrom stringi stri_trans_totitle
#'@examples
#'\dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename)))
#' }
#'
#' @export
eq_location_clean<-function(datfram){
  LOCATION_NAME<-NULL
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}


# Function that will use the GeomTimeLine Prototype Function required to Plot a Timeline with the Earthquakes of a given country
#' @param mapping aesthetic mappings created by aes
#' @param data is the dataframe that contains the Earthquake's data
#' @param na.rm  will hepls to remove the NA values from the data frame
#' @param position position adjustment functio
#' @param stat The Layer's statistical transformation 
#' @param show.legend layer's legend
#' @param inherit.aes will indicate the default aesthetics overridng 
#' @param ... layer's other arguments
#' @return In a plot an Earthquakes timeline which contains all Earthquakes of a Given Country or List of Countries between a set of dates
#' @import ggplot2
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("MEXICO","USA", "JORDAN"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, 
                          data = NULL, 
                          na.rm = TRUE,
                          position = "identity",
                          stat = "identity",
                          show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    Geom = GeomTimeline, 
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}


#'Funcion for ploting an Earthquake's Location timeline building a GEOM Function from scratch
#'The GeomTimeLine will use a Dataframe compiled using the function eq_clean_data. 
#'The GeomTimeLine function is a prototype function which will be used as foundation for our geom_timeline function.
#'The GeomTimeLine function will take advantage of the ggplot2's geom_point.
#'Using the Earthquakes' dates as X-axis main values, the Y-axis value will be not relevant while plotting a timeline horizontal bar
#'The geom_point's size and colour will be defined by the Earthquake's magnitude
#'The GeomTimeLine was build using the Function Prototype provided in the Course's Material 4.7.1 Building a New Geom 
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 #<character vector of required aesthetics>
                                 required_aes = c("x"),
                                 #aes(<default values for certain aesthetics>)
                                 default_aes = ggplot2::aes(y = 0.1,
                                                            shape = 21,
                                                            size = 1,
                                                            colour = "blue",
                                                            alpha = 0.8,
                                                            stroke = 1,
                                                            fill = NA),
                                 #<a function used to draw the key in the legend>
                                 draw_key = ggplot2::draw_key_point,
                                 ## Function that returns a grid grob that will 
                                 ## be plotted (this is where the real work occurs)
                                 draw_panel = function(data, panel_scales, coord) {
                                   # Transform the data first
                                   coords <- coord$transform(data, panel_scales)
                                   
                                   #To create the Earthquake's timeline we will separate the task in two parts
                                   #1) The line over the X-axis from where it will be plotted the Earthquakes as Points
                                   #2) The points for each Earthquake of a given Country in between two dates (years)
                                   #The use of the Concept of Grobs
                                   
                                   # 1) Creating the X-axis line (timeline)
                                   Timeline_line_grobs <- grid::polylineGrob(x = grid::unit(rep(c(0, 1),
                                                                                                length(coords$y)),
                                                                                            "npc"), 
                                                                             y = rep(coords$y, each = 2),
                                                                             id.length = rep(2,length(coords$y)),
                                                                             gp = grid::gpar(col = "black", lwd = 0.3, lty = 1))
                                   
                                   # 2) Creating the points for each Earthquake of a Given Country
                                   Earthquakes_points_grobs <- grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(col = alpha(coords$colour, coords$alpha), fill = alpha(coords$fill, coords$alpha),
                                                     lwd = coords$stroke * .stroke / 2),
                                     fontsize = coords$size * .pt + coords$stroke * .stroke / 2
                                     
                                   )
                                   
                                   # Plotting both the Timeline (X-axis) and the Eartquakes Points
                                   grid::gTree(children = grid::gList(Timeline_line, Earthquakes_points_grobs))
                                 })


#' Funcion for adding the Eartquakes's Location labels to an Earthquake's timeline
#' @param mapping aesthetic mappings created by aes
#' @param data is the dataframe that contains the Earthquake's data
#' @param na.rm  will hepls to remove the NA values from the data frame
#' @param show.legend layer's legend
#' @param stat The Layer's statistical transformation 
#' @param position position adjustment functio
#' @param inherit.aes will indicate the default aesthetics overridng 
#' @param ... layer's other arguments
#' @return the Earthquake's labels
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(datetime >= "1980-01-01" & datetime <="2014-01-01" & COUNTRY == c("MEXICO","USA", "JORDAN"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
#' geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))
#'}
#'

#' @export
geom_timeline_label <- function(mapping = NULL, 
                                data = NULL, 
                                na.rm = TRUE,
                                show.legend = NA,
                                stat = "identity",
                                position = "identity",
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeLineAnnotation, 
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


GeomTimeLineAnnotation <- ggplot2::ggproto("GeomTimeLineAnnotation", ggplot2::Geom,
                                           #<character vector of required aesthetics>
                                           required_aes = c("x", "tags"),
                                           #aes(<default values for certain aesthetics>)
                                           default_aes = ggplot2::aes(y = 0.5, 
                                                                      number = NULL, 
                                                                      max_aes = NULL),
                                           #<a function used to draw the key in the legend>
                                           draw_key = draw_key_text,
                                           ## Function that returns a grid grob that will 
                                           ## be plotted (this is where the real work occurs)
                                           draw_panel = function(data, panel_scales, coord) {
                                             
                                             # Transform the data
                                             coords <- coord$transform(data, panel_scales)
                                             
                                             #To create the Earthquake's timeline with annothation we will separate the task in two parts
                                             #1) we will locate where the tags should be places and then
                                             #2) To add the annotation labels to the layer 
                                             
                                             #1) Creating the location in the timelines (X-axis) where the location names will be placed
                                             Timeline_seg_grobs <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"), 
                                                                                      y0 = grid::unit(coords$y, "npc"),
                                                                                      x1 = grid::unit(coords$x, "npc"), 
                                                                                      y1 = grid::unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                      default.units = "npc",
                                                                                      arrow = NULL,
                                                                                      name = NULL, 
                                                                                      gp = grid::gpar(), 
                                                                                      vp = NULL)
                                             
                                             #2) Adding the text to the grid
                                             Earthquake_text_grobs <- grid::textGrob(label = coords$tags, 
                                                                                     x = unit(coords$x, "npc"), 
                                                                                     y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                                                     rot = 60, 
                                                                                     just = "left",
                                                                                     gp = grid::gpar(fontsize = 8))
                                             
                                             # Plotting the Eartquakes location label over the timeline
                                             grid::gTree(children = grid::gList(Timeline_seg_grobs, Earthquake_text_grobs))
                                           }
)


#' Earthquakes Data in an Interactive Map.
#'
#' The Earthquakes will be mapped centered with their latitude and
#' longitude "epicenter". The epicenter is annotated based on an annot_col which the user can specify.
#' In addition, if the user specifies "popup_text" then a call to eq_create_label generates
#' the appropriate text for the popup.
#'
#' @references \url{http://rstudio.github.io/leaflet/}
#'
#' @param eq_clean The clean earthquake data in a tbl_df object.
#' @param annot_col Column in the tbl_df object to be used for annotation.
#'
#' @return This function returns an interactive map.
#'
#' @note If an invalid column name is provided, the function provides a warning
#' and uses the LOCATION_NAME column as teh annotation column.
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 1980) %>%
#' eq_map(annot_col = "datetime")
#' }
#'
#' @export
eq_map <- function(eq_clean=NULL, annot_col="datetime"){
  
  #test that correct columns are present
  all_columns <- colnames(eq_clean)
  
  stopifnot(any('datetime' %in% all_columns),any('LATITUDE' %in% all_columns),
            any('LONGITUDE' %in% all_columns),any('EQ_MAG_ML' %in% all_columns))
  
  #check to see if invalid column provided - print message and default to DATE
  if(!(any(annot_col %in% all_columns))) {
    warning("Invalid Column - DATE Displayed")
    annot_col = "datetime"
  }
  
  #call to leaflet
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_clean, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_MAG_ML,
                              weight=1, fillOpacity = 0.2, popup =~ paste(get(annot_col)))
  
}

#' Creates popup text for markers.
#'
#' This function generates HTML formatted text to be used in popups for map markers.
#'
#' @param eq_clean The clean earthquake data in a tbl_df object.
#' @return This function returns a character vector containing popup text to be used in a leaflet visualization.
#'
#' @examples
#' \dontrun{
#' filename<-system.file("data","earthquakes_data.txt.zip",package="capstone")
#' eq_location_clean(eq_clean_data(eq_data_read(filename))) %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 1980) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(eq_clean=NULL) {
  
  #test that correct columns are present
  all_columns <- colnames(eq_clean)
  
  stopifnot(any('LOCATION_NAME' %in% all_columns),any('EQ_MAG_ML' %in% all_columns),
            any('DEATHS' %in% all_columns))
  
  #Creating the "popup_text" without using NA Labels
  data2<- eq_clean %>% dplyr::select_(.dots=c('LOCATION_NAME','EQ_MAG_ML','DEATHS')) %>%
    dplyr::mutate(new_LOCATION_NAME = ~ ifelse(is.na(LOCATION_NAME), LOCATION_NAME, paste0("<b>Location:</b> ", LOCATION_NAME,"<br />"))) %>%
    dplyr::mutate(new_EQ_PRIMARY = ~ ifelse(is.na(EQ_MAG_ML), EQ_MAG_ML, paste0("<b>Magnitude:</b> ", EQ_MAG_ML,"<br />"))) %>%
    dplyr::mutate(new_DEATHS = ~ ifelse(is.na(DEATHS), DEATHS, paste0("<b>Total Deaths:</b> ", DEATHS))) %>%
    tidyr::unite('popup_values',c('new_LOCATION_NAME','new_EQ_PRIMARY','new_DEATHS'),sep ='') %>%
    dplyr::mutate(popup_values = ~ stringr::str_replace_all(popup_values,"[,]*NA[,]*","")) %>%
    dplyr::mutate(popup_values = ~ ifelse(popup_values=="","All Values are NA",popup_values))
  
  popup_values <- dplyr::collect(dplyr::select(data2,.dots=c('popup_values')))[[1]]
  
  return(popup_values)
  
}
