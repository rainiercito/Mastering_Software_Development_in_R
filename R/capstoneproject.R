#Mastering Software Development in R Specialization Capstone Project
# Coursera Capstone Project
#The overall goal of the capstone project is to integrate the skills you have developed 
#over the courses in this Specialization and to build a software package that can be used 
#to work with the NOAA Significant Earthquakes dataset.
# Victor Rainier Cruz Perez
# rainiercp@gmail.com

#Documentation at http://rpubs.com/rainiero/Master_Software_Developing

#' Use the Use_vignette once to generate the vignette folder in the package's skeleton where the vignette documentation
#' made with rmarkdown will be made
#'use_vignette("capstoneproject")
#'Use the use_testthat() once to generate the test folder in the package's skeleton where the test scripts will be placed
#'
#'use_testthat()
#'

# Parameters and libraries needed
#' Funcion for reading and cleaning the Earthquake data
#' @return a dataframe which contains the Eathquake data filtered required for mapping in a timeline the data
# @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#'@examples
#'\dontrun{
#' eq_clean_data()
#' }
#'
#' @export
eq_clean_data<-function(){
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
  raw_data <- readr::read_delim("/Users/rainier/Desktop/CursoR2/capstone/signif.txt.tsv", 
                           col_names=T,delim = "\t",na = "-99")
    
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
# @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#'@examples
#'\dontrun{
#' eq_location_clean(eq_clean_data())
#' }
#'
#' @export
eq_location_clean<-function(datfram){
  LOCATION_NAME<-NULL
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}

# Funcion for ploting an Earthquake's Location timeline
# @param datfram is the dataframe that contains the Earthquake's data
# @param start_date is the starting date from when the Earthquakes are going to be ploted
# @param end_date is the end date up to the Earthquakes are going to be ploted
# @param countries is a vector which contains the list of Countries that are going to be plotted
## @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
# @export
#geom_timeline<-function(datfram, start_date="1980-01-01", end_date="2014-01-01", countries =c("MEXICO","USA", "JORDAN")){
 
#    datfram%>%
#    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
#    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%
#    ggplot(aes(x = datetime, y = EQ_MAG_ML/EQ_MAG_ML, color=DEATHS, size=EQ_MAG_ML)) +
#    geom_point(alpha = 0.5)+scale_y_continuous(limits=c(0.8,1.2),breaks=NULL) + xlab("Year") + ylab("")+
#    facet_grid(COUNTRY ~ .) +
#        theme(panel.grid.minor = element_blank(), panel.grid.major =element_blank())
#}

############################
#'
#'Funcion for ploting an Earthquake's Location timeline building a GEOM Function from scratch
#'
#'The GeomTimeLine will use a Dataframe compiled using the function eq_clean_data. 
#'The GeomTimeLine function is a prototype function which will be used as foundation for our geom_timeline function.
#'The GeomTimeLine function will take advantage of the ggplot2's geom_point.
#'Using the Earthquakes' dates as X-axis main values, the Y-axis value will be not relevant while plotting a timeline (horizontal bar)
#'The geom_point's size and colour will be defined by the Earthquake's magnitude
#'The GeomTimeLine was build using the Function Prototype provided in the Course's Material 4.7.1 Building a New Geom 
GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
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
#############################

# Function that will use the GeomTimeLine Prototype Function required to Plot a Timeline with the Earthquakes of a given country
#' @param datfram is the dataframe that contains the Earthquake's data
#' @param start_date is the starting date from when the Earthquakes are going to be ploted
#' @param end_date is the end date up to the Earthquakes are going to be ploted
#' @param countries is a vector which contains the list of Countries that are going to be plotted
#' @return In a plot an Earthquakes timeline which contains all Earthquakes of a Given Country or List of Countries between a set of dates
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_clean_data()) %>%
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
    geom = GeomTimeline, 
    mapping = mapping,
    data = data, 
    stat = stat, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}



# Funcion for ploting an Eartquakes's Location, magnitude in a timeline
# @param datfram is the dataframe that contains the Earthquake's data
# @param start_date is the starting date from when the Earthquakes are going to be ploted
# @param end_date is the end date up to the Earthquakes are going to be ploted
# @param countries is a vector which contains the list of Countries that are going to be plotted
# @param number is the number of most important Earthquakes that should be highligthed by printing their names
## @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
# @export
#geom_timeline_label<-function(datfram, start_date="1980-01-01", end_date="2014-01-01", countries =c("MEXICO","USA", "JORDAN"), number=3){
#    temp<-datfram%>%
#    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
#    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%
#    dplyr::group_by(COUNTRY) %>% top_n(number, EQ_MAG_ML)
    
#  datfram%>%
#    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
#    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%  
#    ggplot(aes(x = datetime, y = EQ_MAG_ML/EQ_MAG_ML, color=DEATHS, size=EQ_MAG_ML)) +
#    geom_point(alpha = 0.5)+scale_y_continuous(limits=c(0.8,1.2),breaks=NULL) + xlab("Year") + ylab("")+
#    geom_text(data=temp, aes(x = datetime, y=1.2, label = LOCATION_NAME), position="jitter" ) +
#    geom_hline( yintercept=0, size=1, scale =1 ) +
#   facet_grid(COUNTRY ~ .) +
#    theme(panel.grid.minor = element_blank(), panel.grid.major =element_blank())
   
#}


#####################
#'Funcion for ploting an Earthquake's Location timeline building a GEOM Function from scratch but it will include annotations
#'
#'The GeomTimeLineAnnotation will use a Dataframe compiled using the function eq_clean_data
#'The GeomTimeLineAnnotation function is a prototype function which will be used as foundation for our geom_timeline function
#'The GeomTimeLineAnnotation function will take advantage of the ggplot2's geom_point
#'Using the Earthquakes' dates as X-axis main values, the Y-axis value will be not relevant while plotting a timeline (horizontal bar)
#'The GeomTimeLineAnnotation will use a number of annotations that will be plotted (Number of Major Earthquakes to be highlighted)
#'The geom_point's size and colour will be defined by the Earthquake's magnitude
#'The GeomTimeLineAnnotation was build using the Function Prototype provided in the Course's Material 4.7.1 Building a New Geom 


GeomTimeLineAnnotation <- ggplot2::ggproto("GeomTimeLineAnnotation", Geom,
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

#####################

#' Funcion for adding the Eartquakes's Location labels to an Earthquake's timeline
#' @param datfram is the dataframe that contains the Earthquake's data
#' @param start_date is the starting date from when the Earthquakes are going to be ploted
#' @param end_date is the end date up to the Earthquakes are going to be ploted
#' @param countries is a vector which contains the list of Countries that are going to be plotted
#' @param number is the number of most important Earthquakes that should be highligthed by printing their names
#' @return the Earthquake's labels
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_clean_data()) %>%
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





# Funcion for plotting in a simple map where a set of Earthquakes occured
# @param Country is the Country that contains the Earthquake's data to be displayed
# @param annot_col is the annotation to be displayed using a pop up
# @param year is the starting year from where the data will be displayed
# @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
# @export
#eq_map_simple<-function(country ="MEXICO", annot_col="DATE", date="1980"){
#   map_data<-eq_clean_data() %>% 
#   dplyr::filter(COUNTRY == country & lubridate::year(datetime) >= date)
#  
#  MaP <- ggmap::get_map(country, zoom = 4)
#  ggmap::ggmap(MaP)+
#    geom_point(data = map_data, aes(x = LONGITUDE, y = LATITUDE, color = EQ_MAG_ML, alpha=0.5)) + theme_void() 
#}

#' Funcion for plotting in an interactive map the location of a set of Earthquakes ocurred in a given country
#' @param Country is the Country that contains the Earthquake's data to be displayed
#' @param annot_col is the annotation to be displayed using a pop up use date to display only the date or blank for enhanced data display
#' @param dat is the starting year from where the data will be displayed
# @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#'@examples
#'\dontrun{
#' eq_map()
#' }
#'
#' @export
eq_map<-function(country ="MEXICO", annot_col="date", dat="1980"){
  datetime <- NULL # For removing CMD errors
  country <- NULL #For removing errors
   map_data<-eq_clean_data()%>%
    dplyr::filter(COUNTRY == country & lubridate::year(datetime) >= dat)
  
  if(annot_col=="date"){
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(data = map_data, radius= 2, lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ paste("Info:", lubridate::ymd_hms(datetime)))
  }
  else {
    map_data<-eq_location_clean(map_data)
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(data = map_data, radius= 2, lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ paste("<b>Location:<b>", LOCATION_NAME, "<br />",
                                                                                                        "<b>Magnitude:</b>", EQ_MAG_ML, "<br />",
                                                                                                        "<b>Total Deaths:<b>",DEATHS))
  }  
}