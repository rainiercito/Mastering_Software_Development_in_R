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
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
eq_clean_data<-function(){
    raw_data <- readr::read_delim("/Users/rainier/Desktop/CursoR2/capstone/signif.txt.tsv", 
                           col_names=T,delim = "\t",
                         na = "-99")
  # "subset to the specific columns that will be required..." 
  clean_data = raw_data %>%
   # dplyr::filter(FLAG_TSUNAMI != "Tsu") %>%       # taking out the Tsunami's datapoints
    dplyr::select(COUNTRY,LOCATION_NAME, LATITUDE, LONGITUDE,YEAR, MONTH, DAY, HOUR, EQ_MAG_ML,DEATHS) %>%
    dplyr::mutate_each(funs(gsub(".*:", "", LOCATION_NAME)),LOCATION_NAME)%>%
    dplyr::mutate(LATITUDE= as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE= as.numeric(LONGITUDE))%>%
    tidyr::unite(datetime, YEAR, MONTH, DAY, HOUR) %>%
    dplyr::mutate(datetime = ymd_h(datetime))%>%
    dplyr::mutate(DEATHS=as.numeric(DEATHS))
  rm(raw_data)
  #returning the cleaned data
  eq_location_clean(clean_data)
  
}
#' Funcion for title case the Earthquake's Location Data-Name
#' @param datfram is the dataframe that contains location names written in Uper case
#' @return a dataframe which contains the Eathquake data filtered required for mapping in a timeline the data and the Tittle Case Location
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
eq_location_clean<-function(datfram){
  datfram = datfram%>%
    dplyr::mutate(LOCATION_NAME=stringi::stri_trans_totitle(LOCATION_NAME))
  datfram
}

#' Funcion for ploting an Earthquake's Location timelinee
#' @param datfram is the dataframe that contains the Earthquake's data
#' @param start_date is the starting date from when the Earthquakes are going to be ploted
#' @param end_date is the end date up to the Earthquakes are going to be ploted
#' @param countries is a vector which contains the list of Countries that are going to be plotted
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
geom_timeline<-function(datfram, start_date="1980-01-01", end_date="2014-01-01", countries =c("MEXICO","USA", "JORDAN")){
 
    datfram%>%
    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%
    ggplot(aes(x = datetime, y = EQ_MAG_ML/EQ_MAG_ML, color=DEATHS, size=EQ_MAG_ML)) +
    geom_point(alpha = 0.5)+scale_y_continuous(limits=c(0.8,1.2),breaks=NULL) + xlab("Year") + ylab("")+
    facet_grid(COUNTRY ~ .) +
        theme(panel.grid.minor = element_blank(), panel.grid.major =element_blank())
}



#' Funcion for ploting an Eartquakes's Location, magnitude in a timeline
#' @param datfram is the dataframe that contains the Earthquake's data
#' @param start_date is the starting date from when the Earthquakes are going to be ploted
#' @param end_date is the end date up to the Earthquakes are going to be ploted
#' @param countries is a vector which contains the list of Countries that are going to be plotted
#' @param number is the number of most important Earthquakes that should be highligthed by printing their names
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
geom_timeline_label<-function(datfram, start_date="1980-01-01", end_date="2014-01-01", countries =c("MEXICO","USA", "JORDAN"), number=3){
    temp<-datfram%>%
    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%
    dplyr::group_by(COUNTRY) %>% top_n(number, EQ_MAG_ML)
    
  datfram%>%
    dplyr::filter(datetime >= start_date & datetime <=end_date & COUNTRY == countries)%>%
    dplyr::mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))%>%  
    ggplot(aes(x = datetime, y = EQ_MAG_ML/EQ_MAG_ML, color=DEATHS, size=EQ_MAG_ML)) +
    geom_point(alpha = 0.5)+scale_y_continuous(limits=c(0.8,1.2),breaks=NULL) + xlab("Year") + ylab("")+
    geom_text(data=temp, aes(x = datetime, y=1.2, label = LOCATION_NAME), position="jitter" ) +
    geom_hline( yintercept=0, size=1, scale =1 ) +
    facet_grid(COUNTRY ~ .) +
    theme(panel.grid.minor = element_blank(), panel.grid.major =element_blank())
   
}

#' Funcion for plotting in a simple map where a set of Earthquakes occured
#' @param Country is the Country that contains the Earthquake's data to be displayed
#' @param annot_col is the annotation to be displayed using a pop up
#' @param year is the starting year from where the data will be displayed
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
eq_map_simple<-function(country ="MEXICO", annot_col="DATE", date="1980"){
   map_data<-eq_clean_data() %>% 
  dplyr::filter(COUNTRY == country & lubridate::year(datetime) >= date)
  
  MaP <- get_map(country, zoom = 4)
  ggmap(MaP)+
    geom_point(data = map_data, aes(x = LONGITUDE, y = LATITUDE, color = EQ_MAG_ML, alpha=0.5)) + theme_void() 
}

#' Funcion for plotting in an interactive map the location of a set of Earthquakes ocurred in a given country
#' @param Country is the Country that contains the Earthquake's data to be displayed
#' @param annot_col is the annotation to be displayed using a pop up use date to display only the date or blank for enhanced data display
#' @param dat is the starting year from where the data will be displayed
#' @import stringi magrittr readr dplyr ggplot2 tidyr lubridate ggmap stringi plotly leaflet
#' @export
eq_map<-function(country ="MEXICO", annot_col="date", dat="1980"){
   map_data<-eq_clean_data()%>%
    dplyr::filter(COUNTRY == country & lubridate::year(datetime) >= dat)
  
  if(annot_col=="date"){
  leaflet() %>%
    addTiles() %>%
   addCircleMarkers(data = map_data, radius= 2, lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ paste("Info:", lubridate::ymd_hms(datetime)))
  }
  else {
    map_data<-eq_location_clean(map_data)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = map_data, radius= 2, lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ paste("<b>Location:<b>", LOCATION_NAME, "<br />",
                                                                                                        "<b>Magnitude:</b>", EQ_MAG_ML, "<br />",
                                                                                                        "<b>Total Deaths:<b>",DEATHS))
  }  
}