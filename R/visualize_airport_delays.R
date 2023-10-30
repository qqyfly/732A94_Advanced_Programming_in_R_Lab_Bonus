#' Airport Delay Visualize
#' 
#' @name visualize_airport_delays
#' 
#' @description
#' visualize_airport_delays() creates a plot that visualize the mean delay of flights
#' in different airports using ggplot2 library
#' 
#' @import nycflights13
#' @import dplyr
#' @import ggplot2
#' 
#' @return Return plot
#' @export visualize_airport_delays
visualize_airport_delays <- function(){
  
  flight <- nycflights13::flights
  airport <- nycflights13::airports
  lat <- airport$lat
  lon <- airport$lon
  dep_delay <- flight$dep_delay
  arr_delay <- flight$arr_delay
  
}