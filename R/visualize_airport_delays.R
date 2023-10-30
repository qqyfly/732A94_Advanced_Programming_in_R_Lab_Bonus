#' Visualize Delay & Distance Relationship
#' 
#' @name visualize_airport_delays
#' 
#' @description
#' visualize_airport_delays() creates a plot that visualize the relation between
#' mean(distance) and mean(delay) using ggplot2 library(using same destination)
#' @import nycflights13
#' @import dplyr
#' @import ggplot2
#' @importFrom stats dist
#' 
#' @return Return plot
#' @export visualize_airport_delays
visualize_airport_delays <- function(){
  
  # get data set
  flight <- nycflights13::flights
  
  dep_delay <- flight$dep_delay
  arr_delay <- flight$arr_delay
  
  # select wanted data
  myFlight<-select(flight,year,month,day,dep_delay,arr_delay,distance,dest)
  #remove null,na data
  myFlight<-filter(myFlight,!is.na(dep_delay),!is.na(arr_delay))
  
  # sort data in ASC order 
  myFlight<-arrange(myFlight,dep_delay)
  
  # Grouping data
  by_dest<-group_by(myFlight,dest)
  # Merging results
  delay<-summarise(by_dest,
                   count=n(),
                   dist=mean(distance,na.rm = TRUE),
                   delay=mean(arr_delay,na.rm = TRUE)) 
  # Let's pretend valid value we use is those number of flights greater than 10
  delay<-filter(delay,count > 15)
  
  ggplot(data = delay) +
    geom_point(mapping = aes(x=dist,y=delay)) +
    geom_smooth(mapping = aes(x=dist,y=delay))
}