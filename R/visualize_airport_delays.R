#' Visualize Airport Delays
#' 
#' This function creates a plot that visualizes the mean delay of flights for different airports by longitude and latitude.
#' 
#' @return Returns a graph with the mean delay values for airports within the United States, plotted by their map coordinates 
#' 
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#' @import maps
#' 
#' @export visualize_airport_delays

library("nycflights13")
library(dplyr)
library(ggplot2)
library(maps)

visualize_airport_delays <- function(){
  flights_data<-select(flights,dep_delay,arr_delay,flight,origin,dest)
  flights_data_dest<-group_by(flights_data,dest)
  airport_data<-select(airports,dest=faa,lat,lon)
  combine_data<-right_join(airport_data,flights_data_dest,by="dest")
  delay<-combine_data %>% na.omit() %>% group_by(dest) %>% summarise(mean_delay = mean(dep_delay))
  clean_data<-as.data.frame(right_join(airport_data,delay,by="dest"))
  map_us<-map_data("usa")
  plot_data<-ggplot(map_us, aes(x = lon, y = lat))+
    geom_point(data = clean_data,aes(x=lon,y=lat), color = "blue", size = 4) +
    geom_label(data = clean_data,aes(label=round(mean_delay,1)), angle = 30, hjust = 0, color = "black")
    
  return(plot_data)
}