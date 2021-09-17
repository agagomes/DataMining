#introduction to RNOAA package: http://spatialecology.weebly.com/r-code--data/34
library(sp)
library('rnoaa')
library(lubridate)
library(stringr)
require(devtools)
options(noaakey = "mqEuOSuAUjyuGlTjVjxxCpzRlbrooRnr")
#To gain access to NCDC CDO Web Services, you must obtain a token using this link and following the directions given. http://www.ncdc.noaa.gov/cdo-web/token
library(biogeo)
#Get available stations
#station_data <- ghcnd_stations() # Takes a while to run and you can load form the available R object 
load("station_data.RData")


ConvUnit <- function(coordinates) {
  counter=0
  if(str_sub(coordinates,1,1)== '-'){
    coordinates<-str_sub(coordinates,2,nchar(coordinates))
  }
  if(str_sub(coordinates,1,2)== "00"){
    if(nchar(coordinates)==8){
      str_sub(coordinates,9,12)<-".000"
    }
    coordinates<-str_sub(coordinates,4,nchar(coordinates))
    str_sub(coordinates,6,6)<-':'
    str_sub(coordinates,9,10)<-str_sub(coordinates,8,9)
    str_sub(coordinates,8,8)<-'.'
  }
  database$lon[database$id==5939]="8.59"
  database$lon[database$id==5921]="7.52"
  for(x in 1:nchar(coordinates)){
    if(x==nchar(coordinates)){
      return(coordinates)
    }
    if(isTRUE(str_sub(coordinates,x,x)== ':' | str_sub(coordinates,x,x)== '\'' | str_sub(coordinates,x,x)=='º')){
      str_sub(coordinates,x,x) = ' '
      counter=counter+1
    }
    if(counter==2){
      coordinates<-gsub(",", ".", str_sub(coordinates,1,nchar(coordinates)))
      coordinates<-str_remove_all(coordinates, "'")
      if(grepl("E", coordinates, fixed = TRUE)){
        str_sub(coordinates,x+1,nchar(coordinates))<-format(as.numeric(str_sub(coordinates,x+1,nchar(coordinates))),scientific=F)
      }
      return(conv_unit(coordinates, "deg_min_sec", "dec_deg"))
    }
    if(isTRUE(str_sub(coordinates,x,x)== '-')){
      return(NA)
    }
  }
}


getTemp <- function(district,lat,long,alert_date) {
  #define the GPS coordinates of a fire event 
  df <- data.frame(
    id = c(as.character(district)), 
    latitude = c(as.numeric(lat)),
    longitude = c(-as.numeric(long)),
    stringsAsFactors = FALSE
  )
  print(df)
  
  #Get nearby stations that can provide the Maximum temperature (TMAX)
  nearby_stations <-  meteo_nearby_stations(lat_lon_df = df,
                                            station_data = station_data, radius = 1000, 
                                            var = c("TMAX"),
                                            year_min = 2015, year_max = 2015)
  
  #Get TMAX data
  weather_data <- ghcnd_search(nearby_stations[[1]]$id[1], var = c("TMAX") , date_min = "2015-01-01", date_max = "2015-12-31")
  
  return((weather_data))
}

