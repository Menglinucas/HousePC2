#' @title Boundaries
#' @description Extract the administration bondary and effective position restricted by existed houses, of a city
#' @param district City name, character
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param enctype The encoding type, gbk or utf8 or else ? character
#' @return A list:
#' @return (1) administration boundary, data frame
#' @return (2) house boundary, data frame
#' @export
boundary<-function(district,host,port,user,password,dbname,enctype){
  
  ###################################################
  # the used libraries and functions:               #
  #  library(maps)                                  #
  #  library(mapdata)                               #
  #  library(sp)                                    #
  #  library(ggplot2)                               #
  #  library(maptools)                              #
  #  library(readxl)                                #
  #  library(mgcv)                                  #
  #  library(rgeos)                                 #
  #  library(raster)                                #
  #  library(RMySQL)                                #
  #  library(rgdal)                                 #
  #  source("segpiece.R", encoding = 'UTF-8')       #
  #  source("fortify-spatial.r",encoding = 'UTF-8') #
  ###################################################
  
  # projection: +init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0
  #  +units=m +nadgrids=@null +no_defs
  newproj <- CRS("+init=epsg:3857")
  
  ########################################################
  ########## fetch boundary from shapfile  ###############
  ########################################################
  #suppressWarnings(x<-readShapePoly('data/city/city.shp'))
  city<-subset(x,x@data$city_code==district)
  if (nrow(city@data) == 0){
    cat("There's no boundary shapfile data!!!")
    return(0)
  }
  
  ############################################################################################
  ##################### CRS transformation, and convert to dataframe #########################
  ############################################################################################
  projection(city) <- CRS("+init=epsg:4326")
  city1 <- plyr::ldply(city@polygons,fortify)  ## fortify maybe deprecated. then, can be instituted by broom::tidy
  city <- spTransform(city,newproj)
  # city<-segpiece(city)

  ##############################################################################################################
  ################################ reliable boundary by ershou and xin house ###################################
  ##############################################################################################################
  con <- dbConnect(MySQL(),host=host,dbname=dbname,user=user,password=password,port=port)
  dbSendQuery(con,enctype)
  str <- paste0("SELECT avg(x) AS x,avg(y) AS y FROM ha_position 
                          WHERE x >= ",min(city1$long)," and x <=",max(city1$long),
                          " and y >= ",min(city1$lat)," and y <=",max(city1$lat),
                          "GROUP BY ha_code")
  allh <- dbGetQuery(con, str)
  dbDisconnect(con)
  if (nrow(allh) < 10){
    cat("Housing area samples are less than 10 !!!")
    return(0)
  }

  # CRS transformation
  coordinates(allh)<-~x+y
  projection(allh) <- CRS("+init=epsg:4326")
  allh <- spTransform(allh,newproj)

  # # 2km buffer
  # housebd <- gBuffer(allh,width=200.,byid=TRUE)
  # housebd <- raster::aggregate(housebd)
  
  # # convert to dataframe
  # housebd<-segpiece(housebd)

  out<-list(city,allh)
  
  return(out)
  
}
