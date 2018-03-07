#' Convert UTM to other coordinate reference system.
#' 
#' @description Converts UTM coordinates to the other coordinate reference system.
#' 
#' @param point_ID character, Data point ID.
#' @param zone  integer, UTM zone.
#' @param northing integer, UTM northing.
#' @param easting integer, UTM easting. 
#' @param CRS_To  character, Defines the spatial coordination reference that you wish to transform.
#'        Default is BC Albers reference system.
#' @param class character, Define the class of returned objective. Currently this function supports
#'              either \code{table} or \code{sp} class. Default is \code{table}.                  
#'                  
#' @return  Reprojected objective.
#'                      
#' @importFrom sp SpatialPoints spTransform CRS SpatialPointsDataFrame
#' @importFrom data.table data.table
#'
#' @export
#' @docType methods
#' @rdname UTM_Convertor
#'
#' @author Yong Luo
setGeneric("UTM_Convertor", 
           function(point_ID, zone, northing, easting, CRS_To, class) {
             standardGeneric("UTM_Convertor")
           })

#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "character",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "character",
                class = "character"),
  definition = function(point_ID, zone, northing, easting, CRS_To, class = "character") {
    UTMTable <- data.table::data.table(point_ID = point_ID,
                                       Zone = zone, Northing = northing,
                                       Easting = easting)
    fullUTMInfor <- UTMTable[!is.na(Zone) & !is.na(Easting) & !is.na(Northing),]
    output <- UTMTable[!(point_ID %in% fullUTMInfor$point_ID),.(point_ID,
                                                                Longitude = as.numeric(NA),
                                                                Latitude = as.numeric(NA))]
    utmZones <- unique(fullUTMInfor$Zone)
    for(utmZone in utmZones){
      outputAdded <- fullUTMInfor[Zone == utmZone, ]
      crsUTMstring <- CRS(paste("+proj=utm +zone=", utmZone, sep=""))
      utmcoor <- SpatialPoints(cbind(outputAdded$Easting,
                                     outputAdded$Northing),
                               proj4string = crsUTMstring)
      longlatcoor <- spTransform(utmcoor, CRS(CRS_To))
      transformed <- data.table(attributes(longlatcoor)$coords)
      names(transformed) <- c("Longitude", "Latitude")
      outputAdded[, ':='(Longitude = transformed$Longitude,
                         Latitude = transformed$Latitude)]
      output <- rbindlist(list(output, outputAdded[,.(point_ID, Longitude, Latitude)]))
    }
    
    if(class == "table"){
      return(output[,.(point_ID, Longitude, Latitude)])
    } else if (class == "sp"){
      output_valid <- output[!is.na(Longitude) & !is.na(Latitude),]
      output_invalid <- output[!(point_ID %in% unique(output_valid$point_ID)),]
      row.names(output_valid) <- output_valid$point_ID
      utmcoor <- SpatialPointsDataFrame(cbind(output_valid$Longitude,
                                              output_valid$Latitude),
                                        data = output_valid,
                                        proj4string = CRS(CRS_To),
                                        match.ID = TRUE)
      if(nrow(output_invalid) > 0){
        warning(paste("The below point(s) can not be converted due to missing UTM information:\n",
                      data.frame(output_invalid[,.(point_ID)]),
                      sep = ""))
      }
      return(utmcoor)
    } else {
      stop("class is not properly defined.")
    }
  })


#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "character",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "missing",
                class = "character"),
  definition = function(point_ID, zone, northing, easting, class) {
    UTM_Convertor(point_ID, zone, northing, easting, 
                  CRS_To = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
                  +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                  class)
  })    

#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "character",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "character",
                class = "missing"),
  definition = function(point_ID, zone, northing, easting, CRS_To) {
    UTM_Convertor(point_ID, zone, northing, easting, 
                  CRS_To,
                  class = "table")
  }) 

#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "character",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "missing",
                class = "missing"),
  definition = function(point_ID, zone, northing, easting) {
    UTM_Convertor(point_ID, zone, northing, easting, 
                  CRS_To = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
                  +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                  class = "table")
  }) 

#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "missing",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "missing",
                class = "missing"),
  definition = function(zone, northing, easting) {
    UTM_Convertor(point_ID = as.character(1:length(zone)),
                  zone, northing, easting, 
                  CRS_To = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
                  +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                  class = "table")
  }) 

#' @export
#' @rdname UTM_Convertor
setMethod(
  "UTM_Convertor",
  signature = c(point_ID = "missing",
                zone = "integer",
                northing = "integer",
                easting = "integer",
                CRS_To = "missing",
                class = "character"),
  definition = function(zone, northing, easting, class) {
    UTM_Convertor(point_ID = as.character(1:length(zone)),
                  zone, northing, easting, 
                  CRS_To = "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
                  +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                  class)
  }) 


