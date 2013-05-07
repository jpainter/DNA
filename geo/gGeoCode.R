# google geocode from http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
## modified function to return status

# from vba code...
     # sUrl = "https://maps.googleapis.com/maps/api/geocode/xml?address=" & sFormatAddress & "&sensor=false"
     # sUrl = "http://maps.googleapis.com/maps/api/geocode/xml?address=" & sFormatAddress & "&sensor=false"

library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", 
                                  sensor = "false", http = "http://") {
#      root <- "https://maps.google.com/maps/api/geocode/"
     root = paste( http, "maps.googleapis.com/maps/api/geocode/", sep='')
  
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    status = x$status
    accuracy = x$results[[1]]$geometry$location_typ
    return(c(status, lat, lng, accuracy))
  } else {
    status = x$status

    return(c(status, NA, NA, NA))
  }
}

# test: 
gGeoCode("210 third Ave, Decatur, Ga, 30030")

