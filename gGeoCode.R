# google geocode from http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
## modified function to return status

library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
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
    accuracy = x$accuracy
    return(c(lat, lng, status, accuracy))
  } else {
    status = x$status
    return(c(NA,NA, status, NA))
  }
}

# test: 
gGeoCode("210 third Ave, Decatur, Ga, 30030")
