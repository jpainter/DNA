# google geocode from http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
## modified function to return status

# from vba code...
     # sUrl = "https://maps.googleapis.com/maps/api/geocode/xml?address=" & sFormatAddress & "&sensor=false"
     # sUrl = "http://maps.googleapis.com/maps/api/geocode/xml?address=" & sFormatAddress & "&sensor=false"

library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, 
                                  return.call = "json", 
                                  sensor = "false",
                                  header="http://") {
     root <- paste(header, "maps.googleapis.com/maps/api/geocode/", sep="")
     u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
     return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE, http="http://") {

  u <- construct.geocode.url(address, header=http)
  if(verbose) {
       cat("address : ", address, "\n")
       cat("u : ", u, "\n")
  }
  
  doc <- getURL(u  ,
                .opts = list(capath = system.file("CurlSSL", "cacert.pem", 
                                                  package = "RCurl"), 
                             ssl.verifypeer = FALSE))
  
  if(verbose) { cat("doc : ", doc, "\n")  }
  
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
# gGeoCode("210 Third Ave, Decatur, Ga, 30030", http="https://", verbose=TRUE)
# gGeoCode("1600 Clifton Road, Atlnta, Ga, 30333", http="https://", verbose=TRUE)

# to avoid error with https, see 
# http://stackoverflow.com/questions/3442781/rgoogledocs-or-rcurl-giving-ssl-certificate-problem
#  options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))