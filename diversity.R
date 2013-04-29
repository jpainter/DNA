
# diveristy
# in degree decimal coordinates, 1 degree~70 miles; 0.1~7miles
load("edn.rda")
str(edn)

latlong = data.frame(lat=round(edn$Latitude,1) , long=-round(edn$Longitude,1))
row.names(latlong) = edn$AlienID
latlong = latlong[which(latlong$long>0 & latlong$lat>0),]  #limit to pos long -- removes guam
head(latlong)
d = dist(latlong[1:100,])
h = hclust(d)
p = plclust(h, hmin=30)
plot(h)



