# Centered World Map
# ========================================================
#   - The problem:  trying to recenter a map causes open , wild looking, polygons
#   - Discussion of what happens when you try to [recenter a map](http://stackoverflow.com/questions/5353184/fixing-maps-library-data-for-pacific-centred-0-360-longitude-display) with a suggested function

# The function is called plot.map()

plot.map<- function(database,center,...){
  Obj <- map(database,...,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])
  
  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})
  
  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + center
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    if(sum(diff(x[,1])>300,na.rm=T) >0){
      id <- x[,1] < 0
      x <- rbind(x[id,],c(NA,NA),x[!id,])
    }
    x
  })
  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]
  
  map(Obj,...)
}


# and it can be used like this

plot.map("world", center=75, col="white",bg="gray",fill=TRUE,ylim=c(-60,90),mar=c(0,0,0,0))


# Alternatively, there a function to recenter a polygon or line (e.g. greatcircle route)
# need to [download the fortify spatial code](https://github.com/hadley/ggplot2/blob/master/R/fortify-spatial.r) into existing directory.

source ("fortify-spatial.r") 
Recenter = function(
  data=world,
  center=260,
  # positive values only - US centered view is 260
  shapeType=c("polygon","segment"),
  idfield=NULL
  # shift coordinates to recenter great circles
) {
  
  #use inherited id column, or create a new one from breaks in the data
  
  if(is.null(idfield)) {
    data$id=factor(cumsum(is.na(data$long)))
  }else{
    data$id = get(idfield,pos=data)
  }
  
  # shift coordinates to recenter worldmap
  
  data$long <- ifelse(data$long < center - 180 , data$long + 360, data$long)
  
  ### Function to regroup split lines and polygons
  # takes dataframe, column with long and unique group variable,
  #returns df with added column named group.regroup
 
  RegroupElements <- function(df, longcol, idcol){
    g 300) {
      # check if longitude within group differs more than 300 deg, ie if element was split
      d mean(range(df[,longcol]))
      # we use the mean to help us separate the extreme values
      g[!d] <- 1
      # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
      g[d] <- 2
      # parts that are moved
    }
g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
df$group.regroup <- g
df
  }

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable

ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  df
}

# now regroup

returnframe <- ddply(data, .(id), RegroupElements, "long", "id")

# close polys

if(shapeType[1]=="polygon") {
  returnframe <- ddply(returnframe, .(group.regroup), ClosePolygons, "long", "order") # use the new grouping var
}
ggplot(returnframe,aes(x=long,y=lat))+geom_polygon(aes(group=group.regroup))
returnframe
}
