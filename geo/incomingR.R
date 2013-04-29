

# ==== run in R session
#      system(R -e "source('~/incomingR.R')")
            
#  ===== Get records from Access database.  Thereafter, use R to geocode.
# library(RODBC)  
#      edn.geocode = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/EDN geocode.accdb"
#      edn.con = odbcConnectAccess2007(edn.geocode)
#      qry = "SELECT * FROM Table1" 
#      edn.geo = sqlQuery( edn.con, qry)
#      odbcCloseAll()
#      
#      str(edn.geo)
# 
# # limit to records with reasonable ArrivalDate
#      geo = edn.geo[year(edn.geo$ArrivalDate)>2005,]
# 
#      table(year(geo$ArrivalDate), geo$Status, useNA='always')
#      accuracy = table(geo$Accuracy)
#      per.acc = 100 * prop.table(accuracy)
#      accuracy = cbind( accuracy, as.numeric(format(per.acc, digits=2 )))
#      colnames(accuracy)[2] = "%" ; accuracy
# 
#      table(edn.geo$AlienType, useNA='always')
#      
# # update visa type from EDN
#      edn.con = odbcConnect("edn")
#      qry.visa = "SELECT AlienID, 
#                CASE WHEN immigranttype = 'I' THEN 'Immigrant' 
#                WHEN immigranttype = 'K1' THEN 'K1-FIANCEE' 
#                WHEN immigranttype = 'P-N' THEN 'Parolee no benefits' 
#                WHEN refugeetype = 'A' THEN 'Asylee' 
#                WHEN refugeetype = 'P-R' THEN 'Parolee with refugee benefits' 
#                WHEN refugeetype = 'R' THEN 'Refugee' 
#                WHEN refugeetype = 'SIV' THEN 'Special Immigrant Visa' 
#                ELSE 'Unknown' END as VisaType
#                FROM Alien" 
#      edn.visa = sqlQuery( edn.con, qry.visa)
#      odbcCloseAll()
#      str(edn.visa)
# 
#      edn.geo = merge(edn.geo, edn.visa, all.x=TRUE)
#      
# 
# # Save edn.geo to secure directory
#      directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
#      save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))
  
# ==== open file

     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))

     # confirm load
     library(lubridate)
#      table(year(edn.geo$ArrivalDate), useNA = 'always')
     
# ==== get new records from EDN
     
     


# ==== geocode records
     
     # === progress bar?
     # progress bar
     #      library(R.utils)
     #      total <- 20
     #      # create progress bar
     #      pb <- txtProgressBar(min = 0, max = total, style = 3)
     #      for(i in 1:total){
     #           Sys.sleep(0.1)
     #           # update progress bar
     #           setTxtProgressBar(pb, i)
     #      }
     #      close(pb)
     
     # see google geocode from 
     ## http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
     ### modified function to return status and accuracy
     
     source("gGeoCode.R")     

     # order by oldest record in this year, 
     # then previous years (e.g. by Year (desc), Notification (asc))
     edn.geo = edn.geo[order(edn.geo$NotificationDate, decreasing=TRUE),]
     nrow(edn.geo)
     
     # index records without geocode
     recordsToGeocode = which( !(edn.geo$Status %in% "OK") & !(edn.geo$Status %in% "ZERO_RESULTS"))
     print(paste(format(nrow(edn.geo[recordsToGeocode,]), big.mark=","), "records without geocode"))
     
     # geocode while status is 'ok'
     ok = 0
     for (i in 1:nrow(edn.geo[recordsToGeocode,]) ){
          record = edn.geo[recordsToGeocode[i],]
          address = paste(record$Address1, ",", record$city,
                          "," , record$state, "," ,
                          record$zip)
          print(paste(record$NotificationDate, " ...", address)) 
          # 'Test address1 for leading # sign, as in: #3b 3730 W. LELAND, chicago, il, 60625
          # 'If found, remove # and number.
          address = address
          
          
          geocode = gGeoCode(address)
          if (geocode[1]=="OK"){ok = ok + 1 }
          print(paste(geocode[1], geocode[4], ok))
          
          edn.geo[recordsToGeocode[i],]$Status = geocode[1]
          edn.geo[recordsToGeocode[i],]$Latitude = as.numeric(geocode[2])
          edn.geo[recordsToGeocode[i],]$Longitude = as.numeric(geocode[3])
          edn.geo[recordsToGeocode[i],]$Accuracy = geocode[4]
          
 
          if (geocode[1] %in% "OVER_QUERY_LIMIT"){ 
               DayInSeconds = 26*60*60
               print(paste("over query limit at", Sys.time()))
               Sys.sleep(DayInSeconds)
               } 
     }
     
     paste( ok, " of" , i, "records were geocoded.")
     recordsToGeocode = which( !(edn.geo$Status %in% "OK") &
                                    !(edn.geo$Status %in% "ZERO_RESULTS"))
     paste("now there are",format(nrow(edn.geo[recordsToGeocode,]), big.mark=","), 
           "records without geocode")
     
     save(edn.geo, file = paste(directory, "edn.geo.rda", sep="")) 
     
     head(edn.geo[recordsToGeocode, c(1,6,9:12)], 15)
     badDate = edn.geo$NotificationDate<mdy("January 1 2006")
     table(year(edn.geo[!badDate, "NotificationDate"]), 
           edn.geo[!badDate, "Status"], 
           useNA = 'always')
     
 #  ====  Add long names for country and state
     
     # rename cols
     colnames(edn.geo)[colnames(edn.geo) %in% "Country"]<-"country.code"
     colnames(edn.geo)[colnames(edn.geo) %in% "state"]<-"state.abb"
     
     # add country names
     load("country.lat.long.RData") # country codes and names (edn has codes only)
     edn.geo = merge(country.lat.long, edn.geo, 
                     by.x="FIPS", by.y="country.code",  all.y=TRUE)
     colnames(edn.geo)[colnames(edn.geo) %in% "FIPS"]<-"country.code"
     
     # add state names
     load("state.codes.rda")
     edn.geo = merge(state.codes, edn.geo ,
                     by.x="abb", by.y="state.abb", all.x = TRUE)
     colnames(edn.geo)[colnames(edn.geo) %in% "abb"]<-"state.abb"
     
     save(edn.geo, file = paste(directory, "edn.geo.rda", sep="")) 
     
