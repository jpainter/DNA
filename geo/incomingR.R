

# ==== run in R session
#      system(R -e "source('~/incomingR.R')")
            
#  ===== Get records from Access database.  Thereafter, use R to geocode. ====
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
  
# ==== open file  ====

     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))

     # confirm load
     library(lubridate)
#      table(year(edn.geo$ArrivalDate), useNA = 'always')
 
# Deduplicate  ====
dedupe.edn.geo = function(){
     cat("there are ", nrow(edn.geo), " records.  ")
     #      Find records with identical AlienID and AddressID
     dupes = which(duplicated(edn.geo[,c("AlienID","AddressID")]))
     edn.dupes = edn.geo[dupes,]
     edn.dupes = edn.dupes[order(edn.dupes$ArrivalDate, edn.dupes$AlienID, 
                                 decreasing=TRUE, na.last = TRUE),] 
     cat("Of these, ", nrow(edn.dupes), " have same alienID and AddressID.  ")
     #      edn.dupes[, c("AlienID", "AddressID", "state", "Country", "ArrivalDate", "NotificationDate")]

     #      Find records with identical AlienID and select one with most recent (largest) AddressID
     dupes = edn.geo[duplicated(edn.geo[,c("AlienID")]), "AlienID"]
     edn.dupes = edn.geo[edn.geo$AlienID %in% dupes,]
     edn.dupes = edn.dupes[order(edn.dupes$AlienID, edn.dupes$AddressID, 
                                 decreasing=TRUE, na.last = TRUE),]
     cat("there are ", nrow(edn.dupes), " with same alienID. ")
  
     library(plyr)
     keepers = ddply(edn.dupes[, c("AlienID", "AddressID")],  .(AlienID), 
                    summarise, BestAddressID = max(AddressID)
                     )
     
     cat("Of these, ", nrow(keepers), " should be kept.  ")
     # get addressIDs for all records in duplicates that are not keepers
     outdated = edn.dupes[!edn.dupes$AddressID %in% keepers$BestAddressID,"AddressID"]
     cat("Of these, ", length(outdated), " with old AddressID.  ")

     # remove outdated records
     edn.geo = edn.geo[!edn.geo$AddressID %in% outdated,]
     cat("there are now ", nrow(x), " records.  ")

     # Save edn.geo to secure directory
     save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))
     }

# ==== get new records from EDN ====
     library(RODBC)
     edn <-odbcConnect(dsn='edn')

     updateDataSQL =  # paragraph returns included for readablility. 
          "SELECT TOP 3000 
               dbo_AlienAddress.AddressID, 
               dbo_AlienAddress.AlienID, 
               dbo_AlienAddress.Address1, 
               dbo_AlienAddress.City, 
               dbo_AlienAddress.State, 
               dbo_AlienAddress.Zip, 
               dbo_Alien.DateOfArrival as ArrivalDate, 
               dbo_AlienAddress.DateOfNotification as NotificationDate, 
               dbo_AlienDemographics.PresentCountryOfResidence AS Country, 
               dbo_AlienDemographics.ConsulateCity as Consulate, 
               dbo_AlienClassification.ClassB, 
               dbo_Alien.TBClassID,
               dbo_Alien.AlienType,
               CASE WHEN dbo_Alien.immigranttype = 'I' THEN 'Immigrant' 
                    WHEN dbo_Alien.immigranttype = 'K1' THEN 'K1-FIANCEE' 
                    WHEN dbo_Alien.immigranttype = 'P-N' THEN 'Parolee no benefits' 
                    WHEN dbo_Alien.refugeetype = 'A' THEN 'Asylee' 
                    WHEN dbo_Alien.refugeetype = 'P-R' THEN 'Parolee with refugee benefits' 
                    WHEN dbo_Alien.refugeetype = 'R' THEN 'Refugee' 
                    WHEN dbo_Alien.refugeetype = 'SIV' THEN 'Special Immigrant Visa' 
                    ELSE 'Unknown' END as VisaType
               
               FROM (    Select 
                         dbo_AlienAddress.AlienID, 
                         Max(dbo_AlienAddress.DateOfNotification) AS MaxOfDateOfNotification, 
                         Max(dbo_AlienAddress.addressID) AS AddressID
                         FROM dbo_AlienAddress
                         GROUP BY dbo_AlienAddress.AlienID
                    ) as MostRecentAlienAddress 
                    INNER JOIN (((dbo_AlienAddress 
                    INNER JOIN dbo_AlienDemographics ON dbo_AlienAddress.AlienID = dbo_AlienDemographics.AlienID) 
                    INNER JOIN dbo_AlienClassification ON dbo_AlienAddress.AlienID = dbo_AlienClassification.AlienID) 
                    INNER JOIN dbo_Alien ON dbo_AlienAddress.AlienID = dbo_Alien.AlienID) 
                    ON MostRecentAlienAddress.AddressID = dbo_AlienAddress.AddressID
               ORDER BY dbo_AlienAddress.DateOfNotification DESC"

     # remove paragraph marks (\n) from query
     query = gsub("\n", "", updateDataSQL)
     query = gsub("dbo_", "", query)

     arrivals <- sqlQuery(edn, query)

# Add records to existing data set  ====
     # load edn dataset
     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))
     format(nrow(edn.geo), big.mark=",")

     # find records from arrivals not in edn.geo
     library(compare)
     comparison <- compare(arrivals$AddressID, edn.geo$AddressID, allowAll=TRUE)
     comparison
     new.records = merge(arrivals, edn.geo, by = c('AlienID', 'AddressID'), 
                         all.x=TRUE, suffixes="")
     
     cat("there are ", nrow(new.records), " new records from ", 
         as.character(min(new.records$NotificationDate)), " to ",
         as.character(max(new.records$NotificationDate)))

     # add new records to edn.geo
     library(plyr)
     edn.geo = rbind.fill(edn.geo, new.records)
     cat("there are now ", format(nrow(edn.geo), big.mark=","), " in edn.geo")
     
     # Save edn.geo to secure directory
           save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))

# ==== geocode records  ====
     
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
     
