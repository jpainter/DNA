
# ==== open file  ====

     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))
      # load(paste(directory, "edn.geo_May17.rda", sep=""))
      # edn.geo.old = edn.geo

     # confirm load
     library(lubridate)
     table(year(edn.geo$ArrivalDate), useNA = 'always')
 
# Deduplicate function ====
dedupe.edn.geo = function(){

     cat("there are ", nrow(edn.geo), " records.  \n")
     #      Find records with identical AlienID and AddressID
     dupes = duplicated(edn.geo[,c("AlienID","AddressID")])
     cat("Of these, ", sum(dupes), " have same alienID and AddressID. \n")
     
     # remove outdated records
     edn.geo = edn.geo[!dupes,]
     cat("After removing duplicates, there are now ", nrow(edn.geo), " records. \n ")

     # To eliminate aliens with >1 address, find records with identical AlienID 
     # and select one with most recent (largest) AddressID
     dupes = duplicated(edn.geo[,c("AlienID")])
     cat("There are ", sum(dupes), " alienID that are duplicated. \n")
     edn.dupes = edn.geo[dupes,]
     edn.dupes = edn.geo[edn.geo$AlienID %in% edn.geo[dupes,"AlienID"],]
     edn.dupes = edn.dupes[order(edn.dupes$AlienID, edn.dupes$AddressID, 
                                 decreasing=TRUE, na.last = TRUE),]

  
     library(plyr)
     keepers = ddply(edn.dupes[, c("AlienID", "AddressID")],  .(AlienID), 
                    summarise, BestAddressID = max(AddressID)
                     )
     
     cat("Of these, ", nrow(keepers), " should be kept.  \n")
     # get addressIDs for all records in duplicates that are not keepers
     outdated = edn.dupes[!edn.dupes$AddressID %in% keepers$BestAddressID,"AddressID"]
     cat("Of these, ", length(outdated), " with old AddressID.  \n")

     # remove outdated records
     edn.geo = edn.geo[!(edn.geo$AddressID %in% outdated),]
     cat("There are now ", nrow(edn.geo), " records. \n ")
    
     return(edn.geo)
     # Save edn.geo to secure directory
     # save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))
     }

     # deduplicate
     edn.geo = dedupe.edn.geo()
     
     # Save edn.geo to secure directory
     save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))

# ==== get new records from EDN ====
     library(RODBC)
     edn <-odbcConnect(dsn='edn')
     
     #### NOTE: query currently set to retreive records from years >2010 only

     updateDataSQL =  # paragraph returns included for readablility. 
          "SELECT 
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
              where year(dbo_AlienAddress.DateOfNotification)>2005
              ORDER BY dbo_AlienAddress.DateOfNotification DESC"

    ## optional where clause
    # where year(dbo_Alien.DateOfArrival)>2006 and 
    # year(dbo_AlienAddress.DateOfNotification)>2009

     # remove paragraph marks (\n) from query
     query = gsub("\n", "", updateDataSQL)
     query = gsub("dbo_", "", query)
     
     # set strings to not be imported as factors, then run query
     options(stringsAsFactors = FALSE)
     arrivals <- sqlQuery(edn, query)

# Add records to existing data set  ====

     # load edn dataset
     directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
     load(paste(directory, "edn.geo.rda", sep=""))

     format(nrow(edn.geo), big.mark=",")
     library(lubridate)
     t = table(year(edn.geo$ArrivalDate), edn.geo$Status, useNA = 'always')
     t = addmargins(t, c(1,2))
     print(t)
     
     # find new records in arrival data that have new AddressId
     oldAddress = arrivals$AddressID %in% edn.geo$AddressID
     newRecords = arrivals[!oldAddress,]
     cat("there are ", format(nrow(newRecords), big.mark=","), " new records to add to edn.geo")

     # add new records to edn.geo
     library(plyr)
     edn.geo = rbind.fill(edn.geo, newRecords)
     cat("there are now ", format(nrow(edn.geo), big.mark=","), " in edn.geo")
     
     # deduplicate
     edn.geo = dedupe.edn.geo()

     # Save edn.geo to secure directory
     save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))

    format(nrow(edn.geo), big.mark=",")
    t = table(year(edn.geo$ArrivalDate), edn.geo$Status, useNA = 'always')
    t = addmargins(t, c(1,2))
    print(t)
     
#### EDN Geocode function ====
      setwd("geo/")
     ednGeocode = function(n=10, header="http://"){
          source('gGeoCode.R')
          start=Sys.time()
          
          directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
          load(paste(directory, "edn.geo.rda", sep=""))
          
          # Filter and sort records without geocode
          recordsToGeocode = !(edn.geo$Status %in% c("OK", "ZERO_RESULTS"))
          
          cat("there are ", format(nrow(edn.geo), big.mark=","), " records in edn.geo.  ")
          hasGeocode = edn.geo$Status %in% c("OK", "ZERO_RESULTS")
          cat("Of those, ", 
                      format(nrow(edn.geo[!hasGeocode,]), big.mark=","),
                      "records are without geocode. \n")
          
          # limit data to number of records to geocode
          # order by oldest record in this year, 
          # then previous years (e.g. by Year (desc), Notification (asc))
          update.data = edn.geo[recordsToGeocode,]
          update.data = update.data[order(update.data$NotificationDate, 
                                          decreasing=TRUE),]
          update.data = update.data[1:n,]
          
          # geocode while status is 'ok'
          ok = 0
          for (i in 1:n){
               address = paste(update.data[i, "Address1"], ",", 
                               update.data[i,"City"], "," , 
                               update.data[i, "State"], "," ,
                               update.data[i, "Zip"])
               cat(i, ")", update.data[i, "AlienID"], "  ", 
                    format(update.data[i, "NotificationDate"], "%B-%d-%Y"), " ...", 
                    address,"\n") 
               
               # 'Test address1 for leading # sign, as in: 
               # #3b 3730 W. LELAND, chicago, il, 60625
               # 'If found, remove # and number.
               address = gsub("[[:punct:]]", " ", address)

               geocode = gGeoCode(address, http=header)
               
               if (geocode[1] %in% "OVER_QUERY_LIMIT"){ 
                    print("OVER THE QUERY LIMIT")
                    return(update.data)
                    # DayInSeconds = 5; 26*60*60
                    # cat("over query limit at", Sys.time())
                    # Sys.sleep(DayInSeconds)
               } 
               
               if (geocode[1]=="OK"){ok = ok + 1 }
               cat("   ", geocode[1], geocode[4],"\n")
                              
               update.data[i, "Status"] = geocode[1]
               update.data[i, "Latitude"] = as.numeric(geocode[2])
               update.data[i, "Longitude"] = as.numeric(geocode[3])
               update.data[i, "Accuracy"] = geocode[4]
               Sys.sleep(.1) # wait 0.1 seconds between calls
          }
          cat( ok, " of" , i, "records were geocoded.\n")
          
          Stop=Sys.time()
          print(difftime(Stop, start, unit="min"))
          return(update.data)
     }
     
#### geocoding....
      # delay, code, repeat ... 
        maxit = 24
        directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
        load(paste(directory, "edn.geo.rda", sep=""))
        for (i in 1:maxit){
          # alternate http and https when i is odd or even
           http = ifelse( i %% 2 == 0, "https://","http://")
           
           new.geo = ednGeocode(n=200, header=http)
           
           new.geo = new.geo[new.geo$Status %in% c("OK", "ZERO_RESULTS"),]
           nrow(new.geo)
           
           # update edn.geo with new geo
           rownames(edn.geo) = edn.geo$AddressID
           rownames(new.geo) = new.geo$AddressID
           edn.geo[rownames(new.geo), ] = new.geo
      
           ## save file
           save(edn.geo, file = paste(directory, "edn.geo.rda", sep=""))     
           
           # pause (in seconds)
           pause.edngeo = Sys.time()
           pause = 1*60*60
           
           # If not the last iteration, pause. print summary table
           if (!(i==maxit)){
             
             cat("Finished iteration ", i, ". The system will now pause at ", 
                 format(pause.edngeo), 
                 " for ", pause/60 , " minutes \n")
             
             # table of geocode for records since 2006 
             library(lubridate)
             t = table(year(edn.geo[, "NotificationDate"]), 
                       edn.geo[, "Status"], 
                       useNA = 'always')
             t = addmargins(t,c(1,2))
             print(t) 
            
             Sys.sleep(pause)
             
           } else { 
             cat("there are ", format(nrow(edn.geo), big.mark=","), " records in edn.geo")
             
             # table of geocode for records since 2006  
             t = table(year(edn.geo[, "NotificationDate"]), 
                       edn.geo[, "Status"], 
                       useNA = 'always')
             t = addmargins(t,c(1,2))
             print(t) 
             hasGeocode = edn.geo$Status %in% c("OK", "ZERO_RESULTS")
             print(paste("There are ", 
                         format(nrow(edn.geo[!hasGeocode,]), big.mark=","),
                         "records without geocode"))
        }}


#### fix records with missing state and country codes =====
  directory = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/"
  load(paste(directory, "edn.geo.rda", sep=""))

      rownames(edn.geo) = edn.geo$AddressID
      rownames(arrivals) = arrivals$AddressID
      str(edn.geo)
      str(arrivals)
      sum(is.na(arrivals$Country))
      sum(is.na(arrivals$Latitude))
      
      arrivals[rownames(edn.geo), "Latitude" ] = edn.geo[rownames(edn.geo), "Latitude"]
      arrivals[rownames(edn.geo), "Longitude" ] = edn.geo[rownames(edn.geo), "Longitude"]
      arrivals[rownames(edn.geo), "Status" ] = edn.geo[rownames(edn.geo), "Status"]
      arrivals[rownames(edn.geo), "Accuracy" ] = edn.geo[rownames(edn.geo), "Accuracy"]
      arrivals[rownames(edn.geo), "ReturnedAddress" ] = edn.geo[rownames(edn.geo), "ReturnedAddress"]
      arrivals$ReturnedAddress = as.character(arrivals$ReturnedAddress)
      table(arrivals$Country, useNA="always") # Country is country.code
      arrivals = arrivals[!is.na(arrivals$Country),]
      table(arrivals$State, useNA="always") # Country is country.code
      no.country = is.na(arrivals$Country)
      sum(no.country)
      table(year(arrivals[,"NotificationDate"]))
      ####
      edn.geo.old = edn.geo
      edn.geo = arrivals



#       colnames(edn.geo)
#       table(edn.geo$Country, useNA="always")
#       table(edn.geo$country.code, useNA="always")
#       str(edn.geo$country.code)
#       str(arrivals$Country)
#       table(arrivals$Country, useNA="always") # Country is country.code
#       # unfactor country.code, replace values from arrivals
#       edn.geo$country.code = as.character(edn.geo$country.code)
#       edn.geo$Country = as.character(edn.geo$Country)
#       # set country to country.code
#       edn.geo[rownames(arrivals), "Country" ] = 
#         arrivals[rownames(arrivals), "Country" ]
#       table(edn.geo$Country, useNA="always")
#       
#       table(edn.geo$state.abb, useNA="always")
#       table(edn.geo$State, useNA="always")
#       str(edn.geo$state.abb)
#       str(edn.geo$State)
#       table(arrivals$State, useNA="always") # Country is country.code
#       # unfactor country.code, replace values from arrivals
#       edn.geo$state.abb = as.character(edn.geo$state.abb)
#       # set State to state.abb
#       edn.geo[rownames(arrivals), "state.abb" ] = 
#         arrivals[rownames(arrivals), "State" ]
#       table(edn.geo$state.abb, useNA="always")
#       
#       # Remove weird colnames
#       keep.cols = colnames(edn.geo[,-c(21:38)])
#       keep.cols
#       edn.geo = edn.geo[, keep.cols] 
#       str(edn.geo)
#       edn.geo$ReturnedAddress = as.character(edn.geo$ReturnedAddress)
#       colnames(edn.geo)
#       keep.cols = colnames(edn.geo[,-c(2:14, 16:20)])
#       keep.cols
#       edn.geo = edn.geo[, keep.cols] 
#       str(edn.geo)

# ===== Summary ====    
     # number notifications by year

     t =  table(year(edn.geo$NotificationDate), 
           edn.geo$Status, 
           useNA = 'always')
     t = addmargins(t, c(1:2))
     print(t)

     # number arrivals by year
     badDate = edn.geo$ArrivalDate<mdy("January 1 2006")
     t =  table(year(edn.geo[!badDate, "ArrivalDate"]), 
                edn.geo[!badDate, "Status"], 
                useNA = 'always')
     t = addmargins(t, c(1:2))
     print(t)

     
 #  ===== Add long names for country and state
    ##### need to fix....this causes duplicate columns
    setwd("geo/")
    # add country names
    load("country.lat.long.rda") # country codes and names (edn has codes only)
    country.cols = colnames(country.lat.long)[c(2:6,8)]
    countries = country.lat.long[,country.cols]
    str(countries)
    colnames(countries)
    colnames(countries)[5] = "country.name"
    colnames(countries)[6] = "country.code"
    colnames(countries)

    edn.geo = merge(edn.geo, countries,
                       by.y="country.code", 
                       by.x="Country",  
                       all.x=TRUE,
                       sort=FALSE, sufficxes='')
    colnames(edn.geo)
    str(edn.geo)
    table(edn.geo$country.name, useNA='always')
    no.country = is.na(edn.geo$country.name)
    table(edn.geo[no.country,"Country"], useNA="always")
    table(edn.geo[no.country,"Country"],
          year(edn.geo[no.country,"NotificationDate"]),
          useNA="always")
    
     
     # add state names
      str(edn.geo)
     load("state.codes.rda")
      str(state.codes)
      colnames(state.codes)
      states = state.codes[,c(1:3,8)]
      states$abb = as.character(states$abb)
     edn.geo = merge(edn.geo , states, 
                      by.x="State", by.y="abb",
                     all.x = TRUE,
                     sort=FALSE, sufficxes='')
     colnames(edn.geo)
      table(edn.geo$state, useNA='always')
      no.state = is.na(edn.geo$state)
      table(edn.geo[no.state,"State"], useNA="always")
      table(edn.geo[no.state,"State"],
            year(edn.geo[no.state,"NotificationDate"]), 
            useNA="always")
                     
     
#      save(edn.geo, file = paste(directory, "edn.geo.rda", sep="")) 
     

