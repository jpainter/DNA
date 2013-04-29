library(shiny)


### get geocoded data
#      library(RODBC)  
#      edn.geocode = "//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/EDN geocode.accdb"
#      edn.con = odbcConnectAccess2007(edn.geocode)
#      
#      qry = "SELECT Country, State, ArrivalDate, AlienType, TBClassID,
#                       Longitude, Latitude FROM Table1 WHERE [ArrivalDate] 
#                          between CDATE('",
#                  date1,
#                  "') AND CDATE('",
#                  date2,
#                  "')",
#                  sep = "")
#      
#      edn = sqlQuery( edn.con, qry)
#      odbcCloseAll()
#      # add country names
#      load("country.lat.long.RData") # country codes and names (edn has codes only)
#      colnames(edn)[colnames(edn) %in% "Country"]<-"country.code"
#      edn = merge(country.lat.long, edn, 
#                  by.x="FIPS", by.y="country.code",  all.y=TRUE)

#      # add state names
#      load("state.codes.rda")
#      colnames(edn)[colnames(edn) %in% "State"]<-"state.abb"
#      edn = merge(state.codes, edn ,
#            by.x="abb", by.y="state.abb", all.x = TRUE)
#      save(edn, file = "edn.rda" )

     countries = unique(as.character(edn.geo$Country))
     country.list = c("All", countries[order(countries)])
     state.list = c("All", unique(edn.geo$state))

# Load dataset  

     source("hot_spots.r")
      
shinyUI(bootstrapPage(
     
     headerPanel("Visa Medical Exams - Notification Clusters"),
     
     sidebarPanel(
          
          wellPanel(
               selectInput(inputId = "country",
                           label = "Departure country",
                           choices = c("All", country.list),
                           selected = "All"
                    ) 
               ) ,
          wellPanel(
               
               selectInput(inputId = "state",
                           label = "Arrival state",
                           choices = c("All", state.list),
                           selected = "All"
                    ) 
               ),
          
          wellPanel(
               selectInput(inputId = "digit",
                           label = "Cluster area",
                           choices = c("0.5 sq. miles" = 2,
                                       "50 sq. miles" = 1,
                                       "500 sq. miles" = 0),
                           selected = "50 sq. miles"
                           ),
               
#                uiOutput("x_range_slider")
#           )

                    sliderInput(inputId = "min", label = "Minimum Number of Persons",
                                min = 1, max = 2000, step = 100, value = 500
                                ),
       

                    checkboxInput(inputId = "top",
                                  label = strong("Find top clusters"),
                                  value = TRUE
                                  ),
               
                    sliderInput(inputId = "top.num", label = "Number of clusters",
                           min = 1, max = 100, step = 1, value = 20
                                )
          ),
          
          
          # This unused, invisible slider is necessary because of a bug
          conditionalPanel(
               condition = "false",
               sliderInput("nothing", "This does nothing:",
                           min = 1, max = 1000, value = 100)
          )
     ),
     
     mainPanel(
          plotOutput(outputId = "main_plot"),
          
          conditionalPanel("input.min == true",
                           p(strong("Minimum number persons")),
                           verbatimTextOutput(outputId = "min_text")
                           ),
          
          conditionalPanel("input.top == true",
                           p(strong("Top clusters")),
                           verbatimTextOutput(outputId = "top_text")
                           )
          )
          ))