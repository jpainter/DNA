library(ggplot2)


shinyServer(function(input, output) { 
     
     
     output$main_plot <- renderPlot(function() {
          
          
          hot.spot.map(
                         digit = as.numeric(input$digit) ,
                         country = input$country ,
                         state = input$state ,
                         min=as.numeric(input$min) , 
                         top= ifelse(input$top, input$top.num, NA)
                    )
          
         
     })
     
     

     
})
