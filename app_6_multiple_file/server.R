# -------------------------------------------------------------------------
# App 6. 
# Example of Shiny App built in two files (ui and server) instead of 1  (app.r)
# showcasing interactive plots with tooltips, and leaflet mapping
# -------------------------------------------------------------------------


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
