
library(shiny)
library(palmerpenguins)
data("penguins")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reactivity, an example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          checkboxGroupInput(
            "chosen_islands", 
            label = h3("Show penguins from:"), 
            choices = list("Torgersen Island" = "Torgersen",
                           "Biscoe Island" = "Biscoe", 
                           "Dream Island" = "Dream"),
            selected = "Torgersen"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("penguinplot"),
           tableOutput("penguintable")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  # create a plot of penguin sizes between species and islands
    output$penguinplot <- renderPlot({
      
      # filter dataset to only selected islands
      data_filtered <- penguins %>%
        filter(island %in% input$chosen_islands)

        # plot penguins from filtered dataset
      data_filtered %>%
        ggplot(aes(x=bill_length_mm, 
                   y=body_mass_g, bill_, 
                   color=species)) +
        geom_point(size = 3)+
        theme_bw(base_size = 18) +
        coord_cartesian(xlim = c(min(penguins$bill_length_mm, na.rm = T), 
                                 max(penguins$bill_length_mm, na.rm = T)),
                        ylim = c(min(penguins$body_mass_g, na.rm = T), 
                                 max(penguins$body_mass_g, na.rm = T))) +
        labs(x = "Bill Length (mm)",
             y = "Body Mass (g)") +
        theme(legend.position = c(.05,.95),
              legend.justification = c(0,1))
        
    })
    
    
    output$penguintable <- renderTable({
      
      data_filtered <- penguins %>%
        filter(island %in% input$chosen_islands)
      
      
      data_filtered %>%
        arrange(desc(body_mass_g)) %>%
        mutate(rank = c(1:n())) %>%
        dplyr::select(rank, island, species, sex, body_mass_g) %>%
        slice(1:10)
        
      
    })
}

# Run the application 
#shinyApp(ui = ui, server = server)

# Or, Run in showcase mode: 
reactlog_enable()
app <- shinyApp(ui = ui, server = server)
runApp(app, display.mode = "showcase")
shiny::reactlogShow()
