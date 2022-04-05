
# -------------------------------------------------------------------------
# App 4. 
# Return to App 1, demonstrating different kinds of reactivity
# -------------------------------------------------------------------------



# install libraries -------------------------------------------------------

library(shiny)
library(palmerpenguins)
library(dplyr)
library(RColorBrewer)
library(ggplot2)


# define dataset ----------------------------------------------------------
df <- penguins 



# start user interface ----------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Palmer penguins - default shiny layout"),
  
  # Sidebar with a user input widgets 
  # see widget gallery https://shiny.rstudio.com/gallery/widget-gallery.html
  sidebarLayout(
    sidebarPanel(
      
      # bill length slider widget
      sliderInput("bill_length_slider",
                  "Show Bill Lengths Between:",
                  min = 30,
                  max = 60,
                  value = c(35,55)),
      
      # body mass slider widget
      sliderInput("body_mass_slider",
                  "Show Body Masses  Between:",
                  min = 2000,
                  max = 7000,
                  value = c(2500,6500)),
      
      # color variable button widget
      radioButtons("color_by", 
                   "Color by:",
                   choices = list("Species" = "species", 
                                  "Sex" = "sex"), 
                   selected = "species"),
      
    ),
    
    
    # show plot
    mainPanel(
      plotOutput("penguinplot", height = "600px")
    )
  )
) # end ui -----




# start server ------------------------------------------------------------
server <- function(input, output) {
  
  
  # create a reactive object, which automatically creates a dataset that reacts
  # to user inputs. Since this reactive dataset is always listening, we will
  # refer to it as a function: plot_data() when used.
  plot_data <- reactive({
    df %>%
      dplyr::filter(bill_length_mm > input$bill_length_slider[[1]] &
                      bill_length_mm < input$bill_length_slider[[2]] ) %>%
      dplyr::filter(body_mass_g > input$body_mass_slider[[1]] &
                      body_mass_g < input$body_mass_slider[[2]])
  })
  
  
  # create a reactive object for the color palette of the plot.
  # use switch() to change color palettes depending on the input$color_by variable
  # then name the vector elements, and return the named vector.
  # again, since this is reactive, we will call it as a funciton: plot_colors().
  plot_colors <- reactive({
    
    plot_colors <- switch(input$color_by,
                          "species" = c("Magenta2","DarkCyan","Purple"),
                          "sex" = c("darkorange1","darkorchid","grey80"))
    
    names(plot_colors) <- df %>% distinct(get(input$color_by)) %>% pull()
    
    return(plot_colors)
  })
  

  # 
  output$penguinplot <- renderPlot({
    
    # plot 
    plot_data() %>%
      ggplot(aes(x=bill_length_mm, 
                 y=body_mass_g, bill_, 
                 color=get(input$color_by))) +
      geom_point(size = 3)+
      theme_bw(base_size = 18) +
      coord_cartesian(xlim = c(min(df$bill_length_mm, na.rm = T), 
                               max(df$bill_length_mm, na.rm = T)),
                      ylim = c(min(df$body_mass_g, na.rm = T), 
                               max(df$body_mass_g, na.rm = T))) +
      scale_color_manual(name = input$color_by,
                         values = plot_colors()) +
      labs(x = "Bill Length (mm)",
           y = "Body Mass (g)") +
      theme(legend.position = "bottom")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
