
# -------------------------------------------------------------------------
# App 1. 
# Example of basic Shiny App, using PalmerPenguins dataset 
# and default Shiny user interface layout
# -------------------------------------------------------------------------



# install libraries -------------------------------------------------------

library(shiny)
library(palmerpenguins)
library(dplyr)
library(RColorBrewer)
library(ggplot2)


# define dataset ----------------------------------------------------------
df <- penguins 
#reactlog_enable()


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

    output$penguinplot <- renderPlot({
      
      # create dataset filtered by user inputs
        plot_data <- df %>%
          dplyr::filter(bill_length_mm > input$bill_length_slider[[1]] &
                          bill_length_mm < input$bill_length_slider[[2]] ) %>%
          dplyr::filter(body_mass_g > input$body_mass_slider[[1]] &
                          body_mass_g < input$body_mass_slider[[2]])
      
        # create color scale depending on input color variable
        color_vals <-  df %>% distinct(get(input$color_by)) %>% pull()
        plot_colors <- brewer.pal(length(color_vals), "Dark2")
        names(plot_colors) <- color_vals
        
        

        # plot 
        plot_data %>%
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
                             values = plot_colors) +
          labs(x = "Bill Length (mm)",
               y = "Body Mass (g)") +
          theme(legend.position = "bottom")

    })
}


# Run the application
#shinyApp(ui = ui, server = server)

# Or, Run in showcase mode: 
app <- shinyApp(ui = ui, server = server)
runApp(app, display.mode = "showcase")
