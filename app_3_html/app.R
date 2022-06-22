
# -------------------------------------------------------------------------
# App 3. 
# Example of custom shiny HTML app, using data from PalmerPenguins dataset, 
# and customized interface using html and a css stylesheet. 
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
  
  theme = 'style.css', # add css file
  
  # background stock image
  fluidRow(
    HTML('<style>
        .parallax {
          /* The image used */
          background-image: url("penguins.jpg");

          /* Set a specific height */
            height: 650px;

          /* Create the parallax scrolling effect */
          background-attachment: fixed;
          background-position: center;
          background-repeat: no-repeat;
          background-size: cover;
          filter: grayscale(15%) blur(0px) sharpen(2px);
          }
        </style>
        <!-- Container element -->
        <div class="parallax"></div>'
    )),
  
  
  # title panel
  fluidRow(id = "title",
           HTML("<center>
                <br>
                <h1>Palmer Penguins</h1>
                <h3>Freeform HTML Layout</h3>
                <br>
                </center>")
  ),
  
  # start about row -----
  fluidRow(id="about",
           column(1),
           column(10,
                  column(2),
                  column(8,
                         br(),br(),br(),
                         HTML("<p><b> Shiny Apps with HTML </b> <br>
                           This example codes a user interface mostly without
                           using default shiny elements. Here, we use fluidRows, 
                           columns, and divs to create the interface for our app.
                           Add elements using HTML coding such as fluidRows and
                           columns, and edit styles in the external .css file.
                           This style of UI development offers the most freedom
                           in layout of your shiny app, but can be confusing to 
                           navigate. </p>"),
                         br(),br(),br()),
                  column(1)),
           column(2)), ## end about row -----
  
  # body 
  fluidRow(
    id = "about",
    column(1),
    column(10,
           column(1),
           column(4,
                  
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
                               selected = "species")
           ),
           column(6,
                  plotOutput("penguinplot", 
                             height = "600px")
           ),
           column(1)
    ),
    column(1)
  )
    
  )
  
  




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
      theme(legend.position = "bottom",
            plot.background = element_rect(color = "transparent", fill = "transparent"),
            panel.background = element_rect(color = "transparent", fill = "transparent"),
            legend.background = element_blank())
    
  }, bg = "transparent")
}

# Run the application 
shinyApp(ui = ui, server = server)
