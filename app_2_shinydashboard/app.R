
# -------------------------------------------------------------------------
# App 2. 
# Example of Shiny Dashboard App, using PalmerPenguins dataset 
# and layout from the style of shinydashboards package
# BONUS: Tab 2 features interactive tooltop ggplot using `ggplotly`
# -------------------------------------------------------------------------


# install libraries -------------------------------------------------------

library(shiny)
library(palmerpenguins)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(shinydashboard)
library(plotly)


# define dataset ----------------------------------------------------------
df <- penguins 



# start user interface ----------------------------------------------------
ui <-
  dashboardPage(
    skin = "blue",
    
    # dashboard header -----
    dashboardHeader(title = "Palmer Penguins"), ## end header -----
    
    # dashboard sidebar -----
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", 
                 tabName = "dashboard", 
                 icon = icon("dashboard", verify_fa = FALSE)
                 ),
        menuItem("Interactive Dashboard", 
                 tabName = "interactive_dashboard",
                 icon = icon("mouse", verify_fa = FALSE)
                 )
        
      )
    ), ## end sidebar -----
    
    
    # dashboard body -----
    dashboardBody(
      
      tabItems(
        ##1. first tab content -----
        tabItem(tabName = "dashboard",
                
                column(5,
                       box(width = 12, 
                           title = "User Inputs",
                           status = "primary",
                           sliderInput("bill_length_slider",
                                       "Show Bill Lengths Between:",
                                       min = 30,
                                       max = 60,
                                       value = c(35,55)),
                           
                           sliderInput("body_mass_slider",
                                       "Show Body Masses  Between:",
                                       min = 2000,
                                       max = 7000,
                                       value = c(2500,6500)),
                           
                           radioButtons("color_by", 
                                        "Color by:",
                                        choices = list("Species" = "species", 
                                                       "Sex" = "sex"), 
                                        selected = "species")
                           
                       ),
                ), # end column 1
                
                column(7,
                       box(
                         width = 12,
                         status = "primary",
                         plotOutput("penguinplot", height = "600px"))
                       
                ) # end column 2
                
        ), # end tab 1
        
        
        
        ## 2. second tab content -----
        tabItem(tabName = "interactive_dashboard",
                
                column(5,
                       box(width = 12, 
                           title = "User Inputs",
                           status = "primary",
                           sliderInput("bill_length_slider_tab2",
                                       "Show Bill Lengths Between:",
                                       min = 30,
                                       max = 60,
                                       value = c(35,55)),
                           
                           sliderInput("body_mass_slider_tab2",
                                       "Show Body Masses  Between:",
                                       min = 2000,
                                       max = 7000,
                                       value = c(2500,6500)),
                           
                           radioButtons("color_by_tab2", 
                                        "Color by:",
                                        choices = list("Species" = "species", 
                                                       "Sex" = "sex"), 
                                        selected = "species")
                           
                       ),
                ), # end column 1
                
                column(7,
                       box(
                         width = 12,
                         status = "primary",
                         plotlyOutput("penguinplot_interactive", height = "600px"))
                       
                ) # end column 2
                
        )
        
      ) # end tabitems
    ) # end dashboard body
    
    
  ) ## end UI -----
  




# start server ------------------------------------------------------------
server <- function(input, output) {
  
  
  # tab 1 plot -----
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
  
  # tab 2 plot -----
  output$penguinplot_interactive <- renderPlotly({
    ({
      
      # create dataset filtered by user inputs
      plot_data <- df %>%
        dplyr::filter(bill_length_mm > input$bill_length_slider_tab2[[1]] &
                        bill_length_mm < input$bill_length_slider_tab2[[2]] ) %>%
        dplyr::filter(body_mass_g > input$body_mass_slider_tab2[[1]] &
                        body_mass_g < input$body_mass_slider_tab2[[2]])
      
      # create color scale depending on input color variable
      color_vals <-  df %>% distinct(get(input$color_by_tab2)) %>% pull()
      plot_colors <- brewer.pal(length(color_vals), "Dark2")
      names(plot_colors) <- color_vals
      
      
      
      # plot 
      plot <- plot_data %>%
        ggplot(aes(x=bill_length_mm, 
                   y=body_mass_g,
                   color=get(input$color_by_tab2),
                   text = glue::glue(
                     "<b>Species:</b> {species},
                     <b>Sex:</b> {sex},
                     <b>Island:</b> {island},
                     <b>Body Mass:</b> {body_mass_g}g,
                     <b>Bill Length:</b> {bill_length_mm}mm"
                   )
                   )) +
        geom_point(size = 2)+
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
      
      ggplotly(plot,
               tooltip = c("text"))
      
    })
  })
  
} ## end server -----

# Run the application 
shinyApp(ui = ui, server = server)
