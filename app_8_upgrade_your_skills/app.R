## app.R ##
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(plotly)
library(reactable)
library(MetBrewer)
library(maps)


# begin ui ------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Upgrading your skills!"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "plots", icon = icon("bar-chart-o")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
      
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content -----
      tabItem(tabName = "plots",
              h2("Interactive Plots"),
              fluidRow(
                # 1. static plot -----
                box(title = "Good: ggplot",
                    status = "info",
                    solidHeader = T,
                    plotOutput("static_plot")),
                # 2. plotly plot -----
                box(title = "Better: ggplotly",
                    status = "info",
                    solidHeader = T,
                    plotlyOutput("interactive_plot"))
              
              )
      ),
      
      # Second tab content -----
      tabItem(tabName = "maps",
              h2("Interactive Maps"),
              
              # 1. static map -----
              box(title = "Good: ggplot map",
                  status = "info",
                  solidHeader = T,
                  plotOutput("static_map")
                  ),
              # 2. interactive map -----
              box(title = "Better: leaflet map",
                  status = "info",
                  solidHeader = T,
                  leafletOutput("interactive_map")
              )
      ),
      
      
      # Third tab content ----- 
      tabItem(tabName = "tables",
              h2("Interactive Tables"),
              
              # 1. static table -----
              box(title = "Good: R table",
                  status = "info",
                  solidHeader = T,
                  tableOutput("static_table")
              ),
              # 2. interactive table -----
              box(title = "Better: Reactable table",
                  status = "info",
                  solidHeader = T,
                  reactableOutput("interactive_table")
              )
      )
    )
  ) # end body
  
)



# begin server ------------------------------------------------------------


server <- function(input, output) {

  

# get plot data -----------------------------------------------------------
  df <- palmerpenguins::penguins
  pal <-  met.brewer("Hokusai3",n=3)
  
  

# Static Plot ------------------------------------------------------
  output$static_plot <- renderPlot({
    df %>%
      ggplot(
        aes(x = bill_length_mm,
            y = bill_depth_mm,
            color = species,
            text = glue::glue(
              "<b>Species:</b> {species}
          <b>Sex:</b> {sex}
          <b>Location:</b> {island} Island"))
      ) +
      geom_point(size = 3.5, alpha = .85) +
      theme_minimal(base_size = 18) +
      labs(x = "Bill Length (mm)",
           y = "Bill Depth (mm)",
           color = "Species") +
      theme(legend.position = "top") +
      scale_color_manual(values = pal)
    
  })

  
  

# Interactive Plot -------------------------------------------------
output$interactive_plot <- renderPlotly({
   plot <- df %>%
    ggplot(
      aes(x = bill_length_mm,
          y = bill_depth_mm,
          color = species,
          text = glue::glue(
            "<b>Species:</b> {species}
          <b>Sex:</b> {sex}
          <b>Location:</b> {island} Island"))
    ) +
    geom_point(size = 2.5, alpha = .85) +
     theme_minimal(base_size = 14) +
    labs(x = "Bill Length (mm)",
         y = "Bill Depth (mm)",
         color = "Species") +
    theme(legend.position = c(0,0)) +
    scale_color_manual(values = pal)
   
   ggplotly(plot, tooltip = c("text")) %>%
     layout(legend = list(
       orientation = "h",
       y = 1.15,
       x = .15
     )) %>%
     config(displayModeBar = F)  
     
  
})
  
  
  
  
  
# Get Map Data ------------------------------------------------------------
  data("us.cities")
  capitals <- us.cities %>%
    dplyr::filter(capital == 2) %>%
    
    mutate(label = paste0('<b>', name, '</b><br>',
                          '<strong> Population: </strong>', pop,"<br>"
    )) %>%
    mutate(label2 = purrr::map(.x = label, .f = HTML)) %>%
    dplyr::select(-label) %>%
    rename(label = label2)
  
  usa <- map_data('state')


# Create static map -------------------------------------------------------
  output$static_map <- renderPlot({
    usa %>%
      ggplot(aes(x=long, y=lat)) +
      geom_polygon(aes(group=group),
                   fill = "grey98",
                   color = "grey20")+
      geom_point(data = capitals %>% filter(!country.etc %in% c("AK","HI")),
                 aes(size = pop),
                 fill = "#309189",
                 shape = 21,
                 alpha=.7) +
      scale_radius(range = c(1,12)) +
      coord_map() +
      ggthemes::theme_map() +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size= 20)) +
      labs(size = "Population") 
  })
  
  

# Create Interactive Map --------------------------------------------------
  output$interactive_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98, lat = 35, zoom = 3) %>%
      addCircleMarkers(data = capitals,
                       lng = ~long,
                       lat = ~lat,
                       popup = ~label,
                       radius = ~pop/50000,
                       weight = 1,
                       color = "#309189",
                       fillOpacity = 0.5)
    
  }) 
  
  

# table data --------------------------------------------------------------
data("mtcars")
mtcars <- mtcars[,1:4]
  
  

# 1. Static Table ------------------------------------------------------------
output$static_table <- renderTable({
  
  mtcars
  
})
  

# 2. Interactive Table ----------------------------------------------------
output$interactive_table <- renderReactable({
  reactable(
    mtcars,
    columns = list(
      mpg = colDef(name = "Miles per Gallon", minWidth = 150),
      cyl = colDef(name = "Cyllinders", minWidth = 70),
      disp = colDef(name = "Displacement (cu.in.)", minWidth = 80),
      hp = colDef(name = "Horsepower", minWidth = 70)
      
      )
  )
    
})
  
  
  
}

shinyApp(ui, server)