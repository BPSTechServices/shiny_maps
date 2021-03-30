library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafsync)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pdxlisfr <- readRDS("data/pdxlisfr_2017.rds")

data <- readRDS("data/low_income_sfr_2017.rds") 
sf::st_crs(data) <- 4326

choices2map <- names(data) %>% as.data.frame() %>% setNames("variable") %>%
    filter(!(variable %in% c("GEOID", "TRACT", "geometry"))) %>% 
    mutate(variable = as.character(variable)) %>% pull(variable)

# Define UI for mapping application
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "CHAS Data Explorer", titleWidth = "220"),
                    
                    dashboardSidebar(width = "220",
                                     
                                     div(style="padding: 15px 15px 15px 15px;", "This app allows you to map ", tags$a(href="https://www.huduser.gov/portal/datasets/cp.html","CHAS data"), " from HUD to determine the share of households that are considered ", tags$a(href="https://www.huduser.gov/portal/datasets/il.html","low-income"),"."),
                                     
                                     selectInput(inputId = 'user_var2map', 
                                                 label = 'Select variable to map:', 
                                                 selected = 'share_sfr_total_rent_sfr_li', 
                                                 choices = choices2map),
                                    
                                     div(style="padding: 15px 15px 15px 15px;", "Source: HUD CHAS data from 2013-17 ACS 5-year estimates, Tables 18A, 18B and 18C. Prepared March 30, 2021 by Portland Bureau of Planning & Sustainability. Contact point: ", tags$a(href="mailto:nick.kobel@portlandoregon.gov","Nick Kobel"), ".")),
                    dashboardBody(leafletOutput("map"))
)



# Define server logic required to map
server <- function(input, output) {
    
    data2map <- reactive({
        data %>% mutate(mapvar = !!sym(input$user_var2map))
    })

    output$map <- renderLeaflet({
        mapView(x = data2map(), zcol = "mapvar", layer.name = input$user_var2map) %>%
            .@map %>% leaflet::setView(-122.62, 45.54, zoom = 10)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

