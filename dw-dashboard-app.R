# Interactive Dashboard for Drinking Water Funding 
# Worklog Here: https://docs.google.com/document/d/1sLpBgD32_SmQjgAgVCe_KZUwDSNOTNz3f5kES09f4AM/edit
# Created for Environmental Policy Innovation Center
# Created by Gabriel Watson on 04.07.2023

library(shiny)
library(leaflet)
library(googlesheets4)
library(tidyverse)
library(rgdal)
library(geojsonsf)
library(jsonlite)
library(sf)
library(scales)
library(htmltools)
library(shinyBS)
library(aws.s3)
library(shinyjs)
library(DT)
library(shinyalert)


## Major Components
## Data prep 
## Map 
## Table 
## Sidebar
## Open Modal 



ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(".leaflet-container { background: #FFFFFF; } #Sidebar {background-color: #ffffff;}"))
  ),
  sidebarLayout(
    div( id ="Sidebar",
         sidebarPanel(
           style = "position: fixed; height: 100%; overflow-y: auto; margin-left: -30px; width: 35%;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
           uiOutput("StateName", style = "font-size: 40px"), 
        #   uiOutput("SidebarLines", style = "padding-bottom: 20px; line-height: 15px"),
      #     uiOutput("ChartSelect", style = "margin-bottom:-10px"), 
       #    radioButtons("Chart_Select", "", choices = c("Percent of the population covered" = "Pop","Percent of water systems covered" = "Sys")),
        #   div(plotOutput("Chart"), style = "margin-left:-15px"),
           actionButton("Context", "View Table/Map", style = "display:inline-block; float:left; margin-top: 30px"), 
        #   img(src = 'epic-logo-transparent.png', height = '50px', width = '200px', style = "display:inline-block; float:center; margin-top: 20px; margin-left: 10px"),
           width = 3
         )),

    mainPanel(
    # DT::dataTableOutput("Table"),
     leafletOutput("Map", height = "100vh"),
      hidden(uiOutput("Table")),
      style = "margin-left: -15px;",
      width = 9),
    
    position = c("right"), fluid = TRUE),

)


server <- function(input, output) {

## DATA PREPREATION ##  
PPL_Data <- read_csv(get_object(object = "clean_data/srf_project_priority_lists/web_ppl_combined_clean_v2.csv", bucket = "water-team-data"))

Geo_Data <- geojson_sf("www/states_ak_hi_v3.geojson")

## Summarizing by state
## Adding color based on number of projects - This is subject to change/flexible ## 
PPL_State_Data_Geo <- PPL_Data %>%
                      mutate(Count = 1)%>%
                      left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
                      mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
                      select(NAME, `Funding Amount`,`Principal Forgiveness`,DAC,Count)%>%
                      group_by(NAME)%>%
                      summarize_if(is.numeric,sum)%>%
                      mutate(Color = ifelse(Count > 0 ,"#BDD7E7", Color))%>%
                      mutate(Color = ifelse(Count > 25,"#6BAED6", Color))%>%
                      mutate(Color = ifelse(Count > 50,"#3182BD", Color))%>%
                      mutate(Color = ifelse(Count > 75,"#08519c", Color))%>%
                      mutate(Color = ifelse(is.na(Count), "#D3D3D3",Color))

## Var Decleration ## 
SelectedDataReactive <- reactiveValues(df = data.frame(PPL_Data %>% filter(State == "Alabama")))


### END DATA PREPREATION ### 


### VISUAL COMPONENTS ### 

##### MAP ##### 
output$Map <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 4, maxZoom = 4,dragging = FALSE))%>%
    addPolygons(data = PPL_State_Data_Geo, 
                layerId = ~NAME, 
               # label = ~htmlEscape(label), 
                labelOptions = labelOptions(style = list("color" = "black","font-style" = "bold","font-size" = "12px")), 
                highlight = highlightOptions(weight = 2.5, color = "Black", bringToFront = TRUE),
                fillOpacity = 1, 
               fillColor = ~Color, 
                color = "white", 
                weight = 1.5)%>%
    addLegend("topleft", 
              colors = c("#D3D3D3", "#BDD7E7", "#6BAED6", "#3182BD", "#08519c"),
              labels = c("No project data", "1-25", "25-50","50-75","75+"),
              title = "Number of Declared Projects",
              opacity = 1)%>%
    setView(-95.5795, 36.8283, zoom = 4)
  
})

## OBSERVE EVENTS ## 
# Mapclick observe
observeEvent(input$Map_shape_click,{
  if(!is.na(PPL_State_Data_Geo %>% filter(NAME == input$Map_shape_click$id) %>% pull(`Funding Amount`)))
  {
      SelectedDataReactive$df <- PPL_Data %>%
      filter(State == input$Map_shape_click$id)
  }
  else
  {
    showNotification(paste0("No project data available for ", input$Map_shape_click$id, ", please choose another state"),type = "warning")
  }
})

  
#### End Map ##### 
#### Table #### 
## Note that renderDataTable needs to be wrapped in a renderUI to work with Shinyjs hidden... not sure why ## 
output$Table <- renderUI({
  DT::renderDataTable({SelectedDataReactive$df %>% arrange(desc(Funding.Amount))})
})

# Context switch observe 
observeEvent(input$Context, {
  toggle("Map", anim = FALSE,animType = "slide",time = 0.5)
  toggle("Table", anim = FALSE,animType = "slide",time = 0.5)
})
#### End Table 


#### SideBar ####
output$StateName <- renderText({SelectedDataReactive$df$State[1]})




}

shinyApp(ui = ui, server = server)