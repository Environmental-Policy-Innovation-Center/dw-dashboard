# Interactive Dashboard for Drinking Water Funding 
# Worklog Here: https://docs.google.com/document/d/1sLpBgD32_SmQjgAgVCe_KZUwDSNOTNz3f5kES09f4AM/edit
# Created for Environmental Policy Innovation Center
# Created by Gabriel Watson on 04.07.2023

library(shiny)
library(leaflet)
library(googlesheets4)
library(dplyr)
library(readr)
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
library(janitor)
library(formattable)
library(reactable)
library(shinybrowser)
library(reactablefmtr)
library(stringr)
library(shinyWidgets)
library(waiter)
library(tippy)
library(shinycssloaders)
library(zip)
library(googledrive)
library(ggplot2)


## Major Components ----

ui <- fluidPage(
  detect(),
  useWaitress(),
  useShinyjs(),
  tags$head(
    tags$style(HTML(".leaflet-container { background: #FFFFFF;} 
                    .sidebar form.well { background: transparent;border: 0px;} 
                    .panel-primary>.panel-heading+.panel-collapse>.panel-body{border-right: 1px solid rgba(0, 0, 0, 0.05);}
                    .shiny-notification {position:fixed;top: calc(15%);left: calc(15%); max-width: 300px}"))
  ),
  sidebarLayout(
    div( id ="sidebar",
         sidebarPanel(
           style = "position: fixed; height: 82%; overflow-y: auto; margin-left: -30px;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
           uiOutput("StateName", style = "font-size: 40px; margin-top: -20px;"), 
           uiOutput("StateCategory", style = "font-size: 25px; margin-top: -10px; font-style: italic;"), 
           uiOutput("StateDescription", style = "height: 165px; max-width: 400px; overflow-y: scroll; margin-right: -15px; margin-bottom: 10px; "), 
           actionButton("Context", "National Map or State Data Table",icon(name = "arrows-left-right", lib = "font-awesome"),style = "margin-bottom: 10px; color: #a82b4d; border-color: #a82b4d;"), 
           actionButton("Chart_Summary", "Visualization",icon(name = "arrows-up-down", lib = "font-awesome"),style = "margin-bottom: 10px; color: #ffb448; border-color: #ffb448;"),
           shinyjs::hidden(uiOutput("ChartSelect")),
           shinyjs::hidden(uiOutput("ChartText", style = "font-style: italic; margin-bottom: 10px")),
           shinyjs::hidden(plotOutput("ChartOne")),
           bsCollapse(id = "CollapsePanel", open = c("Drinking Water Needs and Funding", "All Projects"), multiple = TRUE,
                      bsCollapsePanel("Drinking Water Needs and Funding",uiOutput("NeedsFunds", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "primary"),
                      bsCollapsePanel("All Projects",uiOutput("AllProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "info"),
                      bsCollapsePanel("General Projects",uiOutput("GeneralProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "none"),
                      bsCollapsePanel("Lead Projects",uiOutput("LeadProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "warning"),
                      bsCollapsePanel("Emerging Contaminant Projects",uiOutput("ECProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "success")),
           #    uiOutput("StateNumbers", style = "line-height: 20px"),
           #    radioButtons("Chart_Select", "", choices = c("Percent of the population covered" = "Pop","Percent of water systems covered" = "Sys")),
           #    div(plotOutput("Chart"), style = "margin-left:-15px"),
           downloadButton('DataDownload','Download State Data', style = "width: 300px; color: #e3672b; border-color: #e3672b;"),
           actionButton("showInfo", " Info", icon(name = "question", lib = "font-awesome")), 
           img(src = 'epic-logo-transparent.png', height = '40px', width = '150px', style = "margin-left: 5px; margin-top:5px"),

           width = 4
         )),
    
    mainPanel(
      leafletOutput("Map", height = "100vh"),
      hidden(uiOutput("StateNameTwo", style = "font-size: 30px; margin-left: 5px; position:relative; z-index: 500;"), 
             uiOutput("ColumnMouseover", style = "font-size: 15px; margin-left: 5px; position:relative; z-index: 500; font-style: italic; margin-top: -5px;"),
             uiOutput("Table", style = "margin-left: 5px; background-color: none;", width = "100px")),
      style = "margin-left: -15px;",
      width = 8),
    
    position = c("right"), fluid = TRUE),
  
)



server <- function(input, output, session) {

  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
  
  
  ## Data Prep ----
  
  waitress$inc(20) # increase by 10
  dw_data_raw <-  read_csv(get_object(object = "apps/dw-dashboard/dw-dashboard-data_v1-1.csv", bucket = "water-team-data"))
  ppl_data <- read_csv(get_object(object = "clean_data/srf_project_priority_lists/web_ppl_combined_clean.csv", bucket = "water-team-data"))

  waitress$inc(20) # increase by 10
  
  
  ## Importing Geo Data 
  geo_data <- geojson_sf("www/states_ak_hi_v4.geojson")

  waitress$inc(20) # increase by 10
  
  ## Importing Google sheets data (Tool tips)
  ##TODO: Replace with a pull to AWS, will need to make each sheet a separate CSV
  ##TODO: Create function for pulling Google Drive content, formatting, and pushing to AWS
  gs4_deauth()
  URL <- "https://docs.google.com/spreadsheets/d/1hznoLfB8zMzs3jKfLjs-uy9R68mcFsYMAUg1gKXC17Y/edit#gid=0"
  state_descriptions <- read_sheet(URL, sheet = "Text")
  
  waitress$inc(20) # increase by 10
  tooltip_text <- read_sheet(URL, sheet = "Tooltip")
  
  waitress$inc(10) # increase by 10
  column_tooltips <- read_sheet(URL, sheet = "ColumnToolTip")
  
  data_dictionary_index <- read_sheet(URL, sheet = "DataDictionaryIndex")
  
  glossary_link <- data_dictionary_index %>%
    filter(State == "Glossary") %>%
    pull(Link)
  ## Summarizing by state
  ## Adding color based on number of projects - This is subject to change/flexible ## 
  ## Adding Sabs Data 

  ppl_state_data_geo <- dw_data_raw %>%
    left_join(geo_data, ., by = c("NAME"= "State")) %>%
    mutate(Color = "") %>%
    mutate(Color = ifelse(Category == 1,"#08519c", Color)) %>%
    mutate(Color = ifelse(Category == 2,"#3182BD", Color)) %>%
    mutate(Color = ifelse(Category == 3,"#6BAED6", Color)) %>%
    mutate(Color = ifelse(Category == 4, "#D3D3D3",Color)) %>%
    mutate(Color = ifelse(is.na(Category), "#D3D3D3",Color))
  
  
  waitress$inc(10) # increase by 10
  #TODO: Determine if this is used
  ProjectCats <- unique(ppl_data$`Project Type`)
  
  ### Variable Declaration ----
  
  SelectedDataReactive <- reactiveValues(df = ppl_data %>% filter(State == "Alabama"))
  SummaryData <- reactiveValues(df = ppl_data %>%
                                  filter(`Funding Status` == "Funded") %>%
                                  filter(State == "Alabama") %>%
                                  mutate(Count = 1) %>%
                                  mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`)) %>%  
                                  mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0"))) %>%
                                  select(`Project Type`,`Funding Amount`,`Principal Forgiveness`,Count,DAC) %>%
                                  group_by(`Project Type`) %>%
                                  summarize_if(is.numeric,sum) %>%
                                  mutate(`Percent DAC` = DAC / Count))
  
  
  waitress$close() 
  
  ## Visual Components ----
  
  ### Map ----
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 4, maxZoom = 4,dragging = FALSE)) %>%
      addPolygons(data = ppl_state_data_geo, 
                  layerId = ~NAME, 
                  label = ~htmlEscape(NAME), 
                  labelOptions = labelOptions(style = list("color" = "black","font-style" = "bold","font-size" = "12px")), 
                  highlight = highlightOptions(weight = 2.5, color = "Black", bringToFront = TRUE),
                  fillOpacity = 1, 
                  fillColor = ~Color, 
                  color = "white", 
                  weight = 1.5) %>%
      addLegend("topleft", 
                colors = c("#08519c", "#3182BD", "#6BAED6", "#D3D3D3"),
                labels = c("Award Data", "No Award Data, Only Project Applicant Data","Partial Award Data","No Data"),
                title = "Category",
                opacity = 1) %>%
      setView(-95.5795, 36.8283, zoom = 4)
    
  })
  
  #### Observe Events ----
  # Mapclick observe
  observeEvent(input$Map_shape_click,{
    if(!is.na(ppl_state_data_geo %>% filter(NAME == input$Map_shape_click$id) %>% pull(`Funding Amount`)))
    {
      SelectedDataReactive$df <- ppl_data %>%
        filter(State == input$Map_shape_click$id)

      
      SummaryData$df <- SelectedDataReactive$df %>%
        mutate(Count = 1) %>%
        mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`)) %>%  
        mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0"))) %>%
        select(`Project Type`,`Funding Amount`,`Principal Forgiveness`,Count,DAC) %>%
        group_by(`Project Type`) %>%
        summarize_if(is.numeric,list(sum), na.rm = TRUE) %>%
        mutate(`Percent DAC` = DAC / Count)
      
    }
    else
    {
      showNotification(id = "notification", state_descriptions %>% filter(State == input$Map_shape_click$id) %>% pull(Description) ,type = "warning")
    }
  })
  
  
  ### Table ----
  
  ##NOTE: renderDataTable needs to be wrapped in a renderUI to work with Shinyjs hidden... not sure why
  output$Table <- renderUI({
    # Missing pop 
    req(SelectedDataReactive$df)
    
    TableData <- SelectedDataReactive$df %>%
      mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`)) %>%
      mutate(Population = as.numeric(Population)) %>%
      
      ##NOTE: this filters down to the right columns and selects the order in which they are seen! 
      select(2,3,4,5,7,6,16,14,15,8,9,10,11,12)
    
    renderReactable({
      with_tooltip <- function(value, tooltip, ...) {
        div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: info; text-color: #2362d0",
            tippy(value, tooltip, ...))
      }
      reactable(TableData,
                defaultSorted = 'Funding Amount',
                defaultSortOrder = 'desc',
                columns = list(
                  `City Served` = colDef(na = "No Information", header = with_tooltip("City Served", column_tooltips[1,2])),
                  `Borrower`= colDef(na = "No Information", header = with_tooltip("Borrower", column_tooltips[2,2])),
                  `PWSID`= colDef(na = "No Information", header = with_tooltip("PWSID", column_tooltips[3,2]),width = 80),
                  `Project Name` = colDef(na = "No Information", header = with_tooltip("Project Name", column_tooltips[4,2]), name = "Name", minWidth = 110),
                  `Project Type` = colDef(na = "No Information", header = with_tooltip("Project Type", column_tooltips[5,2]), name = "Type"),
                  # Add tooltip for fundable
                   `Funding Status` = colDef(name = "Funding Status", minWidth = 80, header = with_tooltip("Funding Status", column_tooltips[12,2])),
                  `Requested Amount` = colDef(na = "No Information",minWidth = 150, header = with_tooltip("Requested Amount", column_tooltips[14,2]), format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  `Project Cost` = colDef(na = "No Information",minWidth = 120, header = with_tooltip("Project Cost", column_tooltips[13,2]), format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  `Funding Amount` = colDef(na = "No Information", header = with_tooltip("Funding Amount", column_tooltips[6,2]),minWidth = 140,format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  `Principal Forgiveness` = colDef(na = "No Information", header = with_tooltip("Principal Forgiveness", column_tooltips[7,2]),minWidth = 160,name = "Forgiveness",format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                   Population = colDef(na = "No Information", header = with_tooltip("Population", column_tooltips[8,2]),format = colFormat(separators = TRUE, digits = 0)),
                  `Project Description` = colDef(na = "No Information", header = with_tooltip("Project Description", column_tooltips[9,2]), html = TRUE, class = "long-col", name = "Description", minWidth = 200, cell = function(value) {
                    div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help; font-size: 14px;",tippy(value, value))
                  }),
                   `Meets State Disadvantaged Criteria`= colDef(na = "No Information", header = with_tooltip("Meets State Disadvantaged Criteria", column_tooltips[10,2]), minWidth = 250),
                    `State Rank`= colDef(na = "No Information", header = with_tooltip("State Rank", column_tooltips[11,2]))
                  ),
                highlight = TRUE,
                bordered = TRUE,
                resizable = TRUE,
                wrap = FALSE,
                minRows = 20,
                defaultPageSize = 50,
                height = shinybrowser::get_height(),
                defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")))
    })
    
    
    
  })
  
  # Context switch observe 
  observeEvent(input$Context, {
    toggle("Map", anim = FALSE,animType = "slide")
    toggle("Table", anim = FALSE,animType = "slide")
    toggle("StateNameTwo", anim = FALSE,animType = "slide")
    toggle("ColumnMouseover", anim = FALSE,animType = "slide")
  })
  
  observeEvent(input$Chart_Summary, {
    toggle("CollapsePanel", anim = FALSE,animType = "slide")
    toggle("ChartOne", anim = FALSE,animType = "slide")
    toggle("ChartSelect", anim = FALSE,animType = "slide")
    toggle("ChartText", anim = FALSE, animType = "slide")
    
  })

  
  
  ### Sidebar ----
  
  #### Description ----
  
  #State Name
  output$StateName <- renderText({SelectedDataReactive$df$State[1]})
  output$StateNameTwo <- renderText({ paste(SelectedDataReactive$df$State[1], "Data Table")})

  output$StateCategory <- renderText({
    Category <- ppl_state_data_geo %>% filter(NAME == SelectedDataReactive$df$State[1]) %>% pull(Category)
    CategoryText <- ifelse(Category == 1, "Award Data", "")
    CategoryText <- ifelse(Category == 2, "No Award Data, Only Project Applicant Data",CategoryText)
    CategoryText <- ifelse(Category == 3, "Partial Award Data",CategoryText)
    return(paste("Category:", CategoryText))
  })
  
  output$ColumnMouseover <- renderText({
    paste("Click a column to sort, mouse-over a column for details, and slide a column to expand")
  })
  
  #State Description 
  output$StateDescription <- renderText({
    state_descriptions %>% filter(State == SelectedDataReactive$df$State[1]) %>% pull(Description)
  })
  
  #### Needs & Funds ---- 
  output$NeedsFunds <- renderUI({
    State_Data <- ppl_state_data_geo %>%
      filter(NAME == SelectedDataReactive$df$State[1])
    
    Population <- paste("<b>", "Water System Users:", scales::number(State_Data %>% pull(Population), big.mark = ","), "</b>", "<br>")
    WaterSystems <- paste("<b>", "Water Systems:", scales::number(State_Data %>% pull(Systems), big.mark = ","), "</b>", "<br>")
    TwentyYearNeed <- paste("<b>", "EPA Estimated 20-Year Need:", scales::dollar(State_Data %>% pull(TwentyYearNeed) / 1000000000, suffix = "B"), "</b>", "<br>")
    PipeCount <- paste("<b>", "EPA Estimated Number of Lead Pipes:", scales::number(State_Data %>% pull(PipeEstimates), big.mark = ","), "</b>", "<br>")
    Base <- paste("<b>", "Base:", dollar(State_Data %>% pull(Base)/1000000, suffix  = "M"), "</b>", "<br>")
    General <- paste("<b> ", "General Supplemental:", dollar(State_Data %>% pull(General)/1000000, suffix  = "M"), "</b>", "<br>")
    Lead <- paste("<b> ", "Lead:", dollar(State_Data %>% pull(Lead)/1000000, suffix  = "M"), "</b>", "<br>")
    Emerging <- paste("<b> ", "Emerging Contaminants:", dollar(State_Data %>% pull(`Emerging Contaminants`)/1000000, suffix  = "M"), "</b>", "<br>")
    
    tagList(
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(tooltip_text[1,2])), options = list("delay': 1000, 'it" = "sucks")), HTML(Population),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[2,2]), options = list("delay': 1000, 'it" = "sucks")),HTML(WaterSystems),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[3,2]), options = list("delay': 1000, 'it" = "sucks")),HTML(TwentyYearNeed),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[4,2]), options = list("delay': 1000, 'it" = "sucks")), HTML(PipeCount),
      HTML("<br>"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[5,2])), HTML("<b> DWSRF Funds </b> </font> <br>"),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(tooltip_text[10,2]))), HTML(Base),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(tooltip_text[11,2]))), HTML(General),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(tooltip_text[12,2]))), HTML(Lead),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(tooltip_text[13,2]))), HTML(Emerging),
    )
  })
  
  #### Projects ----
  
  ##### All ----
  output$AllProjects <- renderUI({
    req(SummaryData$df)
    TotalCount  <- paste("<b>", "Projects:", sum(SummaryData$df$`Count`), "</b>", "<br>")
    # Funding Amount # 
    TotalFunding  <- paste("<b>", "Funding Amount:", dollar(sum(SummaryData$df$`Funding Amount`)/1000000, suffix  = "M"), "</b>", "<br>")
    # Total Forgiveness # 
    TotalForgive  <- paste("<b>", "Forgiveness:", ifelse(sum(as.numeric(SummaryData$df$`Principal Forgiveness`),na.rm = TRUE) != 0,dollar(sum(SummaryData$df$`Principal Forgiveness`, na.rm = TRUE)/1000000, suffix  = "M"), "No Data"), "</b>", "<br>")
    # % to DAC Communities # 
    TotalDAC <- paste("<b>", "Projects in Disadvantaged Areas:", scales::percent(sum(SummaryData$df$DAC) / sum(SummaryData$df$Count), accuracy = 2, suffix = "%"), "</b>", "<br>")
    
    tagList(
      # HTML("<b> All Projects: </b> <br>"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[6,2])), HTML(TotalCount),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[7,2])), HTML(TotalFunding),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[8,2])), HTML(TotalForgive),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(tooltip_text[9,2])), HTML(TotalDAC)
    )
  })
  
  ##### General ----
  output$GeneralProjects <- renderUI({
    req(SummaryData$df)
    
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("General"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`), "</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"), "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]),dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"), "No Data"), "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%"), "</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data", "</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data", "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data", "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", "No Data", "</b>", "<br>")
    }
    
    tagList(
      #   HTML("<b> General Supplemental Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  ##### Lead ----
  output$LeadProjects <- renderUI({
    req(SummaryData$df)
    
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Lead"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`), "</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"), "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]), dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"), "No Data"), "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%") , "</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data", "</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data", "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data", "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:", "No Data", "</b>", "<br>")
    }
    
    tagList(
      #  HTML("<b> Lead Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  ##### EC ----
  output$ECProjects <- renderUI({
    req(SummaryData$df)
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Emerging Contaminants"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`), "</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"), "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]),dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"), "No Data"), "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%") , "</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data", "</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data", "</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data", "</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:", "No Data", "</b>", "<br>")
    }
    
    tagList(
      #  HTML("<b> Emerging Contaminant Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  #### Charts ----
  
  ##### Chart Select ----
  output$ChartSelect <- renderUI({
    req(SelectedDataReactive$df)
    
    ## This selects charting options based on data availablity - set to 75%
    ChartOptions <- SelectedDataReactive$df %>%
                    select("Population","Project Cost", "Meets State Disadvantaged Criteria") %>%
                    select(where(function(x) sum(is.na(x)) / length(x) < 0.75)) %>%
                    colnames()
  
    selectInput("ChartSelect", "Select a Variable: ", c("Count", ChartOptions))
  })
  

  output$ChartText <- renderText({
    req(input$ChartSelect)
    tooltip_text <- tooltip_text %>%
           filter(Name == as.character(input$ChartSelect)) %>%
           pull(Text)
    
  })
  
  ##### Chart Output ----
  output$ChartOne <- renderPlot({
  req(SelectedDataReactive$df)
  req(input$ChartSelect)
  
  ##NOTE: that sumarization is needed and the correct variables need to align with options in ChartSelect - down to the correct name/spelling!
  Counts <- SelectedDataReactive$df %>%
            group_by(`Funding Status`, `Project Type`) %>%
            tally()
    
  ##TODO: Review what may or may not need to be removed here
   ChartData <- SelectedDataReactive$df %>%
               group_by(`Funding Status`, `Project Type`) %>%
               select(c("Project Cost","Population", "Meets State Disadvantaged Criteria")) %>%
               mutate(`Meets State Disadvantaged Criteria` = ifelse(`Meets State Disadvantaged Criteria` == "Yes", 1, 0)) %>%
               summarise_if(is.numeric, sum ,na.rm = TRUE) %>%
               ## remove this 
               left_join(.,Counts) %>%
               rename(Count = n) %>%
               mutate(`Project Type` = str_replace(`Project Type`,"Emerging Contaminants", "Emerg Contam"))
            #   mutate(`Meets State Disadvantaged Criteria` = `Meets State Disadvantaged Criteria`/Count * 100)
            

    ggplot(data = ChartData, aes(x = `Project Type`, y = !!sym(input$ChartSelect), fill = `Funding Status`))+
      geom_bar(stat="identity", position = "dodge")+
      scale_fill_manual(values=c("Not Funded" = "#326138", "Funded" ="#f45d00"), name = "")+
      ylab(as.character(input$ChartSelect))+
      xlab("")+
      theme_classic()+
      theme(axis.text = element_text(size = 14, color = "black", face = "bold"))+
      theme(axis.title = element_text(size = 14, color = "black", face = "bold"))+
      theme(legend.text = element_text(size = 14, color = "black", face = "bold"))+
 #   theme(plot.margin = margin(0,0,0,0, "cm"))+
      theme(plot.background = element_rect(fill = "#f5f5f5"))+
      theme(panel.background = element_rect(fill = "#f5f5f5"))+
      theme(legend.background = element_rect(fill = "#f5f5f5"))
     # theme(panel.border = element_rect(fill = "#f5f5f5"))
  })
  
  ### Pop-Up Window ----
  
  InfoModal <- modalDialog(
    title = HTML("<b> EPIC’s Drinking Water Funding Dashboard v1.1 </b>"),
    HTML("<b> About this dashboard: </b>"),
    HTML("<br>"),
    HTML("This dashboard attempts to track Drinking Water State Revolving Funds (DWSRFs) from Federal Fiscal Year (FFY) 2022 appropriations, based on data presented in Project Priority Lists (PPLs) published by states for State Fiscal Year (SFY) 2023. 
         For a full understanding of each state’s PPL, we recommend reviewing the Intended Use Plan (IUP) for that state. Also, some states may take up to September 2024 before their FFY2022 lead service line replacement (LSLR) and emerging contaminant (EC) funds are designated to projects."),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> Data Limitations and Disclaimers: </b>"),
    HTML("<br>"),
    HTML("The data in this dashboard draws from states’ PPLs. Due to a large variation of formats across different states, EPIC used discretion in interpreting the data in order to standardize and make it accessible in one dashboard. The sources of state data and how we interpreted it are outlined in state-specific data dictionaries, including explanations where we have noticed discrepancies or uncertainties and some relevant notes from the states’ IUPs. 
         Because of differences across states, state-to-state comparisons are difficult and likely will be inaccurate. The data used for this dashboard is only what is available publicly and particular to the DWSRF and does not include all data maintained by the states and EPA. State data in the dashboard can be categorized in the following ways: 1) States with award data; 2) States with no award data, only project applicant data; 3) States with partial award data; 4) 
         States with no data, which may indicate that the PPLs are not finalized, not released, or are challenging to interpret and transform into a standardized table. To access the raw data, please visit each state’s appropriate website to download the IUPs and PPLs. To the extent possible, EPIC will continue to update and make corrections to the dashboard with additional data as it becomes available."),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> How to use this dashboard: </b>"),
    HTML("<li> Centered, you’ll find a national map of where EPIC has data for DWSRF PPLs.  </li>"),
    HTML("<li> Click a state to activate the sidebar and explore the funding and project category summary.  </li>"),
    HTML("<li> Click ‘National Map or State Data Table’ to switch between the map and the selected states available PPL data. </li> "),
    HTML("<li> Mousing over the ‘i’ icons will reveal additional information about the metrics. </li>"),
    HTML("<li> Click ‘Download State Data’ to download the state specific data, accompanying data dictionary, and "),
    tags$a(href=paste(glossary_link), "glossary of key terms.",  target="_blank"),
    easyClose = FALSE,
    footer = modalButton("Close"),
  )
  
  observeEvent(input$showInfo, ignoreNULL = FALSE,
               {
                 showModal(InfoModal)
               })

  ## Downloader ----

  output$DataDownload <- downloadHandler(
    
    filename = function() {
      paste0(tolower(SelectedDataReactive$df$State[1]),"-dw-funding.zip", sep="")
    },
    content = function(file) {
      
      state_name <- tolower(SelectedDataReactive$df$State[1])
      
      state_link <- data_dictionary_index %>%
                    filter(State == SelectedDataReactive$df$State[1]) %>%
                    pull(Link)

      drive_deauth()
      
      ## TO DO - Add function for pulling in correct state data 
      dictionary_pdf <- drive_download(state_link, file.path(tempdir(), paste0(state_name, "-data-dictionary.pdf", sep="")), overwrite = TRUE)
      
      ## Adding glossary data
      glossary_pdf <- drive_download(glossary_link, 
                               file.path(tempdir(), paste0(state_name, "-data-glossary.pdf", sep="")), overwrite = TRUE)
      
      ## Adding state data
      write.csv(SelectedDataReactive$df, file.path(tempdir(), paste0(state_name, "-dw-funding-data.csv", sep="")), row.names=FALSE)

      zip::zip(file, files = c(file.path(tempdir() ,paste0(state_name, "-dw-funding-data.csv", sep="")),
                               file.path(tempdir() ,paste0(state_name, "-data-dictionary.pdf", sep="")),
                               file.path(tempdir() ,paste0(state_name, "-data-glossary.pdf", sep=""))),
               mode = "cherry-pick")
    })


  
}

shinyApp(ui = ui, server = server)