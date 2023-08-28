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
library(pdftools)
## Major Components
## Data prep 
## Map 
## Table 
## Sidebar
## Open Modal 



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
           actionButton("Context", "Toggle National Map or State Data Table",icon(name = "arrows-up-down", lib = "font-awesome"),style = "margin-bottom: 10px; color: #a82b4d; border-color: #a82b4d;"), 
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



server <- function(input, output,session) {
  
  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
  
  
  waitress$inc(20) # increase by 10
  DW_Data_Raw <-  read_csv(get_object(object = "apps/dw-dashboard/dw-dashboard-data.csv", bucket = "water-team-data"))

  PPL_Data <- read_csv(get_object(object = "clean_data/srf_project_priority_lists/web_ppl_combined_clean_v3.csv", bucket = "water-team-data"))%>%
    mutate(across('Project Type', str_replace, 'Other', 'General'))


  waitress$inc(20) # increase by 10
  
  
  ## Importing Geo Data 
  Geo_Data <- geojson_sf("www/states_ak_hi_v3.geojson")

  waitress$inc(20) # increase by 10
  
  ## Importing Google sheets data (Tool tips)
  gs4_deauth()
  URL <- "https://docs.google.com/spreadsheets/d/1hznoLfB8zMzs3jKfLjs-uy9R68mcFsYMAUg1gKXC17Y/edit#gid=0"
  Text <- read_sheet(URL, sheet = "Text")
  
  waitress$inc(20) # increase by 10
  ToolTip <- read_sheet(URL, sheet = "Tooltip")
  
  waitress$inc(10) # increase by 10
  ColumnToolTip <- read_sheet(URL, sheet = "ColumnToolTip")
  
  DataDictionaryIndex <- read_sheet(URL, sheet = "DataDictionaryIndex")
  ## Summarizing by state
  ## Adding color based on number of projects - This is subject to change/flexible ## 
  ## Adding Sabs Data 

  PPL_State_Data_Geo <- DW_Data_Raw %>%
    left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
    mutate(Color = "")%>%
    mutate(Color = ifelse(Category == 1,"#08519c", Color))%>%
    mutate(Color = ifelse(Category == 2,"#3182BD", Color))%>%
    mutate(Color = ifelse(Category == 3,"#6BAED6", Color))%>%
    mutate(Color = ifelse(Category == 4, "#D3D3D3",Color))%>%
    mutate(Color = ifelse(is.na(Category), "#D3D3D3",Color))
  
  
  waitress$inc(10) # increase by 10
  ProjectCats <- unique(PPL_Data$`Project Type`)
  
  ## Var Decleration ## 
  SelectedDataReactive <- reactiveValues(df = PPL_Data %>% filter(State == "Alabama"))
  SummaryData <- reactiveValues(df = PPL_Data %>%
                                  filter(State == "Alabama")%>%
                                  mutate(Count = 1)%>%
                                  mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%  
                                  mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
                                  select(`Project Type`,`Funding Amount`,`Principal Forgiveness`,Count,DAC)%>%
                                  group_by(`Project Type`)%>%
                                  summarize_if(is.numeric,sum)%>%
                                  mutate(`Percent DAC` = DAC / Count))
  
  ### END DATA PREPREATION ### 
  
  waitress$close() 
  ### VISUAL COMPONENTS ### 
  
  ##### MAP ##### 
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 4, maxZoom = 4,dragging = FALSE))%>%
      addPolygons(data = PPL_State_Data_Geo, 
                  layerId = ~NAME, 
                  label = ~htmlEscape(NAME), 
                  labelOptions = labelOptions(style = list("color" = "black","font-style" = "bold","font-size" = "12px")), 
                  highlight = highlightOptions(weight = 2.5, color = "Black", bringToFront = TRUE),
                  fillOpacity = 1, 
                  fillColor = ~Color, 
                  color = "white", 
                  weight = 1.5)%>%
      addLegend("topleft", 
                colors = c("#08519c", "#3182BD", "#6BAED6", "#D3D3D3"),
                labels = c("Award Data", "No Award Data, Only Project Applicant Data","Partial Award Data","No Data"),
                title = "Category",
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

      
      SummaryData$df <- SelectedDataReactive$df %>%
        mutate(Count = 1)%>%
        mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%  
        mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
        select(`Project Type`,`Funding Amount`,`Principal Forgiveness`,Count,DAC)%>%
        group_by(`Project Type`)%>%
        summarize_if(is.numeric,list(sum), na.rm = TRUE)%>%
        mutate(`Percent DAC` = DAC / Count)
      
    }
    else
    {
      showNotification(id = "notification", Text %>% filter(State == input$Map_shape_click$id) %>% pull(Description) ,type = "warning")
    }
  })
  
  
  #### End Map ##### 
  #### Table #### 
  ## Note that renderDataTable needs to be wrapped in a renderUI to work with Shinyjs hidden... not sure why ## 
  output$Table <- renderUI({
    # Missing pop 
    req(SelectedDataReactive$df)
    
    TableData <- SelectedDataReactive$df %>%
      mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%
      mutate(Population = as.numeric(Population))%>%
      select(2,3,4,5,7,8,9,10,6,11,12)
 
    renderReactable({
      with_tooltip <- function(value, tooltip, ...) {
        div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: info; text-color: #2362d0",
            tippy(value, tooltip, ...))
      }
      reactable(TableData,
                defaultSorted = 'Funding Amount',
                defaultSortOrder = 'desc',
                columns = list(
                  `City Served` = colDef(na = "No Information",header = with_tooltip("City Served",ColumnToolTip[1,2])),
                  `Borrower`= colDef(na = "No Information",header = with_tooltip("Borrower",ColumnToolTip[2,2])),
                  `PWSID`= colDef(na = "No Information",header = with_tooltip("PWSID",ColumnToolTip[3,2]),width = 80),
                  `Project Name` = colDef(na = "No Information",header = with_tooltip("Project Name",ColumnToolTip[4,2]), name = "Name", minWidth = 110),
                  `Project Type` = colDef(na = "No Information",header = with_tooltip("Project Type",ColumnToolTip[5,2]), name = "Type"),
                 `Funding Amount` = colDef(na = "No Information",header = with_tooltip("Funding Amount",ColumnToolTip[6,2]),minWidth = 140,format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  `Principal Forgiveness` = colDef(na = "No Information", header = with_tooltip("Principal Forgiveness",ColumnToolTip[7,2]),minWidth = 160,name = "Forgiveness",format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  
                   Population = colDef(na = "No Information",header = with_tooltip("Population",ColumnToolTip[8,2]),format = colFormat(separators = TRUE, digits = 0)),
            
                  `Project Description` = colDef(na = "No Information",header = with_tooltip("Project Description",ColumnToolTip[9,2]), html = TRUE, class = "long-col", name = "Description", minWidth = 200, cell = function(value) {
                    div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help; font-size: 14px;",tippy(value, value))
                  }),
                   `Meets State Disadvantaged Criteria`= colDef(na = "No Information",header = with_tooltip("Meets State Disadvantaged Criteria",ColumnToolTip[10,2]), minWidth = 250),
                    `State Rank`= colDef(na = "No Information",header = with_tooltip("State Rank",ColumnToolTip[11,2]))
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
  #### End Table 
  
  
  #### Sidebar #### 
  #State Name
  output$StateName <- renderText({SelectedDataReactive$df$State[1]})
  output$StateNameTwo <- renderText({ paste(SelectedDataReactive$df$State[1],"Data Table")})

  output$StateCategory <- renderText({
    Category <- PPL_State_Data_Geo %>% filter(NAME == SelectedDataReactive$df$State[1]) %>% pull(Category)
    CategoryText <- ifelse(Category == 1,"Award Data","")
    CategoryText <- ifelse(Category == 2,"No Award Data, Only Project Applicant Data",CategoryText)
    CategoryText <- ifelse(Category == 3,"Partial Award Data",CategoryText)
    return(paste("Category:", CategoryText))
  })
  
  output$ColumnMouseover <- renderText({
    paste("Click a column to sort, mouse-over a column for details, and slide a column to expand")
  })
  
  #State Description 
  output$StateDescription <- renderText({
    Text %>% filter(State == SelectedDataReactive$df$State[1]) %>% pull(Description)
  })
  
  ## Needs and Funds Section ## 
  output$NeedsFunds <- renderUI({
    State_Data <- PPL_State_Data_Geo %>%
      filter(NAME == SelectedDataReactive$df$State[1])
    
    Population <- paste("<b>", "Water System Users:", scales::number(State_Data %>% pull(Population), big.mark = ","),"</b>", "<br>")
    WaterSystems <- paste("<b>", "Water Systems:", scales::number(State_Data %>% pull(Systems), big.mark = ","),"</b>", "<br>")
    TwentyYearNeed <- paste("<b>", "EPA Estimated 20-Year Need:", scales::dollar(State_Data %>% pull(TwentyYearNeed) / 1000000000, suffix = "B"),"</b>", "<br>")
    PipeCount <- paste("<b>", "EPA Estimated Number of Lead Pipes:", scales::number(State_Data %>% pull(PipeEstimates), big.mark = ","),"</b>", "<br>")
    Base <- paste("<b>", "Base:", dollar(State_Data %>% pull(Base)/1000000, suffix  = "M"),"</b>", "<br>")
    General <- paste("<b> ", "General Supplemental:", dollar(State_Data %>% pull(General)/1000000, suffix  = "M"),"</b>", "<br>")
    Lead <- paste("<b> ", "Lead:", dollar(State_Data %>% pull(Lead)/1000000, suffix  = "M"),"</b>", "<br>")
    Emerging <- paste("<b> ", "Emerging Contaminants:", dollar(State_Data %>% pull(`Emerging Contaminants`)/1000000, suffix  = "M"),"</b>", "<br>")
    
    tagList(
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(ToolTip[1,2])), options = list("delay': 1000, 'it" = "sucks")), HTML(Population),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[2,2]), options = list("delay': 1000, 'it" = "sucks")),HTML(WaterSystems),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[3,2]), options = list("delay': 1000, 'it" = "sucks")),HTML(TwentyYearNeed),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[4,2]), options = list("delay': 1000, 'it" = "sucks")), HTML(PipeCount),
      HTML("<br>"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[5,2])), HTML("<b> DWSRF Funds </b> </font> <br>"),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(ToolTip[10,2]))), HTML(Base),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(ToolTip[11,2]))),HTML(General),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(ToolTip[12,2]))),HTML(Lead),
      HTML("&ensp;"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = HTML(as.character(ToolTip[13,2]))),HTML(Emerging),
    )
  })
  
  ### All Projects 
  output$AllProjects <- renderUI({
    req(SummaryData$df)
    TotalCount  <- paste("<b>", "Projects:", sum(SummaryData$df$`Count`),"</b>", "<br>")
    # Funding Amount # 
    TotalFunding  <- paste("<b>", "Funding Amount:", dollar(sum(SummaryData$df$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
    # Total Forgiveness # 
    TotalForgive  <- paste("<b>", "Forgiveness:", ifelse(sum(as.numeric(SummaryData$df$`Principal Forgiveness`),na.rm = TRUE) != 0,dollar(sum(SummaryData$df$`Principal Forgiveness`, na.rm = TRUE)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
    # % to DAC Communities # 
    TotalDAC <- paste("<b>", "Projects in Disadvantaged Areas:", scales::percent(sum(SummaryData$df$DAC) / sum(SummaryData$df$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
    
    tagList(
      # HTML("<b> All Projects: </b> <br>"),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[6,2])),HTML(TotalCount),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[7,2])),HTML(TotalFunding),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[8,2])),HTML(TotalForgive),
      tipify(el = icon(name = "circle-info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[9,2])),HTML(TotalDAC)
    )
  })
  
  ## General Projects 
  output$GeneralProjects <- renderUI({
    req(SummaryData$df)
    
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("General"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]),dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data","</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data","</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data","</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:","No Data","</b>", "<br>")
    }
    
    tagList(
      #   HTML("<b> General Supplemental Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  output$LeadProjects <- renderUI({
    req(SummaryData$df)
    
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Lead"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]),dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data","</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data","</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data","</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:","No Data","</b>", "<br>")
    }
    
    tagList(
      #  HTML("<b> Lead Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  output$ECProjects <- renderUI({
    req(SummaryData$df)
    
    CategoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Emerging Contaminants"))
    
    if(nrow(CategoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CategoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CategoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CategoryData$`Principal Forgiveness`[1]),dollar(sum(CategoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:", scales::percent(sum(CategoryData$DAC) / sum(CategoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
    }
    else
    {
      Count  <- paste("<b> ", "Projects:", "No Data","</b>", "<br>")
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", "No Data","</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", "No Data","</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:","No Data","</b>", "<br>")
    }
    
    tagList(
      #  HTML("<b> Emerging Contaminant Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  InfoModal <- modalDialog(
    title = HTML("<b> EPIC’s Drinking Water Funding Dashboard </b>"),
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
    HTML("<li> Click ‘Toggle National Map or State Data Table’ to switch between the map and the selected states available PPL data. </li> "),
    HTML("<li> Mousing over the ‘i’ icons will reveal additional information about the metrics. </li>"),
    HTML("<li> Click ‘Download State Data’ to download the state specific data, accompanying data dictionary, and "),
    tags$a(href="https://drive.google.com/file/d/1Qils_r_X8pyMe3F9qJ77bqzsnG_6PQqb/view", "glossary of key terms.",  target="_blank"),
    easyClose = FALSE,
    footer = modalButton("Close"),
  )
  
  observeEvent(input$showInfo, ignoreNULL = FALSE,
               {
                 showModal(InfoModal)
               })

  # Download Handler ##
 # download handler for project export
  output$DataDownload <- downloadHandler(
    
    filename = function() {
      paste0(tolower(SelectedDataReactive$df$State[1]),"-dw-funding.zip", sep="")
    },
    content = function(file) {
      
      state_name <- tolower(SelectedDataReactive$df$State[1])
      
      state_link <- DataDictionaryIndex %>%
                    filter(State == SelectedDataReactive$df$State[1])%>%
                    pull(Link)

      drive_deauth()
      
      ## TO DO - Add function for pulling in correct state data 
      dictionary_pdf <- drive_download(state_link, file.path(tempdir(),paste0(state_name,"-data-dictionary.pdf", sep="")), overwrite = TRUE)
      
      ## Adding glossary data
      glossary_pdf <- drive_download("https://drive.google.com/file/d/1Qils_r_X8pyMe3F9qJ77bqzsnG_6PQqb/view?usp=share_link", 
                               file.path(tempdir(),paste0(state_name,"-data-glossary.pdf", sep="")), overwrite = TRUE)
      
      ## Adding state data
      write.csv(SelectedDataReactive$df, file.path(tempdir(), paste0(state_name,"-dw-funding-data.csv", sep="")), row.names=FALSE)

      zip::zip(file, files = c(file.path(tempdir(),paste0(state_name,"-dw-funding-data.csv", sep="")),
                               file.path(tempdir(),paste0(state_name,"-data-dictionary.pdf", sep="")),
                               file.path(tempdir(),paste0(state_name,"-data-glossary.pdf", sep=""))),
               mode = "cherry-pick")
    })


  
}

shinyApp(ui = ui, server = server)