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
    tags$style(HTML(".leaflet-container { background: #FFFFFF;} .sidebar form.well { background: transparent;border: 0px;} .panel-primary>.panel-heading+.panel-collapse>.panel-body{border-right: 1px solid rgba(0, 0, 0, 0.05);}"))
  ),
  sidebarLayout(
    div( id ="sidebar",
         sidebarPanel(
           style = "position: fixed; height: 100%; overflow-y: auto; margin-left: -30px; width: 400px; overflow-x: auto;", div(style = "display:inline-block; float:right; margin-bottom: 20px"),
           uiOutput("StateName", style = "font-size: 40px; margin-top: -20px;"), 
           uiOutput("StateDescription", style = "height: 165px; max-width: 400px; overflow-y: scroll; margin-right: -15px;  margin-bottom: 10px;"), 
           bsCollapse(id = "CollapsePanel", open = c("Drinking Water Needs and Funding", "All Projects"), multiple = TRUE,
                      bsCollapsePanel("Drinking Water Needs and Funding",uiOutput("NeedsFunds", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "primary"),
                      bsCollapsePanel("All Projects",uiOutput("AllProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "info"),
                      bsCollapsePanel("General Projects",uiOutput("GeneralProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "warning"),
                      bsCollapsePanel("Lead Projects",uiOutput("LeadProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "danger"),
                      bsCollapsePanel("Emerging Contaminant Projects",uiOutput("ECProjects", style = "line-height: 20px; margin-top: -10px; margin-bottom: -10px;"),  style = "success")),
           #    uiOutput("StateNumbers", style = "line-height: 20px"),
           #    radioButtons("Chart_Select", "", choices = c("Percent of the population covered" = "Pop","Percent of water systems covered" = "Sys")),
           #    div(plotOutput("Chart"), style = "margin-left:-15px"),
           actionButton("Context", "Table/Map",icon(name = "arrows-up-down", lib = "font-awesome"),style = "display:inline-block; float:left;"), 
           actionButton("showInfo", " Info", icon(name = "question", lib = "font-awesome"), style = "display:inline-block; float:left;"), 
           downloadButton('DataDownload','Download Data'),
           img(src = 'epic-logo-transparent.png', height = '40px', width = '150px', style = "display:inline-block; float:center; margin-left: 5px; margin-top:10px"),
           width = 3
         )),
    
    mainPanel(
      leafletOutput("Map", height = "100vh"),
      hidden(uiOutput("StateNameTwo", style = "font-size: 30px; margin-left: 5px; position:relative; z-index: 500;"), uiOutput("Table", style = "margin-left: 5px; background-color: none;", width = "100px")),
      style = "margin-left: -15px;",
      width = 9),
    
    position = c("right"), fluid = TRUE),
  
)


server <- function(input, output,session) {
  
  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
  
  
  waitress$inc(20) # increase by 10
  DW_Data_Raw <-  read_csv(get_object(object = "apps/dw-dashboard/dw-dashboard-data.csv", bucket = "water-team-data"))
  
  
  waitress$inc(20) # increase by 10
  
  
  ## Importing Geo Daeta 
  Geo_Data <- geojson_sf("www/states_ak_hi_v3.geojson")
  waitress$inc(20) # increase by 10
  
  ## Importing Google sheets data (Tool tips)
  gs4_deauth()
  URL <- "https://docs.google.com/spreadsheets/d/1hznoLfB8zMzs3jKfLjs-uy9R68mcFsYMAUg1gKXC17Y/edit#gid=0"
  Text <- read_sheet(URL, sheet = "Text")
  waitress$inc(20) # increase by 10
  ToolTip <- read_sheet(URL, sheet = "Tooltip")
  waitress$inc(20) # increase by 10

  ## Summarizing by state
  ## Adding color based on number of projects - This is subject to change/flexible ## 
  ## Adding Sabs Data 
  PPL_State_Data_Geo <- DW_Data_Raw %>%
    left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
    mutate(Color = ifelse(FundPer100k > 0,"#BDD7E7", Color))%>%
    mutate(Color = ifelse(FundPer100k > 25 ,"#6BAED6", Color))%>%
    mutate(Color = ifelse(FundPer100k > 50,"#3182BD", Color))%>%
    mutate(Color = ifelse(FundPer100k > 100,"#08519c", Color))%>%
    mutate(Color = ifelse(FundPer100k > 200,"#02006c", Color))%>%
    mutate(Color = ifelse(is.na(FundPer100k), "#D3D3D3",Color))
  
  
  
  
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
    leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 4.5, maxZoom = 4.5,dragging = FALSE))%>%
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
                colors = c("#D3D3D3","#BDD7E7", "#6BAED6", "#3182BD", "#08519c", "#02006c"),
                labels = c("No project data", "$1-$25", "$25-$50","$50-$100","$100-$200", "$200+"),
                title = "Funding Per Water User",
                opacity = 1)%>%
      setView(-95.5795, 36.8283, zoom = 5)
    
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
        summarize_if(is.numeric,sum)%>%
        mutate(`Percent DAC` = DAC / Count)
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
    # Missing pop 
    req(SelectedDataReactive$df)
    
    TableData <- SelectedDataReactive$df %>%
      mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%
      mutate(Population = as.numeric(Population))%>%
      select(2,3,4,5,7,8,9,10,6,11,12)
    
    # Funding = c('#e5f5e0', '#a1d99b', '#31a354', "#557153")
    # Population = c("#ffb448", "#ffc784","#ffdc97","#fff4bd")
    # Principal = c("#F45D01","#333C6B","#97A7B3")
    # 

 
    renderReactable({
      reactable(TableData,
                defaultSorted = 'Funding Amount',
                defaultSortOrder = 'desc',
                columns = list(
                  `Principal Forgiveness` = colDef(name = "Forgiveness",format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                  `Funding Amount` = colDef(minWidth = 100,format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
                   Population = colDef(format = colFormat(separators = TRUE, digits = 0)),
                  `Project Type` = colDef(name = "Type"),
                  `Project Name` = colDef(name = "Name", minWidth = 100),
                  `Project Description` = colDef(html = TRUE, class = "long-col", name = "Description", minWidth = 200, cell = function(value) {
                    div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help font-size: 10;",
                        tippy(value, value))
                  })),
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
    toggle("Map", anim = FALSE,animType = "slide",time = 0.5)
    toggle("Table", anim = FALSE,animType = "slide",time = 0.5)
    toggle("StateNameTwo", anim = FALSE,animType = "slide",time = 0.5)
  })
  #### End Table 
  
  
  #### Sidebar #### 
  #State Name
  output$StateName <- renderText({SelectedDataReactive$df$State[1]})
  output$StateNameTwo <- renderText({paste(SelectedDataReactive$df$State[1],"Fundable Project List")})
  
  #State Description 
  output$StateDescription <- renderText({
    Text %>% filter(State == SelectedDataReactive$df$State[1]) %>% pull(Description)
  })
  
  ## Needs and Funds Section ## 
  output$NeedsFunds <- renderUI({
    State_Data <- PPL_State_Data_Geo %>%
      filter(NAME == SelectedDataReactive$df$State[1])
    
    Population <- paste("<b>", "Population:", scales::number(State_Data %>% pull(Population), big.mark = ","),"</b>", "<br>")
    WaterSystems <- paste("<b>", "Water Systems:", scales::number(State_Data %>% pull(Systems), big.mark = ","),"</b>", "<br>")
    TwentyYearNeed <- paste("<b>", "Estimated 20 Year Need:", scales::dollar(State_Data %>% pull(TwentyYearNeed) / 1000000000, suffix = "B"),"</b>", "<br>")
    PipeCount <- paste("<b>", "Estimated Number of Lead Pipes", scales::number(State_Data %>% pull(PipeEstimates), big.mark = ","),"</b>", "<br>")
    Base <- paste("<b> &ensp;", "Base:", dollar(State_Data %>% pull(Base)/1000000, suffix  = "M"),"</b>", "<br>")
    General <- paste("<b> &ensp;", "General:", dollar(State_Data %>% pull(General)/1000000, suffix  = "M"),"</b>", "<br>")
    Lead <- paste("<b> &ensp;", "Lead", dollar(State_Data %>% pull(Lead)/1000000, suffix  = "M"),"</b>", "<br>")
    Emerging <- paste("<b> &ensp;", "Emerging Contaminants", dollar(State_Data %>% pull(`Emerging Contaminants`)/1000000, suffix  = "M"),"</b>", "<br>")
    
    tagList(
      #HTML("<b> <font size=4> Need </b> </font> <br>"),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[1,2])), HTML(Population),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[2,2])),HTML(WaterSystems),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[3,2])),HTML(TwentyYearNeed),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[4,2])),HTML(PipeCount),
      HTML("<br>"),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[5,2])), HTML("<b> DWSRF Funds </b> </font> <br>"),
      HTML(Base),
      HTML(General),
      HTML(Lead),
      HTML(Emerging),
    )
  })
  
  ### All Projects 
  output$AllProjects <- renderUI({
    req(SummaryData$df)
    TotalCount  <- paste("<b>", "Projects:", sum(SummaryData$df$`Count`),"</b>", "<br>")
    # Funding Amount # 
    TotalFunding  <- paste("<b>", "Funding Amount:", dollar(sum(SummaryData$df$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
    # Total Forgiveness # 
    TotalForgive  <- paste("<b>", "Forgiveness:", ifelse(!is.na(SummaryData$df$`Principal Forgiveness`[1]),dollar(sum(SummaryData$df$`Principal Forgiveness`, na.rm = TRUE)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
    # % to DAC Communities # 
    TotalDAC <- paste("<b>", "Projects in Disavantaged Areas:", scales::percent(sum(SummaryData$df$DAC) / sum(SummaryData$df$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
    
    tagList(
      # HTML("<b> All Projects: </b> <br>"),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[6,2])),HTML(TotalCount),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[7,2])),HTML(TotalFunding),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[8,2])),HTML(TotalForgive),
      tipify(el = icon(name = "info", lib = "font-awesome"), placement = "right", title = as.character(ToolTip[9,2])),HTML(TotalDAC)
    )
  })
  
  ## General Projects 
  output$GeneralProjects <- renderUI({
    req(SummaryData$df)
    
    CatagoryData <- SummaryData$df %>%
      filter(`Project Type` == ("General"))
    
    if(nrow(CatagoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CatagoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CatagoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CatagoryData$`Principal Forgiveness`[1]),dollar(sum(CatagoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CatagoryData$DAC) / sum(CatagoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
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
      #   HTML("<b> General Projects: </b> <br>"),
      HTML(Count),
      HTML(Funding),
      HTML(Forgive),
      HTML(DAC)
    )
  })
  
  output$LeadProjects <- renderUI({
    req(SummaryData$df)
    
    CatagoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Lead"))
    
    if(nrow(CatagoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CatagoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CatagoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CatagoryData$`Principal Forgiveness`[1]),dollar(sum(CatagoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Projects in Disavantaged Areas:", scales::percent(sum(CatagoryData$DAC) / sum(CatagoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
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
    
    CatagoryData <- SummaryData$df %>%
      filter(`Project Type` == ("Emerging Contaminants"))
    
    if(nrow(CatagoryData) > 0)
    {
      Count  <- paste("<b> ", "Projects:", sum(CatagoryData$`Count`),"</b>", "<br>")
      #
      # Funding Amount # 
      Funding  <- paste("<b> ", "Funding Amount:", dollar(sum(CatagoryData$`Funding Amount`)/1000000, suffix  = "M"),"</b>", "<br>")
      # Total Forgiveness # 
      Forgive  <- paste("<b> ", "Forgiveness:", ifelse(!is.na(CatagoryData$`Principal Forgiveness`[1]),dollar(sum(CatagoryData$`Principal Forgiveness`)/1000000, suffix  = "M"),"No Data"),"</b>", "<br>")
      # % to DAC Communities # 
      DAC <- paste("<b> ", "Percent of Projects in Disavantaged Areas:", scales::percent(sum(CatagoryData$DAC) / sum(CatagoryData$Count), accuracy = 2, suffix = "%") ,"</b>", "<br>")
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
    HTML("<b> Tracking Lead Service Line Replacement: </b>"),
    HTML("<br>"),
    HTML("There are still an estimated 6 to 10 million lead service lines in cities and towns across the country, 
       many of which are in low-income neighborhoods and communities of color. The Infrastructure Investment and Jobs Act (IIJA), 
       or the Bipartisan Infrastructure Law, includes unprecedented funding to improve water infrastructure, including $15 billion over 5 years  (2021-2026) 
       to identify and replace toxic lead pipes carrying drinking water. The main mechanism through which this funding is distributed is the 
       Drinking Water State Revolving Fund (DWSRF), which is a program where the US Environmental Protection Agency (EPA) allots money to 
       each state to then distributes to utilities and municipalities with eligible water infrastructure projects. 
       IIJA requires that 49 percent of all DWSRFs be distributed as Principal Forgiveness or forgivable loans to state-defined disadvantaged communities."),
    HTML("<br>"),
    HTML("<br>"),
    HTML("<b> About the Dashboard: </b>"),
    HTML("<br>"),
    HTML("This dashboard aims to help users identify how much money through the Drinking Water State Revolving Fund that 
       will be used for lead service line replacements according to how each state intends to use of their DWSRF allotments 
       from the US Environmental Protection Agency (EPA). This information is detailed in each state’s intended use plan (IUP) 
       and the list of water infrastructure projects listed in priority order - the project priority list (PPL). 
       The user can view how many of these dollars went towards lead pipe identification and replacement and how much of that benefits state-defined disadvantaged 
       communities (DACs). Currently, the database provides information on 20 states and will be updated as more data is available. 
       This is an ongoing project that EPIC will keep building on through the years of IIJA funding."),
    easyClose = FALSE,
    footer = modalButton("Close"),
  )
  
  observeEvent(input$showInfo, ignoreNULL = FALSE,
               {
                 showModal(InfoModal)
               })
  
  
  
  ## Download Handler ## 
  #download handler for project export 
  output$DataDownload <- downloadHandler(
    
    filename = function() { 
      paste0(SelectedDataReactive$df$State[1],"-dw-funding-data.csv", sep="")
    },
    content = function(file) {
      write.csv(SelectedDataReactive$df, file, row.names=FALSE)
    })
  
  # 
  # output$Chart <- renderPlot({
  #   req(SummaryData$df)
  #   
  #     print(SummaryData$df)
  #     ggplot(data = SummaryData$df, aes(x = "", y = Count, fill = `Project Type`))+
  #       geom_bar(stat = "identity", color = "black", alpha = .8, width = 1)+
  #        coord_polar("y", start=0)+
  #       scale_fill_manual(values=c("Tier 1" = "#326138", "Tier 2" ="#f45d00", "Tier 3" ="#b6174b"))+
  #       geom_text(aes(label = Count, vjust = -1.5, colour = "black", size = 4))+
  #       ylab("")+
  #       #scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,107), breaks = c(0,25,50,75,100))+
  #       xlab("")+
  #       theme_classic()+
  #       theme(legend.position='none')+
  #       theme(axis.text = element_text(size = 14, color = "black"))+
  #       theme(plot.margin = margin(0,0,0,0, "cm"))+
  #       theme(plot.background = element_rect(fill = "#f5f5f5"))+
  #       theme(panel.border = element_blank())
  # 
  #   
  # })
  
  
  
}

shinyApp(ui = ui, server = server)