
### Export Files ----


run_save_plots <- function(gg_plot, gp_object, name) {
  ## Stores a ggplot object as a png and a ggplotly object as HTML
  ## Pushes both to AWS and the link to the HTML in Google Sheets
  ## Takes both data viz objects, the file's name, directories within a bucket where it should be stored,
  ## and the tab within a google sheet where it should be stored.
  ## Intuits the proper Sheet URL from which state abbreviation is included in the sub_folder structure
  ## Assumes file_name ends with file type (.html, .png)
  
  gs_tab <- get_gs_tab(name)
  
  sub_folders <- paste0(state_abbr, "/", tolower(gs_tab), "/")
  
  save_png(gg_plot, sub_folders, name) 

  
  # object url is everything after the general aws url and bucket details
  # ie /TX/overview/name.html
  object_url <- paste0(sub_folders, name, ".html")
  
  # to get state abbr, take first two characters of sub_folders -- helper function below
  state_abbr <- str_extract(object_url, "^.{2}")
  sheet_url <- get_gs_link(state_abbr)
  
  # see below for both functions
  save_to_aws(gp_object, name, object_url)
  update_google_sheet(sheet_url, gs_tab, name, object_url)
}


get_gs_tab <- function(name) {
  gs_tab <- ifelse(grepl("yoy", name), "Overview", 
                             paste0("SFY", str_sub(name,-2)))
  return(gs_tab)
  }


save_png <- function(gg_plot, sub_folders, name) {
  ## Creates a temp png, saves a ggplot to it, and pushes to AWS in subfolders
  temp_png <- tempfile(fileext = ".png")
  ggsave(temp_png, plot = gg_plot, dpi = 300)
  put_object(file = temp_png,
             object = paste0(sub_folders, name, ".png"),
             acl="public-read",
             bucket = "funding-tracker")
  unlink(temp_png)
}


save_to_aws <- function(gp_object, name, object_url) {
  ## Take a HTML object, it's string name, and the intended URL and push to AWS
  
  htmlwidgets::saveWidget(partial_bundle(gp_object) %>%
                            config(displayModeBar = FALSE) %>%
                            config(displaylogo = FALSE) %>%
                            layout(xaxis = list(fixedrange = TRUE),
                                   yaxis = list(fixedrange = TRUE))%>%
                            layout(plot_bgcolor='transparent') %>%
                            layout(paper_bgcolor='transparent'),
                          paste0("output/", name, ".html"))
  
  put_object(
    file = file.path(paste0("output/", name, ".html")),
    acl="public-read",
    object = object_url,
    bucket = "funding-tracker"
  )
  
}

get_gs_urls <- function() {
  ## Create simple dataframe of states and the pre-generated spreadsheets to store AWS links for easy access
  gs_urls <- data.frame(state=c(""), url=c("")) %>%
    add_row(state="TX", url="https://docs.google.com/spreadsheets/d/1rlw0zeu1hmw2rNO1d1lJEihvVM3raUs7C_wfnGy4Jd4") %>%
    add_row(state="AL", url="https://docs.google.com/spreadsheets/d/1PZO5UfGZ_FHhDKnyVVtu6G_E2HJEegVmw7FGIuWQHNk") %>%
    add_row(state="NY", url="https://docs.google.com/spreadsheets/d/1oH07f-AOPzxlB6ozxjtmw1JKYx2qEFMCHAB-WaWs8W4") %>%
    add_row(state="IL", url="https://docs.google.com/spreadsheets/d/1cM8G-83FtrW98MYgvhV5iyaY6m81wDRM0Yo34L2qrZU") %>%
    add_row(state="IN", url="https://docs.google.com/spreadsheets/d/1sV1SUmGlybQTw8VdfyOfw0xSqT8aX--_Tau3vMDgVDs") %>%
    add_row(state="MI", url="https://docs.google.com/spreadsheets/d/1cSYUzZgLUhN10ZMw7QJUtkBlTzMmPnjs5bLHJ17itYY") %>%
    add_row(state="OH", url="https://docs.google.com/spreadsheets/d/1NE0Pl4p1Yv_lXBSIoLS89KpqmIUBWtBPoLQzj2GH2F4") %>%
    add_row(state="PA", url="https://docs.google.com/spreadsheets/d/1-MrH5VRuPABvovqz9fHBGufc-Zz2vpEqZcrPgQyNQzI") %>%
    add_row(state="TN", url="https://docs.google.com/spreadsheets/d/1mCDIMe41m4vJDnekpigRfB_sEvN74Z646J5a4VTvKKU") %>%
    add_row(state="LA", url="https://docs.google.com/spreadsheets/d/17jeMqu1AznH8ns52A5ZbdYQou39u7PLCC_s2DS5i44c")
  
  return(gs_urls)
}

get_gs_link <- function(state_abbr) {
  ## Given a state abbreviation, fetch the google sheet URL
  gs_urls <- get_gs_urls()
  return(gs_urls$url[gs_urls$state==state_abbr])
}

update_google_sheet <- function(sheet_url, sheet_tab, name, object_url) {
  
  # complete URL for AWS to store in Google Sheets
  aws_url <- paste0("https://funding-tracker.s3.us-east-1.amazonaws.com/", object_url)
  
  # Read the existing Google Sheet to get current data
  sheet_data <- read_sheet(sheet_url, sheet = sheet_tab)
  
  # if data exists
  if (nrow(sheet_data) > 0) {
    
    # check if file_name already exists
    file_exists <- sheet_data %>% filter(`Name` == name)
    
    # if the file exists, update the existing row
    if (nrow(file_exists) > 0) {
      sheet_data <- sheet_data %>%
        mutate(
          `Link` = ifelse(`Name` == name, aws_url, as.character(`Link`)),
          `Updated` = ifelse(`Name` == name, format(Sys.Date(), "%m/%d/%Y"), as.character(`Updated`))
        )
    } else {
      # if file doesn't exist, append a new row
      new_row <- tibble(`Name` = name, `Link` = aws_url, `Updated` = format(Sys.Date(), "%m/%d/%Y"))
      sheet_data <- bind_rows(sheet_data, new_row)
    } # end row bind else
    
  }  else { # if the sheet is empty, simply add a new row
    sheet_data <- tibble(`Name` = name, `Link` = aws_url, `Updated` = format(Sys.Date(), "%m/%d/%Y"))
  }
  
  # export to sheet
  write_sheet(sheet_data, sheet_url, sheet = sheet_tab)
}
