## takes a numeric column and returns one formatted with dollar sign and commas
## IE 1000000 -> $1,000,000
format_currency <- function(num_col) {
  #typically this will already have been done, but a failsafe
  num_col <- as.numeric(num_col)
  
  # Apply formatting for each number in the column
  formatted_col <- sapply(num_col, function(num) {
    formatted <- formatC(num, format = "f", big.mark = ",", digits = 2)
    paste0("$", formatted)
  })
  return(formatted_col)
}

## takes a numeric column of the default percent setting from google sheets
## and returns a string column in a user-friendly format 
## IE .16 -> 16%
format_percent <- function(x) {
  # Format the numbers as percentages, rounding to 0 decimal places
  formatted <- paste0(round(x * 100), "%")
  
  return(formatted)
}

## takes the state_fiscal_year column from a given dataset
## and the state_name provided at the top of the notebook
## and formats the subtitle for a given plot
get_subtitle_str <- function(sfy_column, state_name) {
  sfy_column_ordered <- sfy_column[order(sfy_column)]
  # get the first and last two years of the first year in the list
  first_year <- str_sub(as.character(head(sfy_column_ordered, n=1)), -2)
  last_year <- str_sub(as.character(tail(sfy_column_ordered, n=1)), -2)
  
  subtitle_str <- ifelse(first_year == last_year, 
                         paste0(state_name, ", SFY", first_year),
                         paste0(state_name, ", SFY", first_year, "-", last_year))
  
  # return formated string of state name and years in the dataset
  return(subtitle_str)
}


### Data Viz Template Settings ----

# fonts: 
sysfonts::font_add_google("Lato")

showtext::showtext_auto()

# theme: 
# legend position is right, text size is at least 10, and text is Lato
#TODO: Replace font with web-friendly option for website to be san-serif by default OR 
# upload custom fonts to AWS to make them look better
epic_chart_theme <- theme_minimal() + 
  theme(legend.position = "right", 
        text = element_text(size = 11), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), 
        axis.text.x = element_text(margin = margin(t = 10, r = 0, 
                                                   b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, 
                                                    b = 0, l = 0)), 
        axis.text.y = element_text(margin = margin(t = 0, r = 10, 
                                                   b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0))) 

# Funding YOY Colors
fed_cap_grant_colors <- c("Base" = "#172f60",
                          "EC" = "#791a7b",
                          "EC_FFY22" = "#a642a8",
                          "EC_FFY23" = "#5d1460",
                          "LSLR" = "#4ea324",
                          "LSLR_FFY22" = "#6bc842",
                          "LSLR_FFY23" = "#3d7f1c",
                          "IIJA Gen Supp" ="#1054a8",
                          "Base & IIJA Gen Supp" = "#526489",
                          "Base, IIJA Gen Supp, and EC" = "#B077B2",
                          "Base, IIJA Gen Supp, LSLR, and EC" = "#82AB6E",
                          "IIJA Gen Supp, LSLR, and EC" = "#527CAF")

fed_cap_grant_linetypes <- c("Base" = "solid",
                          "EC" = "dashed",
                          "EC_FFY22" = "dashed",
                          "EC_FFY23" = "dashed",
                          "LSLR" = "dotted",
                          "LSLR_FFY22" = "dotted",
                          "LSLR_FFY23" = "dotted",
                          "IIJA Gen Supp" ="dotdash",
                          "Base & IIJA Gen Supp" = "longdash",
                          "Base, IIJA Gen Supp, and EC" = "twodash",
                          "Base, IIJA Gen Supp, LSLR, and EC" = "twodash",
                          "IIJA Gen Supp, LSLR, and EC" = "solid")

allowance_colors <- c("Admin & TA" ="#172f60",
                      "Small System TA"= "#1054a8",
                      "State Program Management"="#4ea324",
                      "Local Assistance & Other"="#791a7b")

pf_colors <- c("DAC PF"="#526489",
               "Discretionary PF"="#172f60",
               "IIJA - EC"="#791a7b",
               "IIJA - EC_FFY22"="#a642a8",
               "IIJA - EC_FFY23"="#5d1460",
               "IIJA - LSLR"="#4ea324",
               "IIJA - LSLR_FFY22"="#6bc842",
               "IIJA - LSLR_FFY23"="#3d7f1c",
               "IIJA Gen Supp"="#1054a8",
               "Remaining Loans" = "lightgrey")

# Project YOY Colors
pt_colors <- c("General"="#526489",
               "Lead"="#82AB6E",
               "Emerging Contaminants"="#B077B2")

ef_dac_sc_colors <- c("No"="#526489",
                      "Yes"="#82AB6E",
                      "No Info"="lightgrey")

lead_colors <- c("Inventory"="#B077B2",
                 "Replacement"="#82AB6E",
                 "Other"="#526489")

# Combined Colors
comp_colors <- c("Total Available"="#4ea324",
                 "Available Funds"="#4ea324",
                 "Total Demand"="#172f60",
                 "Expected Funding"="#791a7b",
                 "Unmet Demand"="lightgrey")

# Single SFY Colors
distribution_colors <- c("Principal Forgiveness"="#4ea324",
                         "DAC PF"="#4ea324",                              
                         "Discretionary PF"="#82AB6E",                    
                         "Admin & TA Set Asides"="#172f60",               
                         "Small System TA Set Asides"="#1054a8",
                         "State Program Management Set Asides"="#526489",
                         "Local Assistance & Other Set Asides"="#527CAF",
                         "Remaining Base Grant, for Loans"="lightgrey",
                         "Remaining Gen Supp Grant, for Loans"="lightgrey",
                         "Remaining LSLR Grant, for Loans"="lightgrey",
                         "Remaining Base Grant, for Loans"="lightgrey")

ef_dac_comp_colors <- c(
  "DACs, No Info"="lightgrey", 
  "DACs, Not Expecting Funding"="#172f60", 
  "DACs, Expecting Funding"="#4ea324",
  "Expecting Funding, DAC"="#4ea324", 
  "Expecting Funding, Not DAC"="#172f60", 
  "Expecting Funding, No Info"="lightgrey"
)

ef_dac_hm_colors <- c("#172f60", "#B9D1FF")

sf_colors <- c(
  "DWSRF Funds" = "#172f60",
  "State Funds" = "#4ea324"
)

base_pf_colors<- c(
  "Allocated DAC PF" = "#526489",
  "Unallocated DAC PF" = "#528989ff",
  "Allocated Discretionary PF" = "#172f60",
  "Unallocated Discretionary PF" ="#176050ff"
  )

lslr_pf_colors <- c(
  "Allocated IIJA - LSLR" = "#4ea324",
  "Unallocated IIJA - LSLR" = "#4ea32487"
)
