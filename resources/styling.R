

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
                          "LSLR" = "#4ea324",
                          "IIJA Gen Supp" ="#1054a8",
                          "Base & IIJA Gen Supp" = "#526489")

allowance_colors <- c("Admin & TA" ="#172f60",
                      "Small System TA"= "#1054a8",
                      "State Program Management"="#4ea324",
                      "Local Assistance & Other"="#791a7b")

pf_colors <- c("DAC"="#526489",
               "Discretionary"="#172f60",
               "IIJA - EC"="#791a7b",
               "IIJA - LSLR"="#4ea324",
               "IIJA Gen Supp"="#1054a8",
               "Remaining Loans" = "lightgrey")

# Project YOY Colors
pt_colors <- c("General"="#526489",
               "Lead"="#82AB6E",
               "Emerging Contaminants"="#B077B2")

ef_dac_sc_colors <- c("Yes"="#82AB6E",
                      "No"="#526489",
                      "No Info"="lightgrey")

lead_colors <- c("Inventory"="#B077B2",
                 "Replacement"="#82AB6E",
                 "Neither"="#526489")

# Combined Colors
comp_colors <- c("Total Available"="#4ea324",
                 "Available Funds"="#4ea324",
                 "Unmet Demand"="#172f60")

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