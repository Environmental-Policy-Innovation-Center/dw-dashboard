source(here("resources", "cleaning.R"))
### Drive Data Access Functions ----

# Because these datasets are pulled from work-in-progress Google Drive sheets, code that relies on them
# can be fragile. As such, these functions exist to standardize pulling and cleaning the data in one location
# so any changes to column names, types, or additional features can be handled here exclusively.

get_financial <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  
  financial <- data.frame(read_sheet(URL, sheet="Financial Data", skip=1))
  
  financial <- financial %>%
    clean_names() %>%
    filter(state == state_name) %>%
    filter(!grepl("total", ignore.case=TRUE, fed_cap_grant)) %>%
    select(-notes, -assignee, -reviewed, -questions, -vlookup) %>%
    # ensure state_fiscal_year is string, arrange the dataframe by year, then set it to factor, levels in ascending order
    mutate(state_fiscal_year = as.character(state_fiscal_year)) %>%
    arrange(state_fiscal_year) %>%
    mutate(state_fiscal_year = factor(state_fiscal_year)) %>%
    # each of these are mutated individually because some rows come in as text, numerical or a list, depending on their contents
    # and mutating with across() produces errors or warnings depending on the context, here at least it is only warnings
    # that can be muted, but the resulting dataframe is still as intended
    mutate(total_fcg = convert_to_numeric(total_fcg),
           ffy21_fcg = convert_to_numeric(ffy21_fcg),
           ffy22_fcg = convert_to_numeric(ffy22_fcg),
           ffy23_fcg = convert_to_numeric(ffy23_fcg),
           ffy24_fcg = convert_to_numeric(ffy24_fcg),
           ffy25_fcg = convert_to_numeric(ffy25_fcg),
           ffy26_fcg = convert_to_numeric(ffy26_fcg),
           nonsrf_state_funds = convert_to_numeric(nonsrf_state_funds),
           arpa_funds = convert_to_numeric(arpa_funds),
           reallotments = convert_to_numeric(reallotments),
           cw_transferred_to_dw = convert_to_numeric(cw_transferred_to_dw),
           dw_transferred_to_cw = convert_to_numeric(dw_transferred_to_cw),
           unutilized_fcg = convert_to_numeric(unutilized_fcg),
           leveraged_funds = convert_to_numeric(leveraged_funds),
           funds_reserved_leveraging = convert_to_numeric(funds_reserved_leveraging),
           total_funding_available = convert_to_numeric(total_funding_available),
           gpr_cost = convert_to_numeric(gpr_cost))
  
  return(financial)
}


get_set_asides <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  set_asides <- data.frame(read_sheet(URL, "Set Asides", skip=1))
  
  set_asides <- set_asides %>%
    clean_names() %>%
    filter(state==state_name) %>%
    # drop all total columns used by policy analysts
    filter(!grepl("total", ignore.case=TRUE, allowance)) %>%
    # drop policy analyst specific columns
    select(-notes, -assignee, -reviewed, -questions) %>%
    # ensure state_fiscal_year is string, arrange the dataframe by year, then set it to factor, levels in ascending order
    mutate(state_fiscal_year = as.character(state_fiscal_year)) %>%
    arrange(state_fiscal_year) %>%
    mutate(state_fiscal_year = factor(state_fiscal_year)) %>%
    # convert strings to numeric
    mutate(total_sa_amt = convert_to_numeric(total_sa_amt),
           total_sa_pct = convert_to_numeric(total_sa_pct),
           ffy22_sa_amt = convert_to_numeric(ffy22_sa_amt),
           ffy22_sa_pct = convert_to_numeric(ffy22_sa_pct),
           ffy23_sa_amt = convert_to_numeric(ffy23_sa_amt),
           ffy23_sa_pct = convert_to_numeric(ffy23_sa_pct),
           ffy24_sa_amt = convert_to_numeric(ffy24_sa_amt),
           ffy24_sa_pct = convert_to_numeric(ffy24_sa_pct),
           ffy25_sa_amt = convert_to_numeric(ffy25_sa_amt),
           ffy25_sa_pct = convert_to_numeric(ffy25_sa_pct),
           ffy26_sa_amt = convert_to_numeric(ffy26_sa_amt),
           ffy26_sa_pct = convert_to_numeric(ffy26_sa_pct),
           total_sa_amt = convert_to_numeric(total_sa_amt),
           total_sa_pct = convert_to_numeric(total_sa_pct),
           total_fcg = convert_to_numeric(total_fcg),
           unutilized_set_asides = convert_to_numeric(unutilized_set_asides),
    ) %>%
    mutate(
      max_set_aside = case_when(
        allowance == "Administration and Technical Assistance (up to 4%)" ~ "Max 4%",
        allowance == "Small System (<10,000 population) Technical Assistance (up to 2%)" ~ "Max 2%",
        allowance == "State Program Management (10%)" ~ "Max 10%",
        allowance == "Local Assistance and other State Programs (up to 15%)" ~ "Max 15%",
        allowance == "Local Assistance and other State Programs (up to 10%)" ~ "Max 10%",
        TRUE ~ "Missing Max"),
      
      allowance = case_when(
        allowance == "Administration and Technical Assistance (up to 4%)" ~ "Admin & TA",
        allowance == "Small System (<10,000 population) Technical Assistance (up to 2%)" ~ "Small System TA",
        allowance == "State Program Management (10%)" ~ "State Program Management",
        allowance == "Local Assistance and other State Programs (up to 15%)" ~ "Local Assistance & Other",
        allowance == "Local Assistance and other State Programs (up to 10%)" ~ "Local Assistance & Other",
        TRUE ~ "Missing Allowance"),
    ) 
  
  return(set_asides)
}

get_pf <- function(state_name) {
  
  URL <- "https://docs.google.com/spreadsheets/d/10NcgSJAZedRNDTq7_9-UJUefF6tQ6RYhvVSib_Dc-3Y/edit?usp=sharing"
  principal_forgiveness <- data.frame(read_sheet(URL, "Principal Forgiveness", skip=1))
  
  principal_forgiveness <- principal_forgiveness %>%
    clean_names() %>%
    filter(state==state_name) %>%
    # drop policy analyst specific columns
    select(-notes, -assignee, -reviewed, -questions) %>%
    # ensure state_fiscal_year is string, arrange the dataframe by year, then set it to factor, levels in ascending order
    mutate(state_fiscal_year = as.character(state_fiscal_year)) %>%
    arrange(state_fiscal_year) %>%
    mutate(state_fiscal_year = factor(state_fiscal_year)) %>%
    # convert strings to numeric
    mutate(total_fcg = convert_to_numeric(total_fcg),
           total_pf_amt = convert_to_numeric(total_pf_amt),
           total_pf_pct  = convert_to_numeric(total_pf_pct),
           ffy21_fcg = convert_to_numeric(ffy21_fcg),
           ffy21_amt = convert_to_numeric(ffy21_amt),
           ffy21_pct = convert_to_numeric(ffy21_pct),
           ffy22_fcg = convert_to_numeric(ffy22_fcg),
           ffy22_amt = convert_to_numeric(ffy22_amt),
           ffy22_pct = convert_to_numeric(ffy22_pct),
           ffy23_fcg = convert_to_numeric(ffy23_fcg),
           ffy23_amt = convert_to_numeric(ffy23_amt),
           ffy23_pct = convert_to_numeric(ffy23_pct),
           ffy24_fcg = convert_to_numeric(ffy24_fcg),
           ffy24_amt = convert_to_numeric(ffy24_amt),
           ffy24_pct = convert_to_numeric(ffy24_pct),
           ffy25_fcg = convert_to_numeric(ffy25_fcg),
           ffy25_amt = convert_to_numeric(ffy25_amt),
           ffy25_pct = convert_to_numeric(ffy25_pct),
           ffy26_fcg = convert_to_numeric(ffy26_fcg),
           ffy26_amt = convert_to_numeric(ffy26_amt),
           ffy26_pct = convert_to_numeric(ffy26_pct),
           unutilized_pf = convert_to_numeric(unutilized_pf),
    )
  
  
  return(principal_forgiveness)
  
}