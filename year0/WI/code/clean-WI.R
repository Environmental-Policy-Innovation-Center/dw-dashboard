clean_wi_y0 <- function() {
  
  wi_ppl <- read.csv("year0/WI/data/sfy2022-ppl.csv") |>
    janitor::clean_names() |>
    # projects on the "Not Eligible" list shouldn't be included in the dataset at all
    dplyr::filter(!grepl("INELIGIBLE", project_description)) |>
    dplyr::mutate(community_served = stringr::str_to_title(municipality),
           project_id = stringr::str_squish(project_number),
           project_id = normalize_dashes(project_id),
           project_cost = clean_numeric_string(estimated_project_cost),
           project_description = stringr::str_squish(project_description),
           population = clean_numeric_string(population),
           project_score = stringr::str_squish(self_score),
           list = case_when(
             grepl("UNDETERMINED", project_description) ~ "SFY22 Not Fundable List",
             TRUE ~ "SFY22 Comprehensive List")) |>
    dplyr::select(community_served, project_id, project_cost,
           project_description, population, project_score, list)
  
  
  wi_fund <- read.csv("year0/WI/data/SFY2022-Funding-List.csv") |>
    janitor::clean_names() |>
    dplyr::mutate(project_id = stringr::str_squish(project_number),
           project_id = stringr::str_replace(project_id, "‐", "-"),
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, fill_na=TRUE),
           principal_forgiveness = convert_to_numeric(pf_estimate, fill_na=TRUE),
           funding_amount = estimated_loan_amount + principal_forgiveness,
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = as.character(principal_forgiveness),
           disadvantaged = ifelse(as.numeric(total_pf_points) >= 50, "Yes", "No"),
           expecting_funding = "Yes",
           list = "SFY22 Fundable List") |>
    dplyr::select(project_id, requested_amount, funding_amount, principal_forgiveness,
           disadvantaged, expecting_funding, list)
  
  
  # separate the funding projects that don't appear on the PPL
  wi_fund_extras <- read.csv("year0/WI/data/SFY2022-Funding-List.csv") |>
    janitor::clean_names() |>
    dplyr::mutate(project_id = stringr::str_squish(project_number),
           project_id = stringr::str_replace(project_id, "‐", "-")) |>
    dplyr::filter(project_id %in% c("5584-08", "5191-13", "5340-07",
                             "5430-09", "5430-10")) |>
    dplyr::mutate(community_served = stringr::str_to_title(municipality),
           project_description = stringr::str_squish(project_description),
           population = clean_numeric_string(population),
           project_score = stringr::str_squish(priority_score),
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, fill_na=TRUE),
           principal_forgiveness = convert_to_numeric(pf_estimate, fill_na=TRUE),
           funding_amount = estimated_loan_amount + principal_forgiveness,
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(pf_estimate),
           disadvantaged = ifelse(as.numeric(total_pf_points) >= 50, "Yes", "No"),
           expecting_funding = "Yes",
           list = "SFY22 Fundable List"
           ) |>
    dplyr::select(community_served, project_description, population,
           project_score, requested_amount, principal_forgiveness,
           funding_amount, disadvantaged, expecting_funding, project_id, list)
    
  
  
  wi_clean <- wi_ppl |>
    dplyr::left_join(wi_fund, by="project_id") |>
    dplyr::mutate(list = ifelse(is.na(list.y), list.x, list.y)) |>
    dplyr::select(-list.x, -list.y)
  
  wi_clean <- bind_rows(wi_clean, wi_fund_extras)
  
  
  
  wi_clean <- wi_clean |>
    dplyr::mutate(
           project_type = case_when(
            grepl(ec_str, project_description) ~ "Emerging Contaminants",
            grepl("lead|lsl", project_description) ~ "Lead",
            TRUE ~ "General"),
           borrower = as.character(NA),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           project_rank = as.character(NA),
           project_cost = replace_na(project_cost, "No Information"),
           project_score = replace_na(project_score, "No Information"),
           requested_amount = replace_na(requested_amount, "No Information"),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           disadvantaged = replace_na(disadvantaged, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           state_fiscal_year = "2022",
           state = "Wisconsin") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year, list)
  
  
  run_tests(wi_clean)
  rm(list=setdiff(ls(), "wi_clean"))
  
  return(wi_clean)
}


####### SANITY CHECKS START #######

# Hone in on project id duplication
# wi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: No duplicates

# Check for disinfection byproduct in description
# wi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No change, classified as expected

# Check for lead subtypes
 # wi_clean |>
 #   dplyr::filter(project_type=="Lead") |>
 # dplyr::mutate(
 #   lead_type = dplyr::case_when(
 #   stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
 #   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
 #   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
 #  # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
 #  stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
 #  stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
 #   TRUE ~ "unknown"
 # )) |>
 #   dplyr::filter(lead_type == "both")
# ####### Decision: No lead projects classified as both

####### SANITY CHECKS END #######