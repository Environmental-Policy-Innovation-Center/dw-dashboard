clean_mi_y0 <- function() {
  
  mi_iup <- read.csv("year0/MI/data/mi-fy22-iup-final.csv") %>%
    clean_names()
  
  mi_clean <- mi_iup %>%
    filter(!is.na(rank)) %>%
    mutate(community_served = str_squish(county),
           borrower = str_squish(project_name),
           project_id = str_squish(project),
           project_type =  case_when(
             grepl(lead_str, project_description, ignore.case=TRUE) | water_infrastructure_fund_transfer_act_wifta_amount != ""  ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
             TRUE ~ "General"),
           requested_amount = clean_numeric_string(project_amount),
           funding_amount = clean_numeric_string(dwsrf_loan_amount),         
           principal_forgiveness = clean_numeric_string(total_principal_forgiveness_non_wifta),
           population = as.character(NA),
           project_description = str_squish(project_description),
          disadvantaged = str_squish(disadvantaged_community),
          disadvantaged = ifelse(disadvantaged=="", "No", "Yes"),
          project_rank = str_squish(rank),
          project_score = str_squish(total_points),
          expecting_funding = ifelse((funding_amount != "0" & funding_amount != "No Information"),
                                        "Yes", "No"),
          project_name = as.character(NA),
          project_cost = as.character(NA),
          pwsid = as.character(NA),
          state = "Michigan",
          state_fiscal_year = "2022"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  mi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No duplicates
  
  # Check for disinfection byproduct in description
  mi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No disinfection byproduct string
    
  ####### SANITY CHECKS END #######
  
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}
