source("resources.R")

clean_in <- function() {
 
  ## PPL
  # (73,17)
  in_ppl <- fread("year1/IN/data/14-Indiana_Q4Final_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ## Lead
  # (9,17)
  in_lead_ppl <- fread("year1/IN/data/14-Indiana_Q4Final_LeadPPL.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (82,17)
  in_combined <- bind_rows(in_ppl, in_lead_ppl)
  
  
  # -> (82,12)  
  in_clean <- in_combined %>%  
    # drop non-project rows
    filter(participant != "NA") %>%
    # process numeric columns
    mutate(
      population = clean_numeric_string(population_served),
      requested_amount = clean_numeric_string(requested_funds),
    ) %>%
    # process text column
    mutate(
      borrower = str_squish(participant),
      pwsid = case_when(
        pwsid_no_s == "TBD" ~ "No Information",
        TRUE ~ paste0("IN", pwsid_no_s)),
      project_id = str_squish(srf_project_no),
      project_description = str_squish(project_description),
      project_score = str_replace_all(ppl_score, "[^0-9.]", ""),
      project_rank = str_replace_all(ppl_rank, "[^0-9.]", ""),
      disadvantaged = str_squish(disadvantaged_community),
      project_type = case_when(
        emerging_contaminants == "Yes" ~ "Emerging Contaminants",
        clean_numeric_string(lead_service_line_replacement_cost) > 0 ~ "Lead",
        TRUE ~ "General"
      ),
      state = "Indiana",
      state_fiscal_year = "2023",
      expecting_funding = as.character(NA),
      community_served = as.character(NA),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(in_clean)
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}