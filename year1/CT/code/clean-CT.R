source("resources.R")

clean_ct_y1 <- function() {
  
  # 199, 16
  ct_comprehensive <- fread("year1/CT/data/CT-iup-attachmentD.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # (107, 15) -> (107,4)
  ct_base_supplemental <- fread("year1/CT/data/CT-iup-attachmentF.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # manually fix error in the PDF data where a project is misidentified by project name
    mutate(project_number = case_when(
      project_number == "SFY 23-45"~ "SFY 23-86",
      TRUE ~ project_number),
          project_name = case_when(
            project_number == "SFY 22-02" ~ "Lead Service Lines - Replacement Phase 1  (Construction)2",
            TRUE ~ project_name
          )
      ) %>%
    # format columns specific to funded data and for merging
    mutate(expecting_funding = "Yes",
           project_name = str_squish(project_name),
           funding_amount = clean_numeric_string(amount_requested)
             ) %>%
    select(project_number,  project_name, expecting_funding, funding_amount)
  
  
  ct_clean <- ct_comprehensive %>%
    # preemptively fix spaces in ct_comprehensive so it will merge correctly
    mutate(project_name = str_squish(project_name)) %>%
    left_join(ct_base_supplemental, by=c("project_number", "project_name")) %>%
    # process funded and not funded together
    mutate(
      population = clean_numeric_string(population_served_by_project),
      requested_amount =  clean_numeric_string(amount_requested), 
      funding_amount = replace_na(funding_amount, "No Information")
    ) %>%
    mutate(community_served = str_squish(town_of_pws),
           borrower = str_squish(public_water_system),
           pwsid = case_when(
             pwsid == "none" ~ "No Information",
             TRUE ~ str_squish(pwsid)
                             ),
           project_id = str_squish(project_number),
           project_description = str_squish(project_name),
           disadvantaged = case_when(
             project_serves_a_disadvantaged_community == "Yes" ~ "Yes",
             TRUE ~ "No"),
           project_rank = str_squish(rank),
           project_rank = replace_na(project_rank, "No Information"),
           project_type = case_when(
             lead_service_line_estimated_amount != "$0" ~ "Lead",
             emerg_contam_estimated_amount != "$0" ~ "Emerging Contaminants",
             TRUE ~ "General"),
           expecting_funding = case_when(
             expecting_funding == "Yes" ~ "Yes",
             TRUE ~ "No"),
           state = "Connecticut",
           state_fiscal_year = "2023",
           project_cost = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_score = as.character(NA),
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(ct_clean)
  rm(list=setdiff(ls(), "ct_clean"))
  
  return(ct_clean)
}