source("resources.R")

clean_de <- function() {
  
  ### APPLICANT
  # only three projects on this list aren't already on the fundable list
  de_ppl <- fread("year1/DE/data/8-Delaware_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    filter(
      (rank == "43" & water_system_borrower == "Artesian") |
        (rank == "44" & water_system_borrower == "Artesian") | 
        (rank == "39" & water_system_borrower == "Artesian")  
    ) %>%
    mutate(expecting_funding = "No") %>%
    rename(project_name = comprehensive_project_name)
  
  ### FUNDED
  de_fund <- fread("year1/DE/data/8-Delaware_PPL_fund.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    rename(project_name = fundable_project_name)
  
  
  de_all <- bind_rows(de_fund, de_ppl)
  
  
  de_clean <- de_all %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population_served),
           # sum of columns, getting rid of everything besides numbers and periods
           funding_amount = clean_numeric_string(amount),
           principal_forgiveness = clean_numeric_string(anticipated_subsidy_amount)
    ) %>%
    # process text columns
    mutate(project_type = case_when(grepl("lead|lsl", project_name, ignore.case = T) ~
                                      "Lead",
                                    grepl("pfas", project_name, ignore.case = T) ~
                                      "Emerging Contaminants",
                                    TRUE ~ "General"),
           project_score = str_replace_all(total_points,"[^0-9.]",""),
           project_rank = str_replace_all(rank,"[^0-9.]",""),
           expecting_funding = "Yes",
           disadvantaged = case_when(!is.na(dac_a_e_w_u) ~ "Yes",
                                     TRUE ~ "No"),
           pwsid_number = str_replace(pwsid_number, "Delmar DE0000567 Holy Oak               DE0000568", "DE0000567"),
           borrower = str_squish(water_system_borrower),
           pwsid = str_squish(pwsid_number),
           pwsid = replace_na(pwsid, "No Information"),
           project_name = str_squish(project_name),
           project_description = str_squish(project_description),
           state = "Delaware",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_cost = as.character(NA),
           requested_amount = as.character(NA),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(de_clean)
  rm(list=setdiff(ls(), "de_clean"))

return(de_clean)

}