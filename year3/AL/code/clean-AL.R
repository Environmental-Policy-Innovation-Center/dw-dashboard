source("resources.R")


clean_al_y3 <- function() {
  base_path <- "year3/AL/data"
  
  # base ppl (24, 14)
  al_base <- fread(file.path(base_path, "al-y3-baseppl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate_all(., ~str_squish(.)) %>%
    # there is one project number also in al_supp but a mismatch in applicant
    # name: 
    mutate(applicant_name = case_when(
      project_number == "FS010096-11" ~ paste0(applicant_name, "**"), 
      TRUE ~ applicant_name)) %>%
    rename(disadvantaged_score = disadva_ntaged_score) %>%
    # we don't need these columns, and they duplicate project numbers 
    # in the merge
    select(-c(fund, justice_40_map_coverage))
  
  # bil gen sup ppl (15, 14)
  al_supp <- fread(file.path(base_path, "al-y3-bilgensupp-iup.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate_all(., ~str_squish(.)) %>%
    select(-c(fund, justice_40_map_coverage))
  
  # lsl ppl (11, 14)
  al_lsl <- fread(file.path(base_path, "al-y3-lsl-ppl.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate_all(., ~str_squish(.)) %>%
    mutate(project_type = "Lead") 
  
  # (49,) - there are a couple of duplicated project numbers, hence the 
  # need for a merge: 
  al_gen <- merge(al_base, al_supp,
                  by = c("project_number", "applicant_name", "priority_ranking_points",
                         "applied_for_project_amount", "disadvantaged_score",
                         "population", "city_town", "project_description", "county"), 
                  all = T) %>%
    # there is one overlapping lsl project: "FS010488-02" but it has a different 
    # project name, confirmed it should be listed twice
    bind_rows(., al_lsl) %>%
    # replace NAs in non-matching columns to zero for math
    mutate(across(.cols = c("dw_bil_amount_granted", "dw_srf_amount_granted", 
                            "dw_bil_lsl_amount_of_pf", "dw_srf_amount_of_pf", 
                            "dw_bil_amount_of_pf"), 
                  ~case_when(
                    is.na(.) ~ "$0", 
                    TRUE ~ . ))) %>%
    mutate(
      ## funding amount: 
      funding_amount = case_when(
      # if lead, sum BIL lead amount granted, plus SRF amount granted
      project_type == "Lead" ~ convert_to_numeric(dw_srf_amount_granted) + convert_to_numeric(dw_bil_lead_amount_granted),
      # if general, sum DW SRF amount granted and BIL amount granted
      TRUE ~ convert_to_numeric(dw_srf_amount_granted) + convert_to_numeric(dw_bil_amount_granted)), 
      
      ## PF: 
      principal_forgiveness = case_when(
        # if lead, sum bil lead amount and srf srf amount of pf
        project_type == "Lead" ~ convert_to_numeric(dw_bil_lsl_amount_of_pf) + convert_to_numeric(dw_srf_amount_of_pf),
        # if general, sum srf amount of PF and DW bil amount of PF 
        TRUE ~ convert_to_numeric(dw_srf_amount_of_pf) + convert_to_numeric(dw_bil_amount_of_pf)
      ))  %>%
    # character columns 
    mutate(funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(principal_forgiveness))

  # ec ppl (7, 14)
  al_ec <- fread(file.path(base_path, "al-y3-ec-ppl.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants", 
           funding_amount = clean_numeric_string(dw_bil_ec_amount_granted), 
           principal_forgiveness = clean_numeric_string(dw_bil_ec_pf))
  
  # (55, ) - none of the ec project names are in al_gen, so we can bind
  al_clean <- bind_rows(al_gen, al_ec) %>%
    # in prep for identifying disadvantaged flag: 
    mutate(disadvantaged_score = convert_to_numeric(disadvantaged_score)) %>%
    mutate(community_served = str_squish(city_town), 
           borrower = str_squish(applicant_name), 
           pwsid = as.character(NA),
           project_id = str_squish(project_number), 
           project_name = as.character(NA),
           project_type = case_when(
             is.na(project_type) ~ "General", 
             TRUE ~ project_type
           ), 
           project_cost = as.character(NA),
           requested_amount = clean_numeric_string(applied_for_project_amount),
           # we have funding and PF covered above.
           project_description = str_squish(project_description),
           population = clean_numeric_string(population), 
           disadvantaged = case_when(
             disadvantaged_score > 1 ~ "Yes", 
             TRUE ~ "No"), 
           project_rank = as.character(NA),
           project_score = str_squish(priority_ranking_points), 
           expecting_funding = "Yes", 
           state = "Alabama",
           state_fiscal_year = "2025") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}