clean_ny <- function() {
  base_path <- file.path("year2", "NY", "data")
  
  ## Base
  
  ny_base <- fread(file.path(base_path,"32-y2-NewYork_base_ppl.csv"),
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General")
  
  ## BIL
  ny_bil <- fread(file.path(base_path,"32-y2-NewYork_bil_ppl.csv"),
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General",
           disadvantaged = "Yes") %>%
    select(project_number, disadvantaged)
  
  
  # all projects on BIL are also on base
  ny_base_bil <- ny_base %>%
    left_join(ny_bil, by="project_number") %>%
    mutate(disadvantaged = case_when(
      score == "H" & is.na(disadvantaged) ~ "Yes",
      is.na(disadvantaged) ~ "No",
      TRUE ~ disadvantaged)
    )
  
  
  ## Lead
  ny_lead <- fread(file.path(base_path, "32-y2-NewYork_bil_lead.csv"),
                   colClasses="character", na.strings=c("", "NA")) %>%
    clean_names() %>%
    mutate(
      project_type = "Lead",
      disadvantaged = case_when(
        dac == "DAC" ~ "Yes",
        is.na(dac) | dac == "" ~ "No",
        TRUE ~ "No"
      )
    )
  
  
  ## Emerging Contaminants - Updated for Year 2
  ny_ec <- fread(file.path(base_path, "32-y2-NewYork_bil_ec.csv"),
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = case_when(
             dac == "DAC" ~ "Yes",
             TRUE ~ "No"
           ))
  
  
  ## Combine & Clean
  ny_combined <- bind_rows(ny_base_bil, ny_lead, ny_ec)
  
  ny_clean <- ny_combined %>%
    select(-any_of(c("dac", "cumulative_total", "code"))) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost),
    ) %>%
    # process text columns
    mutate(project_id = str_squish(project_number),
           community_served = str_squish(county),
           borrower = paste(str_squish(system_name), "/", str_squish(borrower)),
           project_score = str_squish(score),
           project_description = str_squish(description),
           state = "New York",
           state_fiscal_year = "2024",
           expecting_funding = if_else(str_detect(score, "above funding line"), "Yes", "No"),
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = as.character(NA),
    ) %>%
    # Implement Amendment 1
    mutate(
      project_cost = ifelse(project_id == "17629", "1800000", as.character(project_cost)),
      project_score = ifelse(project_id == "18854" & project_score == "40", "80", project_score)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  #Remove zero-cost projects
  ny_clean_nonzero <- ny_clean %>%
    filter(project_cost != "0" & project_cost != "0.0" & project_cost != "" & !is.na(project_cost))
  
  #Count removed projects
  num_removed <- nrow(ny_clean) - nrow(ny_clean_nonzero)
  
  #Comment about removed projects
  cat(paste0("# ", num_removed, " projects with zero or NA cost were removed.\n"))
  
  run_tests(ny_clean_nonzero)
  rm(list=setdiff(ls(), "ny_clean_nonzero"))
  
  return(ny_clean_nonzero)
}