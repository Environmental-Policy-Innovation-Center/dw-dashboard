source("resources.R")

clean_ny <- function() {
  
  ## Multi-year list
  ny_multi <- fread("year1/NY/data/ny-multi-year-list.csv",
                    colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_number = str_squish(project),
           community_served = str_squish(county),
           project_description = str_squish(description),
           population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost),
           project_score = clean_numeric_string(score)
           ) %>%
    select(project_number, community_served, system_name_borrower, project_description, 
           population, project_cost, project_score, description)
  
  
  
  
  # project annual list, p33 of FFY 2023 Final IUP
  # (558,10) -> (558,12)
  ny_base <- fread("year1/NY/data/32-NewYork_base_ppl.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General",
           # projects are expecting funding if they are above the Expanded Subsidized Interest Rate Funding Line
           expecting_funding = ifelse(row_number() <= 148, "Yes", as.character(NA)))
  
  ## BIL
  # (120,10) -> (120,11)
  ny_bil <- fread("year1/NY/data/32-NewYork_bil_ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "General",
           # projects are dacs if on the bil list
           disadvantaged = "Yes") %>%
    select(project_number, disadvantaged)
  

  # all projects on BIL are also on base
  ny_base_bil <- ny_base %>%
    left_join(ny_bil, by="project_number") %>%
    # from Base, if H, disadvantaged. if still NA, not disadvantaged, if it already wasn't NA, inherit "Yes" from BIL
    mutate(disadvantaged = case_when(
      # if NA from bil list and "H", DAC, otherwise not
      score == "H" & is.na(disadvantaged) ~ "Yes",
      is.na(disadvantaged) ~ "No",
      TRUE ~ disadvantaged)
      ) %>%
    # drop 4 projects already on EC/lead list
    filter(!project_number %in% c("18971", "19125", "19171", "19277"))
  
  
  ny_bil_awards <- fread("year1/NY/data/ny-bil-gen-supp-awards.csv",
                         colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(expecting_funding = "Yes",
           disadvantaged = "Yes") %>%
    select(project_number, expecting_funding, disadvantaged)
  
  # 554
  ny_bil_combined <- ny_base_bil %>%
    left_join(ny_bil_awards, by=c("project_number", "expecting_funding", "disadvantaged"))
  
  
  ## Lead - Amendment 4
  # (109,11) -> (109,12)
  ny_lslr <- fread("year1/NY/data/32-NewYork_lead.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "Lead",
           disadvantaged = case_when(
             #NOTE: dac column extracted in scraping process
             dac == "DAC" ~ "Yes",
             TRUE ~ "No"
           ))
  
  ## Lead Funding List
  ny_lslr_awards <- fread("year1/NY/data/ny-lslr-awards.csv",
                          colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(funding_amount = clean_numeric_string(total_bil_lslr_funding_award),
           principal_forgiveness = clean_numeric_string(bil_lslr_grant_award),
           expecting_funding = "Yes") %>%
    select(project_number, funding_amount, principal_forgiveness, expecting_funding)
  
  # 109
  ny_lslr_combined <- ny_lslr %>%
    left_join(ny_lslr_awards, by="project_number")
  
  
  ## Emerging Contaminants - Amendment 2
  # (37,10) -> (37,11)
  ny_ec <- fread("year1/NY/data/32-NewYork_ec.csv",
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-pfas) %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = case_when(
             dac == "DAC" ~ "Yes",
             TRUE ~ "No"),
           population = case_when(
             # fix projects that are missing population from NY Base
             project_number == "19171" ~ "14700",
             project_number == "18971" ~ "28000",
             project_number == "19125" ~ "621"
           )
    )
  
  ## Emerging Contaminants - Funding List
  ny_ec_awards <- fread("year1/NY/data/ny-ec-awards.csv",
                        colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(funding_amount = clean_numeric_string(bil_ec_grant_award),
           principal_forgiveness = clean_numeric_string(bil_ec_grant_award),
           expecting_funding = "Yes",
           ) %>%
    select(project_number, funding_amount, principal_forgiveness, expecting_funding)
  
  # 37
  ny_ec_combined <- ny_ec %>%
    left_join(ny_ec_awards, by="project_number")
  
  ## Combine & Clean
  # -> (700, 13)
  ny_combined <- bind_rows(ny_bil_combined, ny_lslr_combined, ny_ec_combined) %>%
    mutate(system_name_borrower = paste0(system_name, " / ", borrower)) %>%
    select(project_number, system_name_borrower, project_type, expecting_funding, 
           disadvantaged, funding_amount, principal_forgiveness, pop, project_cost, 
           county, score, description)
  
  
  ny_all <- merge(ny_combined, ny_multi, all=TRUE, on="project_number") %>%
    select(project_number) %>%
    distinct()
  
  
  ny_all <- ny_all %>%
    left_join(ny_multi, by="project_number") %>%
    left_join(ny_combined, by="project_number") %>%
    # drop columns that didn't match but contain redundant information, fixing and dropping one by one to clean up the dataframe
   
     mutate(
      population = case_when(
        is.na(population) ~ clean_numeric_string(pop),
        TRUE ~ population),
    ) %>%
    select(-pop) %>%
    
    mutate(
      project_cost = case_when(
        is.na(project_cost.x) ~ clean_numeric_string(project_cost.y),
        is.na(project_cost.y) ~ project_cost.x,
        TRUE ~ clean_numeric_string(project_cost.y))
    ) %>%
    select(-project_cost.x, -project_cost.y) %>%
    
    mutate(project_description = case_when(
      is.na(project_description) & is.na(description.x) ~ str_squish(description.y),
      is.na(project_description) ~ str_squish(description.x),
      TRUE ~ str_squish(project_description)
    )) %>%
    select(-description.x, -description.y) %>%
    
    mutate(community_served = case_when(
      is.na(community_served) ~ str_squish(county),
      TRUE ~ community_served
    )) %>%
    select(-county) %>%
    
    mutate(project_score = case_when(
      is.na(project_score) ~ score,
      TRUE ~ project_score
    )) %>%
    select(-score) %>%
    
    mutate(borrower = case_when(
      is.na(system_name_borrower.y) ~ system_name_borrower.x,
      TRUE ~ system_name_borrower.y
    )) %>%
    select(-system_name_borrower.x, -system_name_borrower.y) %>%
    
    mutate(project_type = case_when(
      project_type == "General" | project_type == "Lead" | project_type == "Emerging Contaminants" ~ project_type,
      is.na(project_type) & grepl("lead", project_description, ignore.case=TRUE) ~ "Lead",
      is.na(project_type) & (grepl("PFAS", project_description, ignore.case=TRUE) | grepl("dioxane", project_description, ignore.case=TRUE) | 
        grepl("cyanotoxins", project_description, ignore.case=TRUE) | grepl("Mn", project_description, ignore.case=FALSE)) ~ "Emerging Contaminants",
      TRUE ~ "General")
           )
    
  ny_clean <- ny_all %>%
    # process text columns  
    mutate(project_id = str_squish(project_number),
           project_description = str_replace_all(project_description, " \\. , ", " "),
           project_description = str_replace_all(project_description, " , ", ", "),
           project_description = str_replace_all(project_description, " \\. ", "\\. "),
           project_description = str_replace_all(project_description, " \\( ", " \\("),
           project_description = str_replace_all(project_description, " \\) ", "\\) "),
           project_description = str_replace_all(project_description, " \\)", "\\)"),
           state = "New York",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
           funding_amount = replace_na(funding_amount, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           disadvantaged = replace_na(disadvantaged, "No Information")
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}