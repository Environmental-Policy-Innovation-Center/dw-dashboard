source("resources.R")

clean_ny <- function() {
  
  ## Base
  # project annual list, p33 of FFY 2023 Final IUP
  # (558,10) -> (558,12)
  ny_base <- fread("year1/NY/data/32-NewYork_base_ppl.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_type = "General")
  
  ## BIL
  # (120,10) -> (120,11)
  ny_bil <- fread("year1/NY/data/32-NewYork_bil_ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "General",
           disadvantaged = "Yes") %>%
    select(project_number, disadvantaged)
  

  # all projects on BIL are also on base
  ny_base_bil <- ny_base %>%
    left_join(ny_bil, by="project_number") %>%
    # from Base, if H, disadvantaged. if still NA, not disadvantaged, if it already wasn't NA, inhereit "Yes" from BIL
    mutate(disadvantaged = case_when(
      score == "H" & is.na(disadvantaged) ~ "Yes",
      is.na(disadvantaged) ~ "No",
      TRUE ~ disadvantaged)
      ) %>%
    # drop 4 projects already on EC/lead list
    filter(!project_number %in% c("18971", "19125", "19171", "19277"))
  
  
  ## Lead - Amendment 4
  # (109,11) -> (109,12)
  ny_lead <- fread("year1/NY/data/32-NewYork_lead.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "Lead",
           disadvantaged = case_when(
             dac == "DAC" ~ "Yes",
             TRUE ~ "No"
           ))
  
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
  
  ## Combine & Clean
  # -> (700, 13)
  ny_combined <- bind_rows(ny_base_bil, ny_lead, ny_ec)
  
  # -> (700,12)
  ny_clean <- ny_combined %>%
    select(-dac, -cumulative_total, -code) %>%
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
           state_fiscal_year = "2023",
           expecting_funding = "Not Funded",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_rank = as.character(NA),
           ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}