clean_mt_y1 <- function() {
  
 
  ## FUNDABLE
  # (37,4)
  # Table 1. DWSRF Projects Anticipated to Receive Funding SFY 2023 from 
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  mt_ppl_fund <- fread("year1/MT/data/montana-ppl-table1.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    rename(rank_no = priority_rank)
  
  
  ## APPLICANT
  # Appendix 2: DWSRF COMPREHENSIVE PROJECT LIST—SFY 2023 from
  # https://deq.mt.gov/files/Water/TFAB/DWSRF/IUP-PPL/2023_DWSRF_%20IUP_FINAL.pdf
  mt_ppl_app <- fread("year1/MT/data/montana-ppl-appendix2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(project_type = "General",
           project_score = str_squish(total_points),
           borrower = str_squish(project_name))
  
  # combine tables from the general PPL
  mt_ppl <- mt_ppl_app %>%
    left_join(mt_ppl_fund, by="rank_no") %>%
    mutate(project_description = 
             case_when(
              is.na(project_information) ~ description,
              TRUE ~ gsub("^.*?\\.","", project_information)),
      project_description = str_squish(project_description),
    ) %>%
    select(-description, -project_information, -total_points, -project_name)
  
  
  ## LEAD FUNDALBE
  # Table 1
  mt_lsl_fund <- fread("year1/MT/data/montana-lsl-table1.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names()  %>%
    mutate(rank_no = case_when(
      priority_rank == "6" ~ "5",
      TRUE ~ priority_rank))
  
  ## LEAD APPLICANT
  # Appendix 2
  mt_lsl_app <- fread("year1/MT/data/montana-lsl-appendix2.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           borrower = str_squish(system_name),
           ) %>%
    rename(description = project_description,
           project_score = ranking)
  
  # combined lead tables
  mt_lsl <- mt_lsl_app %>%
    left_join(mt_lsl_fund, by="rank_no") %>%
    mutate(project_description = 
             case_when(
               is.na(project_information) ~ description,
               TRUE ~ project_information),
           project_description = str_squish(project_description)
    ) %>%
    select(-project, -priority_rank, -system_name, -description, -project_information)

  
  ## EC FUNDABLE
  # Table 1
  mt_ec_fund <- fread("year1/MT/data/montana-ec-table1.csv",
                      colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    rename(rank_no = priority_rank)
  
  ## EC APPLICANT
  # Appendix 2
  mt_ec_app <- fread("year1/MT/data/montana-ec-appendix2.csv",
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants") %>%
    rename(borrower = system_name,
           description = project_description,
           state_score = ranking)
  
  # combine EC tables
  mt_ec <- mt_ec_app %>%
    left_join(mt_ec_fund, by="rank_no") %>%
    mutate(project_description = 
             case_when(
               is.na(project_information) ~ description,
               TRUE ~ as.character(map(strsplit(project_information, split = "\\."), 1))),
           project_description = str_squish(project_description)) %>%
    select(-project, -description, -project_information)
  
  
  # combine all projects
  mt_clean <- bind_rows(mt_ppl, mt_lsl, mt_ec) %>%
    mutate(population = clean_numeric_string(population),
           # remove added details in srf cost and transform into numeric for both cost and funding
           project_cost = as.character(map(strsplit(srf_cost, split = " "), 1)),
           project_cost = convert_to_numeric(project_cost, TRUE),
           funding_amount = project_cost,
           requested_amount = clean_numeric_string(amount),
           ) %>%
    mutate(project_rank = str_squish(rank_no),
           disadvantaged = case_when(
             grepl("P", srf_cost) ~ "Yes",
             # both EC funded projects are Disadvantaged from the project_information column
             project_type == "Emerging Contaminants" & funding_amount > 0 ~ "Yes",
             project_type == "Emerging Contaminants" & funding_amount == 0 ~ "No Information",
             TRUE ~ "No"),
           expecting_funding = case_when(
             funding_amount != "No Information" ~ "Yes",
             TRUE ~ "No"),
           state="Montana",
           state_fiscal_year = "2023"
           ) %>%
    mutate(
    # calculate PF as percentage of funding now that DAC is defined
    principal_forgiveness = case_when(
      # both EC projects get PF up to total of 7.555m. First gets fully funded, second gets the remaining PF available.
      project_type == "Emerging Contaminants" & funding_amount == 3000000 ~ 3000000,
      project_type == "Emerging Contaminants" & funding_amount == 5383000 ~ 4555000,
      # PF for lead and general is % of project cost, up to a max. first calculate the percentage to avoid issues with NAs
      project_type == "Lead" & funding_amount > 0 & disadvantaged == "Yes" ~ funding_amount * .6,
      project_type == "General" & funding_amount > 0 & disadvantaged == "Yes" ~ funding_amount*.75),
  # for lead and general projects, check PF against the allowed cap
  principal_forgiveness = case_when(
    project_type == "Lead" & principal_forgiveness > 2000000 ~ 2000000,
    project_type == "General" & principal_forgiveness > 750000 ~ 750000,
    TRUE ~ principal_forgiveness),
  funding_amount = clean_numeric_string(funding_amount),
  funding_amount = case_when(funding_amount == "0" ~ "No Information", TRUE ~ funding_amount),
  project_cost = clean_numeric_string(project_cost),
  project_cost = case_when(project_cost == "0" ~ "No Information", TRUE ~ project_cost),
  principal_forgiveness = clean_numeric_string(principal_forgiveness),
  principal_forgiveness = case_when(principal_forgiveness == "0" ~ "No Information", TRUE ~ principal_forgiveness),
  community_served = as.character(NA),
  pwsid = as.character(NA),
  project_id = as.character(NA),
  project_name = as.character(NA),
  project_score = replace_na(project_score, "No Information")
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

  run_tests(mt_clean)
  rm(list=setdiff(ls(), "mt_clean"))
  
  return(mt_clean)
}