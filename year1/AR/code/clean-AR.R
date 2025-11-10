clean_ar_y1 <- function() {

  # the manually merged data from MDI includes fundable projects with columns from the comprehensive list
  # this is a combination of charts 3 and 4 from the IUP - Phil
  # (15,21)
  ar_merge <- fread("year1/AR/data/4-Arkansas_Merged.csv",
                    colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ### COMPREHENSIVE
  # the comprehensive PPL includes applicant projects, (747,13)
  ar_comp <- fread("year1/AR/data/4-Arkansas.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  ar_app <- ar_comp %>%
    ## get rid of summary rows
    filter(!is.na(no)) %>%
    ## process numeric columns
    mutate(
      ## getting rid of $ and comma and replacing blanks with zero         
      population = clean_numeric_string(population),
      funding_amount = clean_numeric_string(project_cost),
    ) %>%
    mutate(
      # fix one-off NAs for pwsid
      pws_id = case_when(
        pws == "FORT SMITH WATER  UTILITIES" ~ "507",
        TRUE ~ pws_id
      ),
      pwsid = dplyr::case_when(
        no == "730" ~ "ART001101",
        no == "491" ~ "AR0000507",
        # pws_id is missing leading zeros, so standardize length to 9 characters based on length
        nchar(pws_id) == 1 ~ paste0("AR000000", pws_id), 
        nchar(pws_id) == 2 ~ paste0("AR00000", pws_id),
        nchar(pws_id) == 3 ~ paste0("AR0000", pws_id),
        nchar(pws_id) == 4 ~ paste0("AR000", pws_id)
      ),
      # project types are denoted by X's in their respective column
      project_type = case_when(
        lsl == "X" ~ "Lead",
        ec == "X" ~ "Emerging Contaminants",
        TRUE ~ "General"),
      # disadv column is either YES, NO, or NA by default
      disadvantaged = case_when(
        disadv_antaged_y_n == "YES" ~ "Yes",
        disadv_antaged_y_n == "NO" ~ "No",
        TRUE ~ "No Information"),
      borrower = str_squish(pws),
      project_cost = clean_numeric_string(project_cost),
      project_score = gsub(",", "", total_points),
      # all projects in ppl are applicants, see Merged for fundable projects
      expecting_funding = "No",
    ) %>%
    ## keep relevant columns
    select(no, borrower, pwsid, project_description, project_type, project_score, project_cost, 
           disadvantaged, population, expecting_funding)
  
  
  ### FUNDABLE
  ar_fund <- ar_merge %>% 
    select(-b_c_date_actual_estimated, -term_in_years, -interest_rate,
           -green_project_reserve_amt_estimate, -gpr_category_estimate,
           -mhi, -small_system_y_n) %>%
    # process numeric columns
    mutate(population = clean_numeric_string(population),
           project_cost = clean_numeric_string(project_cost),
           principal_forgiveness = clean_numeric_string(additional_subsidy)
    ) %>%
    # process text columns
    mutate(borrower = str_squish(pws),
           pwsid = case_when(
             nchar(pws_id) == 2 ~ paste0("AR00000", pws_id),
             nchar(pws_id) == 3 ~ paste0("AR0000", pws_id),
             nchar(pws_id) == 4 ~ paste0("AR000", pws_id)),
           project_description = str_squish(project_description),
           project_score = str_replace_all(total_points,"[^0-9.]", ""),
           project_type = case_when(lsl == "X" ~ "Lead",
                                    ec == "X" ~ "Emerging Contaminants",
                                    TRUE ~ "General"),
           disadvantaged = str_to_sentence(disadvantaged_community),
           expecting_funding = "Yes",
    ) %>%
    select(no, project_score, borrower, pwsid, project_description, 
           project_cost, principal_forgiveness, population, disadvantaged, project_type, expecting_funding) |>
    dplyr::group_by(no, borrower, pwsid, project_score, expecting_funding, disadvantaged, population, project_type ) |>
    dplyr::summarise(
      project_description = paste(unique(project_description), collapse = " | "),
      project_cost = as.character(sum(as.numeric(project_cost)),.groups = "drop"),
      principal_forgiveness = as.character(sum(as.numeric(principal_forgiveness)),.groups = "drop")
    ) |>
    dplyr::mutate(
      list = "fundable"
    )
  
  
  # expect (760,11) -> (760,13)
  ar_clean <- ar_app |>
    dplyr::full_join(ar_fund, by = join_by(no, pwsid, project_score)) |>
    #default to info on fundable list
    dplyr::mutate(
      borrower = borrower.y,
      borrower = ifelse(is.na(borrower), borrower.x, borrower),
      project_description = project_description.y,
      project_description = ifelse(is.na(project_description), project_description.x, project_description),
      project_type = project_type.y,
      project_type = ifelse(is.na(project_type), project_type.x, project_type),
      project_cost = project_cost.y,
      project_cost = ifelse(is.na(project_cost), project_cost.x, project_cost),
      expecting_funding = expecting_funding.y, 
      expecting_funding = ifelse(is.na(expecting_funding), expecting_funding.x,  expecting_funding),
      population = population.y,
      population = ifelse(is.na(population), population.x, population),
      disadvantaged = disadvantaged.y,
      disadvantaged = ifelse(is.na(disadvantaged), disadvantaged.x, disadvantaged)
    ) |>
    mutate(pwsid = replace_na(pwsid, "No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "0"), ##ASK
           disadvantaged = replace_na(disadvantaged, "No Information"), 
           community_served = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           funding_amount = as.character(NA),
           project_rank = as.character(NA),
           state = "Arkansas",
           state_fiscal_year = "2023") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
      ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ar_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No project_id
  
  # Check for disinfection byproduct in description
  ar_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No change, classified as expected
  
  # Check for lead subtypes
  ar_clean |>
    dplyr::filter(project_type=="Lead") |>
  dplyr::mutate(
    lead_type = dplyr::case_when(
    stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
    stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
    stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
   # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
   stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
   stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
    TRUE ~ "unknown"
  )) |>
    dplyr::filter(lead_type == "both")
  ####### Decision: No lead projects classified as both
  
  ####### SANITY CHECKS END #######
  
  run_tests(ar_clean)
  rm(list=setdiff(ls(), "ar_clean"))

  return(ar_clean)
}