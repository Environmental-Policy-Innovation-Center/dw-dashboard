clean_wi_y1 <- function() {
  
  wi_comp_list <-  fread("year1/WI/data/wi-sfy23-comp-list.csv",
                         colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = ifelse(grepl("UNDETERMINED", project_description), "SFY23 Not Fundable List", "SFY23 Comprehensive List"),
           ) |>
    unique()
  
  # most but not all gen-lsl projects are on comp list, so it is joined, others are binded on
  wi_gen_lsl_fund <-  fread("year1/WI/data/wi-sfy23-gen-lsl-fundable-list.csv",
                            colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY23 General and LSL Fundable List",
           disadvantaged = ifelse(as.numeric(total_pf_points) >= 60, "Yes", "No"),
           expecting_funding = "Yes",
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
           principal_forgiveness = convert_to_numeric(pf_estimate, T),
           funding_amount = estimated_loan_amount + principal_forgiveness,
           population = clean_numeric_string(population)) |>
    dplyr::rename(self_score = priority_score)
  
  # funded projects not on comp list
  wi_gen_lsl_fund_extra <- wi_gen_lsl_fund |>
    dplyr::filter(project_number %in% c("4920-48", "5369-18", "5436-07", "4768-02", "5545-04"))
  
  
  # after extracting funding projects not on comp list, subset to columns needed to join onto comp list for the rest
  wi_gen_lsl_fund <- wi_gen_lsl_fund |>
    dplyr::select(project_number, list, requested_amount, expecting_funding, principal_forgiveness, funding_amount, disadvantaged, population, self_score)

  
  # none of the EC projects are on Comp list, need to be added on like some of Gen-LSL list
  wi_ec_fund <- fread("year1/WI/data/49-Wisconsin_EC_Funding_List.csv",
                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Emerging Contaminants",
           list = "SFY23 EC Fundable List",
           disadvantaged = "No Information",
           expecting_funding = "Yes",
           requested_amount = clean_numeric_string(requested_project_costs),
           estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
           principal_forgiveness = convert_to_numeric(pf_estimate, T),
           funding_amount = estimated_loan_amount + principal_forgiveness) |>
    dplyr::rename(self_score = priority_score)
  

  # combine comp and funding projects
  wi_all <- wi_comp_list |>
    dplyr::left_join(wi_gen_lsl_fund, by="project_number") |>
    dplyr::mutate(
      self_score = coalesce(self_score.y, self_score.x),
      list = ifelse(!is.na(list.y), list.y, list.x)) |>
    dplyr::select(-c(self_score.y, self_score.x, list.x, list.y))
  
  wi_all <- bind_rows(wi_all, wi_gen_lsl_fund_extra, wi_ec_fund)
  
  wi_clean <- wi_all |>
    dplyr::mutate(
      community_served = stringr::str_to_title(municipality),
      borrower = as.character(NA),
      pwsid = as.character(NA),
      project_id = stringr::str_squish(project_number),
      project_name = as.character(NA),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl(ec_str, project_description) ~ "Emerging Contaminants",
        grepl("lead|lsl", project_description, ignore.case=T) ~ "Lead",
        TRUE ~ "General"
      ),
      project_score = clean_numeric_string(self_score),
      project_rank = as.character(NA),
      project_cost = clean_numeric_string(estimated_project_cost),
      project_cost = ifelse(
        list %in% c("SFY23 General and LSL Fundable List", "SFY23 EC Fundable List"),
        "No Information",
        project_cost
      ),
      disadvantaged = replace_na(disadvantaged, "No Information"),
      expecting_funding = replace_na(expecting_funding, "No"),
      funding_amount = clean_numeric_string(funding_amount),
      requested_amount = clean_numeric_string(requested_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = clean_numeric_string(population),
      state = "Wisconsin",
      state_fiscal_year = "2023"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
# wi_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: No duplicates

# Check for disinfection byproduct in description
# wi_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: ####### Decision: One duplicate, sourced back to original PDF where state lists project twice in Comp List PPL, addressed with unique above

# Check for lead subtypes
# wi_clean |>
#   dplyr::filter(project_type=="Lead") |>
#   dplyr::mutate(
#     lead_type = dplyr::case_when(
#       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#       TRUE ~ "unknown"
#     )) |>
#   dplyr::filter(lead_type == "both")
# ####### Decision: No lead projects classified as both
  
 # Check for lead subtypes: Unknown
  # wi_clean |>
  #   dplyr::filter(project_type=="Lead") |>
  #   dplyr::mutate(
  #     lead_type = dplyr::case_when(
  #       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
  #       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
  #       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
  #       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
  #       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
  #       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
  #       TRUE ~ "unknown"
  #     )
  #   ) |>
  #   dplyr::filter(lead_type == "unknown") 

  ### 11 lead unknown --> LSLR

####### SANITY CHECKS END #######
  
  wi_clean <- wi_clean |>
    dplyr::mutate(
      project_description = ifelse(
        project_id %in% c("5443-10", "5366-06", "5366-07", "5316-06", "4920-38", "4920-42", "4851-42", "4852-18", "5564-03", "4824-05", "4920-48"),
        paste0(project_description, " | FT: LSLR"),
        project_description
      )
    )
    
  run_tests(wi_clean)
  rm(list=setdiff(ls(), "wi_clean"))
  
  return(wi_clean)
}


