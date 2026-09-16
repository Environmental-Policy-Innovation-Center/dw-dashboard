clean_wi_y2 <- function() {

    wi_comp <- fread("year2/WI/data/wi-sfy24-comp-list.csv",
                          colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
      dplyr::mutate(list = "SFY24 Comprehensive List")
    
    
    wi_not_fundable <- fread("year2/WI/data/wi-sfy24-not-fundable-list.csv",
                             colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
      dplyr::mutate(list = "SFY24 Not Fundable List",
             expecting_funding = "No")
    
    # join comp and not funding list to merge onto
    wi_comp_all <- bind_rows(wi_comp, wi_not_fundable) |>
      dplyr::mutate(project_cost = clean_numeric_string(estimated_project_cost),
             project_score = stringr::str_squish(self_score1),
             community_served = stringr::str_to_title(municipality),
             project_id = stringr::str_squish(project_number),
             project_id = normalize_dashes(project_id)) |>
      dplyr::select(project_score, community_served, project_id, project_description, project_cost, list, expecting_funding)
    
    
    # process all columns for projects that arent on LSL/EC lists
    wi_comp_fundable <- fread("year2/WI/data/wi-sfy24-comp-funding-list.csv",
                              colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
      dplyr::mutate(list = "SFY24 Comprehensive Fundable List",
             project_type = case_when(
               program == "BASE" ~ "General",
               program == "LSL" ~ "Lead",
               program == "EC" ~ "Emerging Contaminants",
               grepl("(EC)", project_description) ~ "Emerging Contaminants",
               TRUE ~ "General"),
             community_served = stringr::str_to_title(municipality),
             project_description = stringr::str_squish(project_description),
             project_id = stringr::str_squish(project_number),
             project_id = normalize_dashes(project_id),
             requested_amount = clean_numeric_string(requested_project_costs),
             population = clean_numeric_string(population),
             project_score = stringr::str_squish(priority_score),
             estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
             principal_forgiveness = convert_to_numeric(total_estimated_pf, T),
             funding_amount = estimated_loan_amount + principal_forgiveness,
             disadvantaged = case_when(
               program %in% c("BASE", "EC") & as.numeric(total_pf_points) >= 60 ~ "Yes",
               program == "LSL" & as.numeric(total_pf_points) >= 40 ~ "Yes",
             TRUE ~ "No")
             ) |>
      dplyr::select(project_id, project_score, community_served, project_description, population, project_type, requested_amount, 
             funding_amount, principal_forgiveness, disadvantaged, list)
    
    
    wi_lsl_fundable <- fread("year2/WI/data/wi-sfy24-lsl-fundable-list.csv",
                             colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
      dplyr::mutate(list = "SFY24 LSL Fundable List",
             project_type = "Lead",
             project_id = stringr::str_squish(project_number),
             project_id = normalize_dashes(project_id),
             requested_amount = clean_numeric_string(total_requested_costs_in_application),
             estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
             principal_forgiveness = convert_to_numeric(total_pf_awarded, T),
             funding_amount = estimated_loan_amount + principal_forgiveness,
             project_description = stringr::str_squish(project_description),
             project_score = stringr::str_squish(lsl_priority_score3),
             population = "No Information"
             ) |>
      dplyr::select(project_id, project_score, project_type, requested_amount, funding_amount, principal_forgiveness, project_description, population, list)
    
    
    # for all lead projects, defer to lead-table data, otherwise backfill with comp fundable list data
    wi_fundable <- wi_comp_fundable |>
      left_join(wi_lsl_fundable, by="project_id") |>
      dplyr::mutate(
        project_description = ifelse(!is.na(project_description.y), project_description.y, project_description.x),
        project_score = ifelse(!is.na(project_score.y), project_score.y, project_score.x),
        project_type = ifelse(!is.na(project_type.y), project_type.y, project_type.x),
        requested_amount = ifelse(!is.na(requested_amount.y), requested_amount.y, requested_amount.x),
        funding_amount = ifelse(!is.na(funding_amount.y), funding_amount.y, funding_amount.x),
        principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x),
        list = ifelse(!is.na(list.y), list.y, list.x),
        population = coalesce(population.y, population.x)
             ) |>
      dplyr::select(project_id, project_score, community_served, project_description, population, 
             project_type, requested_amount, funding_amount, principal_forgiveness, disadvantaged, list)
    
    
    wi_ec_fundable <- fread("year2/WI/data/wi-sfy24-ec-fundable-list.csv",
                            colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
      dplyr::mutate(list = "SFY24 EC Fundable List",
             project_type = "Emerging Contaminants",
             project_id = stringr::str_squish(project_number),
             project_id = normalize_dashes(project_id),
             project_score = stringr::str_squish(priority_score),
             requested_amount = clean_numeric_string(requested_project_costs),
             estimated_loan_amount = convert_to_numeric(estimated_loan_amount, T),
             principal_forgiveness = convert_to_numeric(total_estimated_pf, T),
             funding_amount = estimated_loan_amount + principal_forgiveness,
             project_description = stringr::str_squish(project_description)
      ) |>
      dplyr::select(project_id, project_score, project_type, requested_amount, funding_amount, principal_forgiveness, project_description, list)
    
    
    # repeat above process for EC to maintain all EC-specific data, fill in with comp list otherwise
    wi_fundable <- wi_fundable |>
      left_join(wi_ec_fundable, by="project_id") |>
      dplyr::mutate(project_description = ifelse(!is.na(project_description.y), project_description.y, project_description.x),
             project_score = ifelse(!is.na(project_score.y), project_score.y, project_score.x),
             project_type = ifelse(!is.na(project_type.y), project_type.y, project_type.x),
             requested_amount = ifelse(!is.na(requested_amount.y), requested_amount.y, requested_amount.x),
             funding_amount = ifelse(!is.na(funding_amount.y), funding_amount.y, funding_amount.x),
             principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x),
             list = ifelse(!is.na(list.y), list.y, list.x),
             # for all projects on these lists, expecting funding
             expecting_funding = "Yes"
      ) |>
      dplyr::select(project_id, project_score, community_served, project_description, population, 
             project_type, requested_amount, funding_amount, principal_forgiveness, disadvantaged, expecting_funding, list)
    
    # merge funding list onto comp list, keeping projects only on comp funding list
    wi_all <- merge(wi_comp_all, wi_fundable, by="project_id", all=TRUE) |>
      dplyr::mutate(
        project_description = ifelse(!is.na(project_description.y), project_description.y, project_description.x),
        project_score = ifelse(!is.na(project_score.y), project_score.y, project_score.x),
        list = ifelse(!is.na(list.y), list.y, list.x),
        community_served = ifelse(!is.na(community_served.y), community_served.y, community_served.x),
        expecting_funding = ifelse(!is.na(expecting_funding.y), expecting_funding.y, expecting_funding.x),
      ) |>
      dplyr::select(project_id, project_score, community_served, project_description, project_cost, population, project_type,
             requested_amount, funding_amount, principal_forgiveness, disadvantaged, expecting_funding, list)
    
   
    wi_clean <- wi_all |>
      dplyr::mutate(
        borrower = as.character(NA),
        pwsid = as.character(NA),
        project_name = as.character(NA),
        project_rank = as.character(NA),
        project_type = case_when(
          !is.na(project_type) ~ project_type,
          grepl(ec_str, project_description, ignore.case=T) ~ "Emerging Contaminants",
          grepl("(EC)", project_description) ~ "Emerging Contaminants",
          grepl("lsl|lead", project_description, ignore.case=T) ~ "Lead",
          TRUE ~ "General"),
        # project score NA for projects on Not Fundable list
        project_score = replace_na(project_score, "No Information"),
        # project cost NA for projects not on comp list, but on fundable list
        project_cost = replace_na(project_cost, "No Information"),
        project_cost = ifelse(
          list %in% c("SFY24 Comprehensive Fundable List", "SFY24 LSL Fundable List", "SFY24 EC Fundable List"),
          "No Information",
          project_cost
        ),
        # numerous columns NA for projects not on comp funding list
        population = replace_na(population, "No Information"),
        requested_amount = replace_na(requested_amount, "No Information"),
        disadvantaged = replace_na(disadvantaged, "No Information"),
        funding_amount = clean_numeric_string(funding_amount),
        principal_forgiveness = clean_numeric_string(principal_forgiveness),
        # all projects not Yes from comp funding list No
        expecting_funding = replace_na(expecting_funding, "No"),
        state = "Wisconsin",
        state_fiscal_year = "2024"
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
####### Decision: No disinfection byproduct string

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

  ### 95 lead unknown --> resolved to LSLR
  
  wi_clean <- wi_clean |>
    dplyr::mutate(
      project_description = ifelse(
       project_type == "Lead" & project_id %in% c("4754-14", "4755-05", "4759-28", "4762-05", "4764-12", "4783-04", "4790-05", "4796-05", "4800-08", "4803-07", "4814-04", "4824-06", "4825-07", "4825-08", "4828-11", "4835-07", "4850-04",
      "4851-43", "4851-45", "4852-18", "4861-11", "4863-15", "4874-15", "4881-10", "4883-04", "4887-16", "4888-09", "4893-08", "4901-11", "4907-05", "4907-07", "4911-09", "4920-49", "4922-18",
      "4929-08", "4930-14", "4936-03", "4947-03", "5054-07", "5091-16", "5119-08", "5120-10", "5120-14", "5120-15", "5122-09", "5142-15", "5157-09", "5161-03", "5163-11", "5168-13", "5188-05",
      "5191-16", "5191-17", "5191-18", "5198-04", "5214-13", "5239-06", "5244-05", "5289-18", "5331-31", "5364-10", "5369-20", "5404-09", "5439-06", "5444-03", "5449-10", "5452-06", "5452-08",
      "5452-09", "5463-07", "5471-05", "5473-10", "5508-05", "5525-05", "5536-06", "5555-06", "5557-03", "5558-04", "5563-05", "5564-04", "5565-04", "5576-06", "5607-05", "5621-04", "5623-07",
      "5626-02", "5628-02", "5629-02", "5630-02", "5648-01", "5650-01", "5652-01", "5654-01", "5656-02", "5658-01"),
      paste0(project_description, " | FT: LSLR"),
      project_description
      )
    )

####### SANITY CHECKS END #######
  
  run_tests(wi_clean)
  return(wi_clean)
}


