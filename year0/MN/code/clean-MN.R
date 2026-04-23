clean_mn_year_0 <- function() {
  
  # comp ppl contains all of not eligible and fundable projects
  mn_comp_ppl <- data.table::fread("year0/MN/data/mn-comp-ppl.csv",
                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(population != "GRAND TOTAL") |>
    dplyr::mutate(borrower = stringr::str_squish(system),
           project_id = stringr::str_squish(project_id),
           project_id = stringr::str_replace(project_id, "-", "‐"),
           project_description = stringr::str_squish(project),
           project_rank = stringr::str_squish(rank),
           project_score = stringr::str_squish(points),
           )
  
  mn_not_eligible <- data.table::fread("year0/MN/data/mn-not-eligible.csv",
                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::mutate(project_id = stringr::str_squish(project_number),
           project_id = stringr::str_replace(project_id, "-", "‐"),
           estimated_project_cost = clean_numeric_string(estimated_project_cost),
           list_name = "SFY22 Not Eligible List",
           ) |>
    dplyr::select(project_id, estimated_project_cost, list_name)
  
  
  mn_fundable <- data.table::fread("year0/MN/data/mn-fundable.csv",
                       colClasses = "character", na.strings = "") |> 
    janitor::clean_names() |>
    dplyr::filter(!is.na(project_description)) |>
    dplyr::mutate(project_id = str_squish(project_number),
           # remove outlier hyphen character, replace with hyphen found in other tables
           project_id = stringr::str_replace(project_id, "-", "‐"),
           estimated_project_cost = clean_numeric_string(estimated_project_cost),
           expecting_funding = 'Yes',
           # extract PF, but use the original column for calculations with no "No Information" introduced
           principal_forgiveness = clean_numeric_string(est_dwrf_principal_forgiveness_not_final_1),
           principal_forgiveness_amount = convert_to_numeric(est_dwrf_principal_forgiveness_not_final_1, fill_na_0 = TRUE),
           estimated_wif_grant = convert_to_numeric(estimated_wif_grant_not_final_2, TRUE),
           estimated_dwrf_loan = convert_to_numeric(estimated_dwrf_loan, fill_na_0 = TRUE),
           funding_amount = principal_forgiveness_amount + estimated_dwrf_loan,
           funding_amount = clean_numeric_string(funding_amount),
           disadvantaged = case_when(
                                     # any project with PF is yes
                                     principal_forgiveness_amount > 0 ~ "Yes",
                                     # projects on part A with WIF grant are yes
                                     status == "Carryover" & estimated_wif_grant > 0 ~ "Yes",
                                     # projects on part A1-A3 without PF is no
                                     status == "Carryover" & principal_forgiveness_amount == 0 & estimated_wif_grant == 0 ~ "No",
                                     # all else is No Info
                                     TRUE ~ "No Information"),
           list_name = "SFY22 Fundable List"
    ) |>
    dplyr::select(project_id, estimated_project_cost, expecting_funding, funding_amount, principal_forgiveness, disadvantaged, list_name)
  
  
  mn_clean <- mn_comp_ppl |>
    dplyr::left_join(mn_not_eligible, by=c('project_id')) |>
    dplyr::left_join(mn_fundable, by=c('project_id')) |>
    dplyr::mutate(community_served = as.character(NA),
           borrower = stringr::str_squish(system),
           pwsid = paste0("MN", str_remove(project_id, "‐.*")),
           project_name = as.character(NA),
           project_type = dplyr::case_when(
             grepl("lead|lsl", project_description, ignore.case = TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # first combined estimated project cost from non-eligible and fundable, then check against original project cost
           estimated_project_cost = dplyr::case_when(is.na(estimated_project_cost.x) ~ estimated_project_cost.y,
                                              TRUE ~ estimated_project_cost.x),
           project_cost = clean_numeric_string(project_cost),
           project_cost = dplyr::case_when(is.na(estimated_project_cost) ~ project_cost,
                                    TRUE ~ estimated_project_cost),
           requested_amount = as.character(NA),
           funding_amount = tidyr::replace_na(funding_amount, "No Information"),
           principal_forgiveness = tidyr::replace_na(principal_forgiveness, "No Information"),
           population = clean_numeric_string(population),
           project_description = stringr::str_squish(project_description),
           disadvantaged = tidyr::replace_na(disadvantaged, "No Information"),
           project_rank = stringr::str_squish(project_rank),
           project_score = stringr::str_squish(points),
           expecting_funding = ifelse(is.na(expecting_funding), "No", "Yes"),
           state = "Minnesota",
           state_fiscal_year = "2022",
           list_name = case_when(is.na(list_name.x) ~ list_name.y,
                     TRUE ~ list_name.x),
           list = tidyr::replace_na(list_name, "SFY22 Comprehensive List")
           )  |>  
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
         requested_amount, funding_amount, principal_forgiveness, population, project_description,
         disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  run_tests(mn_clean)
  rm(list=setdiff(ls(), "mn_clean"))
  
}


####### SANITY CHECKS START #######

# Hone in on project id duplication
# mn_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: No duplicates

# Check for disinfection byproduct in description
# mn_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
####### Decision: No change, classified as expected

# Check for lead subtypes
# mn_clean |>
#   dplyr::filter(project_type=="Lead") |>
# dplyr::mutate(
#   lead_type = dplyr::case_when(
#   stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#  # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#  stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#  stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#   TRUE ~ "unknown"
# )) |>
#   dplyr::filter(lead_type == "both")
# ####### Decision: No lead projects classified as both

####### SANITY CHECKS END #######