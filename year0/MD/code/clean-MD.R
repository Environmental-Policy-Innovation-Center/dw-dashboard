clean_md_y0 <- function() {

  md_ppl_comprehensive <- data.table::fread("year0/MD/data/MD_Y0_SFY23_Base_Comprehensive_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY23 comprehensive"
    )
  
  #the fundable table was extracted with claude and some dollar amount columns with "-" were substituted with 0, contrast with raw tables for true 0s
  md_ppl_fundable <- data.table::fread("year0/MD/data/MD_Y0_SFY23_Base_Fundable_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY23 fundable",
      dwsrf_total_funding = clean_numeric_string(dwsrf_total_funding),
      expecting_funding = ifelse(dwsrf_total_funding == "0", "No Information", "Yes")
    ) |>
    dplyr::select(project_number, list, dwsrf_total_funding, expecting_funding, dwsrf_loan_forg)
  
  #sum(md_ppl_fundable$project_number %in% md_ppl_comprehensive$project_number)
  #project number '/23' suffix was removed from the raw data
  md_combined <- md_ppl_comprehensive |>
    dplyr::left_join(md_ppl_fundable, by= "project_number") |>
    dplyr::mutate(
      list = ifelse(is.na(list.y), list.x , list.y)
    ) |>
    dplyr::select(-list.x, -list.y)
  
  md_clean <- md_combined |>
    dplyr::mutate(
      community_served = stringr::str_squish(county),
      borrower = stringr::str_squish(applicant_name),
      borrower = stringr::str_remove(borrower, '^[^,]+,\\s*'),
      pwsid = stringr::str_squish(pwsid),
      pwsid = ifelse(pwsid=="MD", "No Information", pwsid),
      #noted by the policy analist that the project number should not be used as project id (as it is not unique)
      project_id = as.character(NA),
      project_name = stringr::str_squish(project_title),
      project_description = stringr::str_squish(project_description),
      #project details column do not have info for project type determination and hence was not used
      project_type =  case_when(
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl("lsl|lead", project_name, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        grepl(ec_str, project_name, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(total_cost),
      requested_amount = clean_numeric_string(requested_funding),
      funding_amount = clean_numeric_string(dwsrf_total_funding),
      funding_amount = ifelse(funding_amount == "0", "No Information", funding_amount),
      principal_forgiveness = clean_numeric_string(dwsrf_loan_forg),
      principal_forgiveness = ifelse(principal_forgiveness == "0", "No Information", principal_forgiveness ),
      population = benefited_population,
      population = ifelse(benefited_population=="?", "No Information", benefited_population),
      # Disadv was encoded as 1 and 0 for Yes and No, respectively
      disadvantaged = ifelse(disadvantaged == "1", "Yes", "No"),
      project_rank = stringr::str_squish(rank),
      project_score = stringr::str_squish(points),
      expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding),
      state = "Maryland",
      state_fiscal_year = "2023",
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
# md_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : No project_id

# Check for disinfection byproduct in description
# md_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string
  
# Check for lead subtypes: Both
# md_clean |>
#     dplyr::filter(project_type=="Lead") |>
#     dplyr::mutate(
#       lead_type = dplyr::case_when(
#         stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#         stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#         stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#         # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#         stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#         stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#         TRUE ~ "unknown"
#       )
#     ) |>
#     dplyr::filter(lead_type == "both")

####### Decision: No projects classified as both
  
# Check for lead subtypes: Unknown
# md_clean |>
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
  
######## Decision: No projects classified as unknown
  
####### SANITY CHECKS END #######
  
  run_tests(md_clean)

  rm(list=setdiff(ls(), "md_clean"))
  
  return(md_clean)
}