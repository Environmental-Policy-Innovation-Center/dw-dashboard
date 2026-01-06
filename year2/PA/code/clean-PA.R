clean_pa_y2 <- function() {
  
  # NOTE: because PA does not provide a project ID and the applicant 
  # name changes across lists in unpredictable ways, an epic_project_id
  # was created to match projects on the fundable list to the comprehensive 
  # list 
  
  pa_fundable <- fread("year2/PA/data/pa-fundable-ppl.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # this removes 3 projects that are the PENNVEST / pending projects 
    filter(!is.na(epic_merge_id)) %>%
    # all of these are expecting funding 
    mutate(expecting_funding = "Yes", 
           funding_amount = clean_numeric_string(total_assistance_amount)) 
  
  # comp list: note there was some light tidying of borrower column manually 
  # (i.e., adding "-" between borrower and project)
  pa_comp <- fread("year2/PA/data/pa-base-ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(community_served = paste0(str_to_title(city), ", ", 
                                     str_to_title(county), " County"), 
           requested_amount = as.character(NA), 
           population = clean_numeric_string(population), 
           project_rank = clean_numeric_string(projrank), 
           project_score = clean_numeric_string(dep_project_rating),
           pwsid = paste0("PA", trimws(pwsid)), 
           project_description = paste0("Project Description: ", proj_description, 
                                        "; Problem Description: ", prob_description)) 
  # merging by project id: 
  pa_clean <- merge(pa_comp, pa_fundable, by = "epic_merge_id", all = T) %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information"), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           funding_amount = clean_numeric_string(funding_amount), 
           expecting_funding = replace_na(expecting_funding, "No"),
           project_id = clean_numeric_string(loan_number), 
           project_type = case_when(grepl(lead_str, project_description, ignore.case = T) ~ "Lead", 
                                    grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
                                    # the presence of a project on a list should 
                                    # override the string matches above
                                    list == "lead_fundable" ~ "Lead", 
                                    list == "ec_fundable" ~ "Emerging Contaminants", 
                                    TRUE ~ "General"),
           state = "Pennsylvania",
           state_fiscal_year = "2024") %>%
    # handling the applicant --> project name and borrower columns: 
    # extract based on presence of hyphen:
    mutate(applicant = str_to_title(applicant_name),
           ## borrower is text before dash
           borrower = trimws(str_extract(applicant, "[^-]+")), 
           ## project name is text after dash
           project_name = trimws(str_extract(applicant, "(?<=-).*")), 
           # note there is one project with applicant_name == "Benton Municipal 
           # Water And Sewer Authority" and the name was copied over for 
           # project name 
           project_name = case_when(is.na(project_name) ~ borrower, 
                                    TRUE ~ project_name)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)

####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  pa_clean |> dplyr::group_by(project_id) |> dplyr::tally()
  ####### Decision: No duplicates from funding lists
  
  # Check for disinfection byproduct in description
  pa_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  pa_clean |>
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
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ####### Decision: 3 both --> replacement
  
  # Check for lead subtypes: Unknown
  pa_clean |>
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
      )
    ) |>
    dplyr::filter(lead_type == "unknown")

  # one unknown --> unknown

  ####### SANITY CHECKS END #######
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}