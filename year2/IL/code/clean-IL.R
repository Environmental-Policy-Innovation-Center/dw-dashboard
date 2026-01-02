clean_il_y2 <- function() {
  base_path <- file.path("year2", "IL", "data")

  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread(file.path(base_path, "y2-Illinois_PPL_Fundable.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = clean_numeric_string(requested_loan_amount),
      principal_forgiveness = clean_numeric_string(disadvantaged_community_principal_forgiveness),
      disadvantaged = ifelse(as.numeric(disadvantaged_community_principal_forgiveness) > 0, "Yes", "No Information"),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      list = "fundable"
    )
  
  ## Lead Fundable PPL -----
  il_lead_f <- data.table::fread(file.path(base_path, "y2-Illinois_Lead_Fundable.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = as.character(convert_to_numeric(reserved_principal_forgiveness_amount, fill_na_0 = TRUE) + convert_to_numeric(reserved_loan_amount, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(reserved_principal_forgiveness_amount),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "lead fundable"
    )

  ## EC Fundable PPL -----
  il_ec_f <- data.table::fread(file.path(base_path, "y2-Illinois_EC_Fundable.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = clean_numeric_string(principal_forgiveness_reserved),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_reserved),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "ec fundable"
    )
  

  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread(file.path(base_path, "y2-Illinois_PPL_Applicant.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(requested_loan_amount),  
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "exhausted"
    )
  
  ## Planning Approval List -----
  # The original PDF is a scanned document, for data cleaning use
  il_ppl_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_Base_IIJA GenSupp_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = "No Information",
      expecting_funding = "No",
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  il_ppl_no_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_Base_IIJA_GenSupp_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = "No Information",
      expecting_funding = "No",
      list = "no planning approval"
    )

  ## Lead Planning Approval List -----
  il_lead_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_LSLR_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead planning approval"
    )
  
  ## Lead No Planning Approval List -----
  il_lead_no_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_LSLR_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead no planning approval"
    )
    
  ## EC Planning Approval List -----
  il_ec_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_EC_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Emerging Contaminants",
      list = "ec planning approval"
    )

  ## EC No Planning Approval List -----
  il_ec_no_pla_ap <- data.table::fread("year2/IL/data/IL_Y2_SFY24_EC_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Emerging Contaminants",
      list = "ec no planning approval"
    )

  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"))
  dfs <- dfs[sapply(dfs, is.data.frame)]
  
  il_merge <- dplyr::bind_rows(dfs)

  # Clean and process data
  il_clean <- il_merge |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      pwsid = coalesce(facility_no, "No Information"),
      project_id = str_squish(l17_number),
      project_id = ifelse(project_id == "TBD", "No Information", project_id),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      population = clean_numeric_string(service_population),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      project_type = dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      disadvantaged = ifelse(is.na(disadvantaged), "No Information", disadvantaged),
      project_rank = as.character(NA),
      project_score = ifelse(is.na(project_score), "No Information", project_score),
      
      state = "Illinois",
      state_fiscal_year = "2024"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  il_clean |>
    dplyr::group_by(project_id) |>
    dplyr::tally() |>
    dplyr::filter(n>1)

  # Check project id
  il_clean |> dplyr::filter(project_id == "4188")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "4188" & list =="exhausted")) 

  # Decision: default to ec fundable

  # Check project id
  il_clean |> dplyr::filter(project_id == "5443")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "5443" & list =="exhausted"))
  
  # Decision: default to ec fundable

  # Check project id
  il_clean |> dplyr::filter(project_id == "5877")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "5877" & list =="exhausted"))
  
  # Decision: default to ec fundable

   # Check project id
  il_clean |> dplyr::filter(project_id == "6043")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "6043" & list =="ec fundable"))
  
  # Decision: default to fundable (more info)

   # Check project id
  il_clean |> dplyr::filter(project_id == "6307")

  il_clean <- il_clean |>
    dplyr::group_by(project_id) |>
    dplyr::filter(!(project_id == "6307" & list =="exhausted")) |>
    dplyr::ungroup()
  
  # Decision: default to ec fundable

  # Check project id
  il_clean |> dplyr::filter(project_id == "6375")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "6375" & list =="exhausted"))
  
  # Decision: default to ec fundable

  # Check project id
  il_clean |> dplyr::filter(project_id == "6376")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "6376" & list =="exhausted"))
  
  # Decision: default to ec fundable

  ####### Decision:  
  # if between planning approval and exhausted funding, default to exhausted funding (more recent, more info)
  # if between fundable and any other list, default to fundable; if project description differ, then keep both projects
  # if from the same list, keep both (we are not certain they are not different phases of the same project)
  
  # Check for disinfection byproduct in description
  #il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes
  # il_clean |>
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
  #   dplyr::filter(lead_type == "both")

  ####### Decision: No lead projects classified as both
  # Check for lead subtypes: Unknown
  # il_clean |>
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

  ####### Decision: project with borrower == "Lemont" & state_fiscal_year == "2024" remains "unknown")
  # community_served	borrower	pwsid	project_id	project_name	project_type	project_cost	requested_amount	funding_amount	principal_forgiveness	population	project_description	disadvantaged	project_rank	project_score	expecting_funding	state	state_fiscal_year	list	lead_type
	# Lemont	IL0311620	No Information		Lead		No Information	No Information	No Information	No Information	Replace 2,135 water services with unknown material.	No Information		No Information	No	Illinois	2024	lead no planning approval	unknown
  
  ####### SANITY CHECKS END #######
  
  il_clean <- il_clean |>
    dplyr::select(-list)

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}








