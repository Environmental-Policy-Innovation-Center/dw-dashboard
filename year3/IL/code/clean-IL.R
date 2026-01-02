clean_il_y3 <- function() {
  base_path <- file.path("year3", "IL", "data")

  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread(file.path(base_path, "y3-Illinois_PPL_Fundable.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      #requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = clean_numeric_string(estimated_loan_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_reserved),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      list = "fundable"
    )
  
  ## Lead Fundable PPL -----
  il_lead_f <- data.table::fread(file.path(base_path, "y3-Illinois_Lead_Fundable.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      #requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = as.character(convert_to_numeric(principal_forgiveness_reserved, fill_na_0 = TRUE) + convert_to_numeric(loan_funding_reserved, fill_na_0 = TRUE)),
      principal_forgiveness =clean_numeric_string(principal_forgiveness_reserved),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "lead fundable"
    )

  ## EC Fundable PPL -----
  il_ec_f <- data.table::fread(file.path(base_path, "y3-Illinois_EC_Fundable.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      #requested_amount = clean_numeric_string(requested_loan_amount),  
      funding_amount = clean_numeric_string(principal_forgiveness),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "ec fundable"
    )

  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread(file.path(base_path, "y3-Illinois_PPL_Applicant.csv"),
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      #requested_amount = clean_numeric_string(requested_loan_amount),  
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "exhausted"
    )
  
  ## Planning Approval List -----
  il_ppl_pla_ap <- data.table::fread("year3/IL/data/IL_Y3_SFY25_Base_IIJAGenSupp_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      #requested_amount = "No Information",
      expecting_funding = "No",
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  il_ppl_no_pla_ap <- data.table::fread("year3/IL/data/IL_Y3_SFY25_Base_IIJAGenSupp_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      #requested_amount = "No Information",
      expecting_funding = "No",
      list = "no planning approval"
    )

  ## Lead Planning Approval List -----
  il_lead_pla_ap <- data.table::fread("year3/IL/data/IL_Y3_SFY25_LSLR_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      #requested_amount = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead planning approval"
    )
  
  ## Lead No Planning Approval List -----
  il_lead_no_pla_ap <- data.table::fread("year3/IL/data/IL_Y3_SFY25_LSLR_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      #requested_amount = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead no planning approval"
    )

  ## EC No Planning Approval List -----
  il_ec_no_pla_ap <- data.table::fread("year3/IL/data/IL_Y3_SFY25_EC_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      #requested_amount = clean_numeric_string(requested_loan_amount),  
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
      #Note also that some cells in these lists are blank. If these projects are not duplicates with the missing information in another list, they should be marked as No Information
      pwsid = coalesce(facility_no, "No Information"),
      pwsid = ifelse(grepl("^[0-9]+$", pwsid), paste0("IL", pwsid), pwsid), #prepend IL if character is only numeric
      project_id = str_squish(l17_number),
      project_id = ifelse(project_id == "TBD", "No Information", project_id),
      project_id = ifelse(is.na(project_id), "No Information", project_id),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(requested_loan_amount),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      project_type = dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      population = dplyr::case_when(
        list == "fundable" ~ clean_numeric_string(population),
        list == "exhausted" ~ clean_numeric_string(population),
        list == "ec fundable" ~ clean_numeric_string(population),
        list == "lead fundable" ~ clean_numeric_string(service_population),
        .default = "No Information"),
      disadvantaged = as.character(NA),
      project_rank = as.character(NA),
      project_score = ifelse(is.na(project_score), "No Information", project_score),
      state = "Illinois",
      state_fiscal_year = "2025"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  #TODO check why some funding amounts for fundable lists (along with their PF) is 0
  # Hone in on project id duplication
  #il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::filter(n>1)

  # Check project id
  #il_clean |> dplyr::filter(project_id %in% c("6024", "6308", "6375","6376","6742","6813","7109","7110","7111","7140", "7141", "7173")) |> arrange(project_id) |> View()

  il_clean <- il_clean |>
    dplyr::mutate(keep = dplyr::case_when(
      project_id == "6024" & list =="no planning approval" ~ FALSE , # decision: default to exhausted list
      project_id == "6308" ~ TRUE , # decision: keep both, different information (funding amounts)
      project_id == "6375" & list == "exhausted" ~ FALSE, # decision:default to ec fundable, following up with the state, bring pwsid from exhausted list
      project_id == "6376" & list == "exhausted" ~ FALSE, # decision:default to ec fundable, following up with the state, bring pwsid from exhausted list
      project_id == "6742" ~ TRUE , # decision: keep both, different info (requested amount)
      project_id == "6813" ~ TRUE , # decision: keep both, different info (requested amount) 
      project_id == "7109" & list == "ec fundable" ~ FALSE, #decision: default to fundable, more info 
      project_id == "7110" & list == "ec fundable" ~ FALSE, #decision: default to fundable, more info  
      project_id == "7111" & list == "ec fundable" ~ FALSE, #decision: default to fundable, more info   
      project_id == "7140" & list == "exhausted" ~ FALSE, # decision:default to ec fundable, following up with the state, bring pwsid from exhausted list 
      project_id == "7141" & list == "exhausted" ~ FALSE, # decision:default to ec fundable, following up with the state, bring pwsid from exhausted list 
      project_id == "7173" & list == "exhausted" ~ FALSE, # decision:default to ec fundable, following up with the state, bring pwsid from exhausted list 
      .default = TRUE
  )) |>
  dplyr::filter(keep) |>
  dplyr::select(-keep) |>
  dplyr::mutate(
      pwsid =ifelse(project_id %in% c("6308", "7109", "7110", "7111"), "IL2015500", pwsid),
      pwsid =ifelse(project_id %in% c("6375", "6376", "7140", "7141", "7173"), "IL970200", pwsid),
      pwsid =ifelse(project_id == "6813", "IL1110100", pwsid)
    )
  
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

  ####### Decision: No lead projects classified as unknown 
  
  ####### SANITY CHECKS END #######
  
  il_clean <- il_clean |>
    dplyr::select(-list)

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}