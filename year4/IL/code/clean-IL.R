clean_il_y4 <- function() {

  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Fundable_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(projected_loan_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      list = "fundable"
    )
  
  ## Lead Fundable PPL -----
  il_lead_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Fundable_List.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = as.character(convert_to_numeric(principal_forgiveness_reserved, fill_na_0 = TRUE) + convert_to_numeric(loan_funding_reserved, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_reserved),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "lead fundable"
    )

  ## EC Fundable PPL -----
  il_ec_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_EC_Fundable_List.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(principal_forgiveness),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "ec fundable"
    )
  

  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "exhausted"
    )
  
  ## Planning Approval List -----
  il_ppl_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  il_ppl_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      list = "no planning approval"
    )

  ## Lead Exhausted Funding List -----
  il_lead_a <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "lead exhausted"
    )
  
  ## Lead Planning Approval List -----
  il_lead_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead planning approval"
    )
  
  ## Lead No Planning Approval List -----
  il_lead_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead no planning approval"
    )
    
  
  ## EC Exhausted Funding List -----
  il_ec_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_EC_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Emerging Contaminants",
      project_score = loan_priority_score,
      list = "ec exhausted"
    )

  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"), envir = .GlobalEnv)
  dfs <- dfs[sapply(dfs, is.data.frame)]
  
  il_merge <- dplyr::bind_rows(dfs)

  # Clean and process data -----
  il_clean <- il_merge |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      pwsid = coalesce(facility_no, "No Information"),
      pwsid = ifelse(grepl("^[0-9]+$", pwsid), paste0("IL", pwsid), pwsid), #prepend IL if character is only numeric
      project_id = str_squish(l17_number),
      project_id = ifelse(is.na(project_id), "No Information", project_id),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = dplyr::case_when(
        list == "ec fundable" ~ clean_numeric_string(requested_loan_amount),
        list == "ec exhausted" ~ clean_numeric_string(requested_loan_amount),
        list == "lead fundable" ~ clean_numeric_string(requested_loan_amount),
        .default = "No Information"
      ),
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
        list == "exhausted"  ~ clean_numeric_string(population),
        .default = "No Information"
      ),
      disadvantaged = as.character(NA),
      project_rank = as.character(NA),
      project_score = ifelse(is.na(project_score), "No Information", project_score),
      state = "Illinois",
      state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::filter(n>1)

  # Check project id
  # il_clean |> dplyr::filter(project_id %in% c("6742","6744", "6810", "7030", "7281", "7594")) |> arrange(project_id) |> View()
  
  il_clean <- il_clean |>
    dplyr::filter(project_id %in% c("6742","6744","6810","7030","7281","7594")) |>
    dplyr::group_by(project_id) |>
    dplyr::mutate(keep = dplyr::case_when(
      project_id == "6742" ~ TRUE, # keep both ec fundable and fundable list (funding amounts differ, may be covering different phases of larger project)
      project_id == "6744" ~ TRUE, # keep both ec fundable and fundable list (funding amounts differ, may be covering different phases of larger project)
      project_id == "6810" ~ TRUE, # keep both ec fundable and fundable list (funding amounts differ, may be covering different phases of larger project)
      project_id == "7030" ~ TRUE, # keep both ec fundable and fundable list (funding amounts differ, may be covering different phases of larger project)
      project_id == "7281" & list == "exhausted" ~ FALSE,       # default to ec exhausted
      project_id == "7594" ~ row_number() == 1                    # keep first instance (same info)
  )) |>
  dplyr::filter(keep) |>
  dplyr::select(-keep) |>
  dplyr::ungroup() |>
  dplyr::mutate(
      pwsid =ifelse(project_id == "6744", "IL0335030", pwsid),
      pwsid =ifelse(project_id == "6810", "IL1434750", pwsid),
      pwsid =ifelse(project_id == "7281", "IL1190200", pwsid)
    )

  ####### Decision:  
  # if between planning approval and exhausted funding, default to exhausted funding (more recent, more info)
  # if between fundable and any other list, default to fundable; if project description differ, then keep both projects
  # if from the same list, keep both (we are not certain they are not different phases of the same project)
  
  # Check for disinfection byproduct in description
  il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  # Check for lead subtypes
  il_clean |>
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

  ####### Decision: No lead projects classified as both
  
  # Check for lead subtypes: Unknown
  il_clean |>
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

  ####### Decision: No lead projects classified as unknown 
  
  ####### SANITY CHECKS END #######

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)

}