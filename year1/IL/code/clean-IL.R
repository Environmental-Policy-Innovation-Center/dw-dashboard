clean_il_y1 <- function() {
  
  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread("year1/IL/data/13-Illinois_PPL_Fundable.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      funding_amount = clean_numeric_string(estimated_loan_amount),
      disadvantaged = ifelse(disadvantaged_community_principal_forgiveness == "N/E", "Yes", "No"),
      # replace "N/E" with 0 for principal forgiveness since N/E is different from the NAs in other docs
      disadvantaged_community_principal_forgiveness = str_replace(disadvantaged_community_principal_forgiveness, "N/E", "0"),
      principal_forgiveness = clean_numeric_string(disadvantaged_community_principal_forgiveness),
      list = "fundable"
    )
  
  ## Fundable Lead PPL -----
  il_lead_f <- data.table::fread("year1/IL/data/13-Illinois_Lead_Fundable.csv", colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      funding_amount = clean_numeric_string(estimated_loan_amount),
      disadvantaged = "No Information",
      principal_forgiveness = "No Information",
      project_type = "Lead",
      list = "lead fundable"
    )
  
  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread("year1/IL/data/13-Illinois_PPL_Applicant.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      list = "exhausted"
      )
  
  ## Planning Approval List -----
  # The original PDF is a scanned document, for data cleaning use
  # il_ppl_pla_ap$facility_no[stringr::str_count(il_ppl_pla_ap$facility_no) < 9] to determine non standard counds of pwsids
  il_ppl_pla_ap <- data.table::fread("year1/IL/data/IL_Y1_SFY23_Base_IIJA_Gen_Supp_Planning_Approval_List.csv", colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      facility_no = stringr::str_replace_all(facility_no, "^..", "IL"),
      facility_no = stringr::str_replace_all(facility_no, "S", "5"),
      facility_no = stringr::str_replace_all(facility_no, "D", "0"),
      facility_no = stringr::str_replace_all(facility_no, "O", "0"),
      facility_no = stringr::str_replace_all(facility_no, "B", "8"),
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  # The original PDF is a scanned document, for data cleaning use
  # il_ppl_no_pla_ap$facility_no[stringr::str_count(il_ppl_pla_ap$facility_no) < 9] to determine non standard counds of pwsids
  il_ppl_no_pla_ap <- data.table::fread("year1/IL/data/IL_Y1_SFY23_Base_IIJA_Gen_Supp_NoPlanning_Approval_List.csv", colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      facility_no = stringr::str_replace(facility_no, "^..", "IL"),
      facility_no = stringr::str_replace_all(facility_no, "S", "5"),
      facility_no = stringr::str_replace_all(facility_no, "D", "0"),
      facility_no = stringr::str_replace_all(facility_no, "O", "0"),
      facility_no = stringr::str_replace_all(facility_no, "B", "8"),
      list = "no planning approval"
    )
  
  ## Lead Exhausted Funding List -----
  il_lead_a <- data.table::fread("year1/IL/data/13-Illinois_Lead_Applicant.csv",
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      project_type = "Lead",
      list = "lead exhausted"
    )
  
  ## Lead Planning Approval List -----
  # The original PDF is a scanned document, for data cleaning use
  # il_lead_pla_ap$facility_no[stringr::str_count(il_lead_pla_ap$facility_no) < 9] to determine non standard counds of pwsids
  il_lead_pla_ap <- data.table::fread("year1/IL/data/IL_Y1_SFY23_LSLR_Planning_Approval_List.csv", colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      facility_no = stringr::str_replace(facility_no, "^..", "IL"),
      facility_no = stringr::str_replace_all(facility_no, "S", "5"),
      facility_no = stringr::str_replace_all(facility_no, "D", "0"),
      facility_no = stringr::str_replace_all(facility_no, "O", "0"),
      facility_no = stringr::str_replace_all(facility_no, "B", "8"),
      project_type = "Lead",
      list = "lead planning approval"
    )
  
  ## Lead No Planning Approval List -----
  # The original PDF is a scanned document, for data cleaning use
  # il_lead_no_pla_ap$facility_no[stringr::str_count(il_lead_no_pla_ap$facility_no) < 9]
  # il_lead_no_pla_ap$facility_no[stringr::str_count(il_lead_no_pla_ap$facility_no) > 9]
  # to determine non standard counds of pwsids
  il_lead_no_pla_ap <- data.table::fread("year1/IL/data/IL_Y1_SFY23_LSLR_NoPlanning_Approval_List.csv", colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      disadvantaged = "No Information",
      expecting_funding = "No",
      principal_forgiveness = "No Information",
      facility_no = stringr::str_replace(facility_no, "^..", "IL"),
      facility_no = stringr::str_replace_all(facility_no, "S", "5"),
      facility_no = stringr::str_replace_all(facility_no, "D", "0"),
      facility_no = stringr::str_replace_all(facility_no, "O", "0"),
      facility_no = stringr::str_replace_all(facility_no, "B", "8"),
      project_type = "Lead",
      list = "lead no planning approval"
    )
  
  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"))
  dfs <- dfs[sapply(dfs, is.data.frame)]
  
  il_merge <- dplyr::bind_rows(dfs)

  # Clean
  il_clean <- il_merge |>
    # process numeric columns
    dplyr::mutate(population = clean_numeric_string(service_population)) |>
    # process text columns
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      project_score = 
        ifelse(
          list %in% c("fundable", "exhausted", "lead fundable", "lead exhausted"), 
          str_replace_all(loan_priority_score, "[^0-9.]", ""),
          "No Information"
        ), 
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      state = "Illinois",
      state_fiscal_year = "2023",
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = as.character(NA),
      project_rank = as.character(NA),
      project_id = ifelse(l17_number=="TBD", "No Information", l17_number)
    ) |>
    dplyr::mutate(
      project_type = dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl(lead_str, project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      )
    ) |>
    # rename columns
    dplyr::rename(
      pwsid = facility_no
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list) ##remove list after checks
  
   ####### SANITY CHECKS START #######
  # Check any inconsistencies between borrower and pwsid
  # inconsistent_borrower_pwsid <- il_clean |> 
  #   dplyr::group_by(borrower) |>
  #   dplyr::filter(dplyr::n_distinct(pwsid) > 1) |>
  #   dplyr::ungroup()

  # inconsistent_borrower_population <- il_clean |> 
  #   dplyr::group_by(borrower) |>
  #   dplyr::filter(dplyr::n_distinct(population) > 1) |>
  #   dplyr::ungroup()
    
  # Hone in on project id duplication
  #il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::arrange(dplyr::desc(n)) |> dplyr::filter(n>1)

  # Check project id
  il_clean |> dplyr::filter(project_id == "3864")

  # Decision: Default to Base Fundable project
  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "3864" & list =="no planning approval")) 

  # Check project id
  il_clean |> dplyr::filter(project_id == "4594")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "4594" & list =="no planning approval"))
  # Decision: Default to fundable

  # Check project id
  il_clean |> dplyr::filter(project_id == "5086")
  # Decision: From same list, keep both

  # Check project id
  il_clean |> dplyr::filter(project_id == "6050")
  # Decision: Keep both, project descriptins differ

  # Check project id
  il_clean |> dplyr::filter(project_id == "6082")

  il_clean <- il_clean |>
    dplyr::filter(!(project_id == "6082" & list =="no planning approval"))
  # Decision: default to exhausted
            
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

  ####### Decision: No lead projects classified as unknonw
 
    
  ####### SANITY CHECKS END #######
  
  il_clean <- il_clean |>
    dplyr::select(-list)

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
}