clean_il_y5 <- function() {
  
  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread("year5/IL/data/il-sfy27-base-iija-gen-supp-fundable-list.csv",
                                colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(projected_loan_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      requested_amount = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      list = "fundable"
    )
  
  ## Lead Fundable PPL -----
  il_lead_f <- data.table::fread("year5/IL/data/il-sfy27-lead-fundable-list.csv",
                                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    filter(!is.na(loan_applicant)) |>
    dplyr::mutate(
      funding_amount = as.character(convert_to_numeric(principal_forgiveness, fill_na_0 = TRUE) + convert_to_numeric(loan_at_0_00_percent, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      requested_amount = clean_numeric_string(total_funding_requested),
      project_score = priority_score,
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "lead fundable"
    )
  
  ## EC Fundable PPL -----
  il_ec_f <- data.table::fread("year5/IL/data/il-sfy27-ec-fundable-list.csv",
                               colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = as.character(convert_to_numeric(principal_forgiveness, fill_na_0 = TRUE) + convert_to_numeric(pwslp_funding_needed, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      requested_amount = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "ec fundable"
    )
  
  
  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread("year5/IL/data/il-sfy27-base-iija-gen-supp-exhausted-list.csv",
                                colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "exhausted"
    )
  
  ## Planning Approval List -----
  il_ppl_pla_ap <- data.table::fread("year5/IL/data/il-sfy27-base-iija-gen-supp-planning-approval-list.csv",
                                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      expecting_funding = "No",
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  il_ppl_no_pla_ap <- data.table::fread("year5/IL/data/il-sfy27-base-iija-gen-supp-no-planning-approval-list.csv",
                                        colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      expecting_funding = "No",
      list = "no planning approval"
    )
  
  ## Lead Exhausted Funding List -----
  il_lead_a <- data.table::fread("year5/IL/data/il-sfy27-lead-exhausted-funding-list.csv",
                                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(total_funding_requested),
      project_score = priority_score,
      expecting_funding = "No",
      list = "lead exhausted"
    )
  
  ## Lead Planning Approval List -----
  il_lead_pla_ap <- data.table::fread("year5/IL/data/il-sfy27-lead-planning-approval-list.csv",
                                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead planning approval"
    )
  
  ## Lead No Planning Approval List -----
  il_lead_no_pla_ap <- data.table::fread("year5/IL/data/il-sfy27-lead-no-planning-approval-list.csv",
                                         colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead no planning approval"
    )
  
  
  ## EC Exhausted Funding List -----
  il_ec_no_pla_ap <- data.table::fread("year5/IL/data/il-sfy27-ec-exhausted-funding-list.csv",
                                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      requested_amount = clean_numeric_string(projected_loan_amount),
      expecting_funding = "No",
      project_type = "Emerging Contaminants",
      project_score = loan_priority_score,
      list = "ec exhausted"
    )
  
  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"))
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
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      project_type = dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      population = as.character(NA),
      disadvantaged = as.character(NA),
      project_rank = str_squish(rank),
      project_rank = replace_na(project_rank, "No Information"),
      project_score = ifelse(is.na(project_score), "No Information", project_score),
      state = "Illinois",
      state_fiscal_year = "2027"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  # dupes <- il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::filter(n>1)
  
  # dupes_list <- il_clean %>%
  #   filter(project_id %in% dupes$project_id) %>%
  #   filter(project_id != "No Information")
  
  ####### Decision:  

  # Projects on the fundable and EC fundable lists (e.g., 6383): 
      # For funding amounts, we'll take the sum of the Principal Forgiveness and PWSLP Funding Needed from the EC fundable list only
      # For PF amounts, we'll sum the PF amounts from both the gen fundable and EC fundable lists
  
  # Relevant projects: 6383, 6742, 6810, 7429, 7610
  
  il_clean <- il_clean %>%
    dplyr::mutate(
      keep = case_when(
        project_id == "6383" & list == "fundable" ~ FALSE,
        project_id == "6742" & list == "fundable" ~ FALSE,
        project_id == "6810" & list == "fundable" ~ FALSE,
        project_id == "7281" & list == "fundable" ~ FALSE,
        project_id == "7429" & list == "fundable" ~ FALSE,
        project_id == "7610" & list == "fundable" ~ FALSE,
        TRUE ~ TRUE),
      principal_forgiveness = case_when(
        project_id == "6383" ~ "7117000",
        project_id == "6742" ~ "8439541",
        project_id == "6810" ~ "6151750",
        project_id == "7281" ~ "6897490",
        project_id == "7429" ~ "5414650",
        project_id == "7610" ~ "5200000",
        TRUE ~ principal_forgiveness)
      ) 
  
 # Projects on exhausted and EC exhausted lists (e.g., 7110): 
    # just keep the information from the EC exhausted list
    # The project is eligible for both gen and EC funds (which is why it appears twice), but we'll keep this as one EC project
  
  # Relevant projects: 7110, 7123, 7140, 7141 7355, 7362, 7598
  
  il_clean <- il_clean %>%
    dplyr::mutate(
      keep = case_when(
        project_id == "7110" & list == "exhausted" ~ FALSE,
        project_id == "7123" & list == "exhausted" ~ FALSE,
        project_id == "7140" & list == "exhausted" ~ FALSE,
        project_id == "7141" & list == "exhausted" ~ FALSE,
        project_id == "7355" & list == "exhausted" ~ FALSE,
        project_id == "7362" & list == "exhausted" ~ FALSE,
        project_id == "7598" & list == "exhausted" ~ FALSE,
        TRUE ~ keep))
  

  # Projects on fundable and EC exhausted lists (e.g., 7318): 
    # These will be considered EC projects, but we'll keep the funding amounts and PF information from the gen fundable list
  
  # Relevant projects: 7030, 7318, 7889
  
  il_clean <- il_clean %>%
    dplyr::mutate(
      keep = case_when(
        project_id == "7030" & list == "ec exhausted" ~ FALSE,
        project_id == "7318" & list == "ec exhausted" ~ FALSE,
        project_id == "7889" & list == "ec exhausted" ~ FALSE,
        TRUE ~ keep))
  

  # drop projects edited above
  il_clean <- il_clean |>
    dplyr::filter(keep) |>
    dplyr::select(-keep)
  
  # Check for disinfection byproduct in description
  # il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
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
  
  # # Check for lead subtypes: Unknown
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

  il_lead_unknow_id <- il_clean |>
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
    dplyr::filter(lead_type == "unknown") |>
    dplyr::pull(project_id)

  #drop No Information
  il_lead_unknow_id <- il_lead_unknow_id[!grepl("No Information", il_lead_unknow_id)]
  
  ####### Decision: 26 projects unknown --> all LSLR

  il_clean <- il_clean |>
      dplyr::mutate(
       project_description = dplyr::case_when(
         borrower == "Broadview Estates Mhp" & pwsid == "IL1190120"  ~ paste0(project_description, " | FT: LSLR"), #unknow project id
         project_id %in% il_lead_unknow_id ~ paste0(project_description, " | FT: LSLR"),
              .default = project_description
       )
      )
  
  ####### SANITY CHECKS END #######
  
  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)
  
}