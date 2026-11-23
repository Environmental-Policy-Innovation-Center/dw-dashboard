clean_pa_y5 <- function() {
  
  ### Fundable lists ----
  pa_fundable_ec <- data.table::fread("year5/PA/data/pa-sfy26-ec-fundable-list.csv",
                                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "SFY26 Fundable EC"
    ) |>
    dplyr::rename(loan = loan_2)
  
  pa_fundable_lsl <- data.table::fread("year5/PA/data/pa-sfy26-lslr-fundable-list.csv",
                                        colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "SFY26 Fundable LSLR"
    )

  pa_fundable_gs <- data.table::fread("year5/PA/data/pa-sfy26-supp-fundable-list.csv",
                                      colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY26 Fundable Gen Supp"
    )
  
  pa_fundable_base <- data.table::fread("year5/PA/data/pa-sfy26-base-fundable-list.csv",
                                        colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY26 Fundable Base"
    )
  
  pa_fundable <- dplyr::bind_rows(pa_fundable_ec, pa_fundable_base, pa_fundable_gs, pa_fundable_lsl) |>
    dplyr::mutate(
      loan_number = ifelse(loan_number == "Pending", "No Information", loan_number)
    ) |>
    dplyr::select(loan_number, project_name, total_assistance_amount, loan, principal_forgiveness, expecting_funding, project_type, list) 

  # pa_fundable |> dplyr::group_by(loan_number) |> dplyr::tally() |> print(n=100) 
  # 6 loan numbers Pending --> No Information from LSL fundable list
  # However, we can map 2 No information loan number to a loan number based on project_name, applicant name, and $ matching in the pa_comp list

  pa_fundable <-  pa_fundable |>
    dplyr::mutate(
      loan_number = dplyr::case_when(
        project_name == "Municipal Authority of the Borough of Waterford - Lead Service Line Replacements - Phase 1" ~ "82266", 
        project_name == "Pittsburgh Water & Sewer Authority -  2026B LSLR" ~ "80330", 
        .default = loan_number
      )
    )

  pa_fundable_no_info_project_id <- pa_fundable |> dplyr::filter(loan_number == "No Information")

  ### Comprehensive list ----
  pa_comp_q1 <- data.table::fread("year5/PA/data/pa-sfy26-comprehensive-list-q1.csv",
                               colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      pwsid = paste0("PA", trimws(pwsid)),
      list = "SFY26 Comprehensive Q1",
      applicant_name = stringr::str_trim(applicant_name),
      loan_number = ifelse(is.na(loan_number), "No Information", loan_number)
      # ,
      # applicant_name = stringr::str_squish(stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-")),
      # applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PROJECT", " PROJECT"),
      # applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PUBLIC", " PUBLIC")
      # # ,
      # # applicant_name = ifelse(grepl("ERIE", applicant_name), stringr::str_remove(applicant_name, "-(?=[^-]*$)"), applicant_name),
      # # applicant_name = stringr::str_replace_all(applicant_name, "GALVANIZED LINE REPLACEMENT|GALVANIZED LINEREPLACEMENT", "GALVANIZED SERVICE LINE REPLACEMENT"),
      # # applicant_name = ifelse(grepl("WILKINSBURG", applicant_name), stringr::str_replace(applicant_name, "REPLACEMENTS", "REPLACEMENT"), applicant_name),
      # # applicant_name = stringr::str_squish(applicant_name)
    )
  
  # pa_comp_q1 |> dplyr::filter(loan_number == "No Information")
  # 4 projects in Q1 without loan number

  pa_comp_q2 <- data.table::fread("year5/PA/data/pa-sfy26-comprehensive-list-q2.csv",
                               colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      pwsid = paste0("PA", trimws(pwsid)),
      list = "SFY26 Comprehensive Q2",
      applicant_name = stringr::str_trim(applicant_name),
      loan_number = ifelse(is.na(loan_number), "No Information", loan_number)
    )
  
  # pa_comp_q2 |> dplyr::filter(loan_number == "No Information")
  # 18 projects in Q2 without loan number
  
  # sum((pa_comp_q1 |>  dplyr::filter(!loan_number == "No Information") |> dplyr::pull(loan_number)) %in% (pa_comp_q2 |>  dplyr::filter(!loan_number == "No Information") |> dplyr::pull(loan_number)))
  # Expecting overlap of at least 46 projects between Q1 and Q2; tbd if No INformation can be mapped
  
  in_both_q1 <- pa_comp_q1 |>
    dplyr::filter(!loan_number == "No Information") |>
    dplyr::filter(
      loan_number %in% pa_comp_q2$loan_number
    ) |>
    dplyr::arrange(loan_number)
  
  in_both_q2 <- pa_comp_q2 |>
    dplyr::filter(!loan_number == "No Information") |>
    dplyr::filter(
      loan_number %in% pa_comp_q1$loan_number
    ) |>
    dplyr::arrange(loan_number)

  # waldo::compare(in_both_q1, in_both_q2)
  # changes from q1 to q2 include address rank and population, pv_rating in q2 is NA; this is used for project score in the DD

  pa_comp_q1_no_info <- pa_comp_q1 |>
    dplyr::filter(loan_number == "No Information") 

  pa_comp_q2_no_info <- pa_comp_q2 |>
    dplyr::filter(loan_number == "No Information") 

  # Use pwsid to compare projects with No Information loan number; and determine if there's overlap 
  # Create an epic id, to filter projects (NOTE the following operation will change when data changes!)
  pa_comp_no_info_project_id <- pa_comp_q1_no_info |>
    dplyr::bind_rows(pa_comp_q2_no_info) |>
    dplyr::arrange(pwsid) |> 
    dplyr::mutate(epic_id = dplyr::row_number()) |>
    dplyr::filter(
      !epic_id %in% c(5, 14) #epic_id ==5 --> amendment to pop| epic_id ==14 --> amendment to address pop
    ) 
# There were 2 projects with No Information loan number that overlapped in comp list; deduplicated in favor of Q2; these 2 projects are lsl projects that are expecting funding (in LSL fundable list, with no loan number as well)
  
pa_comp <- pa_comp_q2 |>
  dplyr::filter(!loan_number == "No Information") |>
  dplyr::bind_rows(
    pa_comp_q1 |>  dplyr::filter(!loan_number == "No Information") |> dplyr::filter(!loan_number %in% pa_comp_q2$loan_number) #only append projects not present in q2
  ) |>
  dplyr::bind_rows(pa_comp_no_info_project_id)

# these loan ids have expecting funding = "no information" if they are not in fundable lists; have to examine projects with No Information loan number separately. 
pa_comp_q2_loan_nef <- pa_comp_q2 |>
  dplyr::filter(!loan_number == "No Information") |>
  dplyr::filter(!loan_number %in% pa_comp_q1$loan_number) |>
  dplyr::pull(loan_number)

# compare applicant name (from comp) to project_name (from fundable) to deduplicate projects
collapsed_comp_fun_no_info_project <- pa_comp_no_info_project_id |>
  dplyr::bind_rows(pa_fundable_no_info_project_id) |> 
  dplyr::mutate(
    epic_id = dplyr::case_when(
      project_name == "Pittsburgh Water & Sewer Autority - 2027A LSLR" ~ 15,
      project_name == "Aqua Pennsylvania, Inc. - Abington LSLR" ~ 6,
      is.na(epic_id) ~ dplyr::row_number() + 100, #added 100 to avoid incorrect collapsing of rows
      .default = epic_id
    )
  ) |> 
  dplyr::group_by(epic_id) |>
  tidyr::fill(everything(), .direction = "downup") |> 
  dplyr::slice_tail() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    list.y = ifelse(list == "SFY26 Fundable LSLR", "SFY26 Fundable LSLR", NA),
    project_type.y = ifelse(list == "SFY26 Fundable LSLR", project_type, NA)
  ) |>
  dplyr::rename(
    list.x = list
  )

# these project names appear to be related, but due to ambiguity, we cannot confidently deduplicat:
# City of Philadelphia 2026 Part 1 LSLR
# City of Philadelphia 2026 Part 2 LSLR
  
  pa_intermediate <-  pa_comp |> 
    dplyr::filter(!loan_number == "No Information") |>
    dplyr::full_join(pa_fundable |> dplyr::filter(!loan_number == "No Information"), by = c("loan_number")) |>
    dplyr::bind_rows(collapsed_comp_fun_no_info_project) 

  ### Clean  ----
  pa_clean <- pa_intermediate |>
    dplyr::mutate(
      list = dplyr::coalesce(list.y, list.x),
      applicant_name = stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-"),
      community_served = dplyr::case_when(
        !is.na(city) ~ paste0(
          stringr::str_to_title(city), ", ",
          stringr::str_to_title(county), " County"
        ),
        !is.na(county) ~ paste0(
          stringr::str_to_title(county), " County"
        ),
        TRUE ~ "No Information"
      ),
      applicant = stringr::str_to_title(applicant_name),
      # [keep] borrower is text before dash
      borrower = ifelse(
        is.na(applicant_name),
       trimws(stringr::str_extract(project_name, "^[^\\-\u2013\u2014]+")),
        trimws(stringr::str_extract(applicant, "^[^\\-\u2013\u2014]+"))
      ),
      # [keep] projects in fundable list are crosswalked to comprehensive, and we default the pwsid to the comprehensive list
      pwsid = ifelse(
        is.na(pwsid),
        "No Information",
        stringr::str_squish(pwsid)
      ),
      project_id = clean_numeric_string(loan_number), 
      project_name = ifelse(
        grepl("fundable", list, ignore.case = TRUE),
        project_name,
        trimws(stringr::str_extract(applicant, "(?<=-).*"))
      ),
      # [keep] Project description and Problem description variables are only present in the comprehensive list and we default to these values as all fundable projets are in comprehensive lists
      project_description = 
        ifelse(
          is.na(proj_description),
          "No Information",  
          paste0("Project Description: ", proj_description, "; Problem Description: ", prob_description)
        ),
      project_type = dplyr::case_when(
        !is.na(project_type.y) ~ project_type.y, 
        #[keep] these are non lead material exceptions
        grepl("one lead and one lag", project_description, ignore.case = T) ~ "Emerging Contaminants", 
        grepl("lead-lag", project_description, ignore.case = T) ~ "Emerging Contaminants", 
        grepl("be the lead project", project_description, ignore.case = T) ~ "General", 
        grepl("lead|lsl|galvanized|grr", project_description, ignore.case = T) ~ "Lead", 
        (list %in% c("SFY26 Fundable Base", "SFY26 Fundable Gen Supp") & grepl("lead|lsl|galvanized|grr", project_name, ignore.case = T)) ~ "Lead", 
        grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
        (list %in% c("SFY26 Fundable Base", "SFY26 Fundable Gen Supp") & grepl(ec_str, project_name, ignore.case = T)) ~ "Emerging Contaminants", 
        TRUE ~ "General"), 
      project_cost = clean_numeric_string(project_cost),
      requested_amount = as.character(NA), 
      funding_amount = clean_numeric_string(total_assistance_amount), 
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      population = clean_numeric_string(population),  
      disadvantaged = as.character(NA),
      project_rank = clean_numeric_string(projrank), 
      # [keep] PV rating is a numeric feature,  clean_numeric_string will coerce any missing values to "No Information", and set the column to character type
      project_score = clean_numeric_string(pv_rating),
      # [keep] there are no projects id by PENNVEST and DEP as future projects
      expecting_funding = 
        dplyr::case_when(
          !is.na(expecting_funding) ~ expecting_funding,
          list == "SFY26 Comprehensive Q2" & loan_number %in% pa_comp_q2_loan_nef ~ "No Information", #by checking the list we know it's not a fundable project
          .default = "No"
        ),
      state = "Pennsylvania",
      state_fiscal_year = "2026") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
                  requested_amount, funding_amount, principal_forgiveness, population, project_description,
                  disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)

  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  # pa_clean |> dplyr::group_by(project_id) |> dplyr::tally() |>dplyr::arrange(dplyr::desc(n))
  ####### Decision: one duplicate: 82264, but 2 distinct projects
    
  # Check for disinfection byproduct in description
  # pa_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: 1 disinfection byproduct string (project id 85240) --> reclassify as General
  
  pa_clean <- pa_clean |> 
    dplyr::mutate(
      project_type = ifelse(grepl("disinfection byproduct", project_description), "General", project_type)
    )

  # Check pwsid length
  # pa_clean |>
  #   dplyr::mutate(length_pwsid = stringr::str_count(pwsid)) |>
  #   dplyr::group_by(length_pwsid) |>
  #   dplyr::tally()
  # Decision: Consistent length

  #Examine lead projects
  # pa_clean|>
  #   dplyr::filter(project_type == "Lead") |> #lead projects
  #   dplyr::filter(!grepl("lsl", list, ignore.case = TRUE)) |> # drop list specific determination
  #   dplyr::filter(grepl("lead|lsl|galvanized|grr", project_description)) |> # assigned by keyword search
  #   dplyr::filter(!grepl("lsli", project_name)) |> #for sure lead
  #   dplyr::filter(!grepl("lslr", project_name)) |> #for sure lead
  #   dplyr::filter(!grepl("lead service", project_name)) |> #for sure lead
  #   dplyr::filter(!grepl("lead line", project_description)) |> #for sure lead material
  #   dplyr::filter(!grepl("lead service line", project_description)) |> #for sure lead material
  #   dplyr::filter(!grepl("lead services line", project_description)) |> #for sure lead material
  #   dplyr::filter(!grepl("leaded joint", project_description)) |> #for sure lead material
  #   dplyr::filter(!grepl("galvanized", project_description)) #for sure lead material
  # Non-lead material exceptions
    # dplyr::filter(!grepl("lead project", project_description)) |> #non material related, addressed --> General (2)
    # dplyr::filter(!grepl("lead-lag", project_description)) |> #non material related, but plant configuration; addressed --> EC type (2)
    # dplyr::filter(!grepl("one lead and one lag", project_description)) #non material related, addressed --> EC type

  # Decision:recategorize projects upstream 

  # Check for lead subtypes: Both
  #  pa_clean |>
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
#  pa_clean |>
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
#     dplyr::filter(lead_type == "unknown")
  
  # 4 unknown --> 82256 --> General, 82265 --> LSLR, two lead fundable  --> LSLR

  pa_clean <- pa_clean |>
    dplyr::mutate(
      project_type = dplyr::case_when(
        project_id == "82256" ~ "General", #No know lead lines (in description)
        .default = project_type
    ),
      project_description = dplyr::case_when(
        project_id == "82265" ~ paste0(project_description, " | FT: LSLR"),
        project_name %in% c("City of Philadelphia - 2026 Part 1 LSLR", "City of Philadelphia - 2026 Part 2 LSLR") ~ paste0(project_description, " | FT: LSLR"),
        .default = project_description 
    )
  )

  ####### SANITY CHECKS END #######
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}