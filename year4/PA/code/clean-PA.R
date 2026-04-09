clean_pa_y4 <- function() {
  
  ### Fundable lists ----
  pa_fundable_ec <- data.table::fread("year4/PA/data/PA_Y4_SFY25_EC_Fundable_List.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
       expecting_funding = "Yes",
       project_type = "Emerging Contaminants",
       list = "SFY25 fundable EC"
    )
  
  pa_fundable_gs <- data.table::fread("year4/PA/data/PA_Y4_SFY25_IIJA_Gen_Supp_Fundable_List.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY25 fundable Gen Supp"
    )
    
  pa_fundable_lead <- data.table::fread("year4/PA/data/PA_Y4_SFY25_LSLR_Fundable_List.csv",
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
       expecting_funding = "Yes",
       project_type = "Lead",
       list = "SFY25 fundable Lead"
      )
    
  pa_fundable_base <- data.table::fread("year4/PA/data/PA_Y4_SFY25_Base_Fundable.csv",
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      expecting_funding = "Yes",
      list = "SFY25 fundable Base"
    )
  
  pa_fundable <- dplyr::bind_rows(pa_fundable_ec, pa_fundable_base, pa_fundable_gs, pa_fundable_lead) |>
    dplyr::mutate(
       applicant_name = stringr::str_to_upper(project_name),
       applicant_name = stringr::str_remove_all(applicant_name, "\n"),
       applicant_name = stringr::str_squish(stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-"))
      ) |>
    tidyr::separate_wider_delim(
      applicant_name,
      delim = "PWSID",
      names = c("applicant_name", "pwsid"),
      too_few = "align_start",
      cols_remove = TRUE 
    )|>
    dplyr::mutate(
      pwsid = stringr::str_remove(pwsid, " "),
      pwsid = ifelse(is.na(pwsid), "No Information", paste0("PA", pwsid)),
      applicant_name = stringr::str_trim(applicant_name),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PROJECT", " PROJECT"),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PUBLIC", " PUBLIC"),
      #applicant_name = ifelse(grepl("ERIE", applicant_name), stringr::str_squish(stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-")), applicant_name),
      applicant_name = stringr::str_squish(applicant_name),
      applicant_name = stringr::str_replace_all(applicant_name, "GALVANIZED LINE REPLACEMENT|GALVANIZED LINEREPLACEMENT", "GALVANIZED SERVICE LINE REPLACEMENT"),
      applicant_name = ifelse(applicant_name == "PITTSBURGH WATER AND SEWER AUTHORITY - 2023 SMALL DIAMETER WATER MAIN REPLACEMENT (NON-LEAD) 1", "PITTSBURGH WATER AND SEWER AUTHORITY - 2023 SMALL DIAMETER WATER MAIN REPLACEMENT (NON-LEAD)", applicant_name)
    )
  
  ### Comprehensive list ----
  pa_comp <- data.table::fread("year4/PA/data/PA_Y4_SFY25_Comprehensive.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      pwsid = paste0("PA", trimws(pwsid)),
      list = "SFY25 comprehensive"   
    )|>
    tidyr::separate_wider_delim(
      applicant_name,
      delim = "PWSID",
      names = c("applicant_name", "pwsid_drop"),
      too_few = "align_start",
      cols_remove = TRUE 
    )|>
    dplyr::select(-pwsid_drop) |>
    dplyr::mutate(
      applicant_name = stringr::str_trim(applicant_name),
      applicant_name = stringr::str_squish(stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-")),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PROJECT", " PROJECT"),
      applicant_name = stringr::str_replace_all(applicant_name, "(?<![\\s])PUBLIC", " PUBLIC"),
      applicant_name = ifelse(grepl("ERIE", applicant_name), stringr::str_remove(applicant_name, "-(?=[^-]*$)"), applicant_name),
      applicant_name = stringr::str_replace_all(applicant_name, "GALVANIZED LINE REPLACEMENT|GALVANIZED LINEREPLACEMENT", "GALVANIZED SERVICE LINE REPLACEMENT"),
      applicant_name = ifelse(grepl("WILKINSBURG", applicant_name), stringr::str_replace(applicant_name, "REPLACEMENTS", "REPLACEMENT"), applicant_name),
      applicant_name = ifelse(applicant_name=="AQUA PENNSYLVANIA MAIN- UPPER DARBY LSL", "AQUA PENNSYLVANIA - UPPER DARBY", applicant_name),
      applicant_name = ifelse(grepl("GRATZ", applicant_name),"GRATZ BOROUGH MUNICIPAL AUTHORITY", applicant_name),
      applicant_name = ifelse(grepl("NEW KENSINGTON", applicant_name),"MUNICIPAL AUTHORITY OF THE CITY OF NEW KENSINGTON 2024 WATERLINE REPLACEMENTS/WTP FILTER MEDIA REPLACEMENT", applicant_name),
      applicant_name = ifelse(applicant_name == "PENNSYLVANIA-AMERICAN WATER COMPANY - UNIONTOWN LSLR PHASE 1", "PENNSYLVANIA AMERICAN WATER COMPANY - UNIONTOWN LSLR PHASE 1", applicant_name),
      applicant_name = ifelse(applicant_name == "PENNSYLVANIA-AMERICAN WATER COMPANY - NEW CUMBERLAND LSLR PHASE 1", "PENNSYLVANIA AMERICAN WATER COMPANY - NEW CUMBERLAND LSLR PHASE 1", applicant_name),
      applicant_name = ifelse(applicant_name == "CITY OF PHILADELPHIA EAST PARK BOOSTER PUMPING STATION", "CITY OF PHILADELPHIA- EAST PARK BOOSTER PUMPING STATION1", applicant_name),
      applicant_name = ifelse(applicant_name == "COMBINED HAZLETON CITY AUTHORITY VILLAGE OF BUCK MTN WATER MAIN REPLACEMENT - W", "HAZLETON CITY AUTHORITY 2025 WATER SYSTEM UPGRADES", applicant_name),
      applicant_name = stringr::str_replace(applicant_name, "PITTSBURGH WATER & SEWER AUTHORITY", "PITTSBURGH WATER AND SEWER AUTHORITY"),
      applicant_name = stringr::str_replace(applicant_name, "WSA", "WATER AND SEWER AUTHORITY"),
      applicant_name = stringr::str_replace(applicant_name, "PITTSBURGH WATER AND SEWER AUTHORITY - 2025B LSLR", "PITTSBURGH WATER AND SEWER AUTHORITY 2025B LSLR"),
      applicant_name = stringr::str_replace(applicant_name, "PITTSBURGH WATER AND SEWER AUTHORITY- 2026A LSLR", "PITTSBURGH WATER AND SEWER AUTHORITY 2026A LSLR"),
      applicant_name = stringr::str_replace(applicant_name, "PITTSBURGH WATER AND SEWER AUTHORITY - 2023 SMALL DIAMETER WATER MAIN REPLACEMENT (NON-LEAD)", "PITTSBURGH WATER AND SEWER AUTHORITY - 2023 SMALL DIAMETER WATER MAIN REPLACEMENT (NON-LEAD) 1"),
      applicant_name = ifelse(applicant_name == "PITTSBURGH WATER & SEWER AUTH. - 2024 AND 2023 A2 SMALL DIA. MAIN REPL. (LEAD)", "PITTSBURGH WATER AND SEWER AUTHORITY 2023 - 2024 A2 SMALL DIAMETER WATER MAIN REPLACEMENT (LSLR)", applicant_name ),
      applicant_name = stringr::str_squish(applicant_name)
    )
  
  #pa_fundable[pa_fundable$applicant_name %in% pa_comp$applicant_name, "applicant_name"]
  #pa_fundable[!pa_fundable$applicant_name %in% pa_comp$applicant_name, "applicant_name"]

  ### Clean  ----
  pa_clean <- pa_comp |> 
    dplyr::full_join(pa_fundable, by = c("applicant_name")) |>
    dplyr::mutate(
      list = ifelse(is.na(list.y), list.x, list.y),
      applicant_name = stringr::str_replace_all(applicant_name, "[\u2013\u2014]", "-"),
      community_served = paste0(stringr::str_to_title(city), ", ", stringr::str_to_title(county), " County"), 
      applicant = stringr::str_to_title(applicant_name),
      # [keep] borrower is text before dash
      borrower = trimws(stringr::str_extract(applicant, "^[^\\-\u2013\u2014]+")),
      # [keep] projects in fundable list are crosswalked to comprehensive, and we default the pwsid to the comprehensive list
      pwsid = pwsid.x,
      project_id = clean_numeric_string(loan_number), 
      # [keep] project name is text after dash, we default o name on comprehensive
      project_name = trimws(stringr::str_extract(applicant, "(?<=-).*")), 
      project_name = dplyr::case_when(
              is.na(project_name) ~ borrower, 
              .default = project_name),
      # [keep] Project description and Problem description variables are only present in the comprehensive list and we default to these values as all fundable projets are in comprehensive lists
      project_description = paste0("Project Description: ", proj_description, "; Problem Description: ", prob_description),
      project_type = dplyr::case_when(
            !is.na(project_type.y) ~ project_type.y, 
            grepl("lead|lsl", project_description, ignore.case = T) ~ "Lead", 
            (list %in% c("SFY25 fundable Base", "SFY25 fundable Gen Supp") & grepl("lead|lsl", project_name, ignore.case = T)) ~ "Lead", 
            grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
            (list %in% c("SFY25 fundable Base", "SFY25 fundable Gen Supp") & grepl(ec_str, project_name, ignore.case = T)) ~ "Emerging Contaminants", 
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
      expecting_funding = replace_na(expecting_funding, "No"),
      state = "Pennsylvania",
      state_fiscal_year = "2025") |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
         requested_amount, funding_amount, principal_forgiveness, population, project_description,
         disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
   
  pa_clean <- pa_clean |>
    dplyr::mutate(
      project_type = dplyr::case_when(
        project_id == "80254" ~ "General",
        project_name == "Ridley Pfas Project" ~ "Emerging Contaminants",
        project_name == "Schuylkill County Municipal Authority Morea System Improvements" ~ "Emerging Contaminants",
        project_name == "Sigsbee/Rsw Pump Stations, Valve Vault & Line Repl Profi" ~ "General",
        project_name == "Borough Of Catasauqua Water Service Billing Meter Replacements" ~ "General",
        .default = project_type
      )
    )
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  # pa_clean |> dplyr::group_by(project_id) |> dplyr::tally() |>dplyr::arrange(dplyr::desc(n))
  ####### Decision: No duplicates from funding lists
  
  # Check for disinfection byproduct in description
  # pa_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
  
  # Check for lead subtypes: Both
  # pa_clean |>
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

  ####### Decision: 4 lead projects classified as both
  
  # Check for lead subtypes: Unknown
  # pa_clean |>
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

  # one unknown --> replacement

pa_clean <- pa_clean |>
  dplyr::mutate(
    project_description = dplyr::case_when(
      project_id == "80298" ~ paste0(project_description, " | FT: LSLR"),
      .default = project_description
  ))
  
  ####### SANITY CHECKS END #######
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}