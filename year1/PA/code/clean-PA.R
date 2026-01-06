clean_pa_y1 <- function() {
  
  # NOTE: because PA does not provide a project ID and the applicant 
  # name changes across lists in unpredictable ways, an epic_project_id
  # was created to match projects on the fundable list to the comprehensive 
  # list 
  
  # fundable list 
  pa_fundable <- fread("year1/PA/data/pa-y1-fundable-project-id.csv",
                       colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(funding_amount = clean_numeric_string(total_assistance_amount), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           expecting_funding = "Yes", 
           project_id = clean_numeric_string(loan_number), 
           # prepping to merge by project name
           project_name = str_to_title(trimws(gsub("\n", "", project_name)))) 

  # comp list: 
  pa_comp <- fread("year1/PA/data/pa-ppl-project-id.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    # clean string columns: 
    mutate(community_served = paste0(str_to_title(city), ", ", 
                                     str_to_title(county), " County"), 
           pwsid = paste0("PA", trimws(pwsid)), 
           project_description = paste0("Project Description: ", proj_description, 
                                        "; Problem Description: ", prob_description), 
           project_description = gsub("\n", "", project_description), 
           # clean numeric columns: 
           project_cost = clean_numeric_string(project_cost), 
           project_score = clean_numeric_string(dep_project_rating),
           project_rank = clean_numeric_string(projrank),
           population = clean_numeric_string(population),
           requested_amount = as.character(NA)) %>%
    mutate(applicant = trimws(applicant), 
           applicant =  gsub("\n", "", applicant)) %>%
    # this is so gnarly - editing applicant so it splits better: 
    mutate(applicant = case_when(applicant == "SOUTHWEST WARREN COUNTY MUNICIPAL AUTHORITY (SWCMA) UPGRADE" ~ "SOUTHWEST WARREN COUNTY MUNICIPAL AUTHORITY - (SWCMA) UPGRADE", 
                                 applicant == "MCCONNELLSBURG BOROUGH MUNICIPAL AUTH WTP DISINFECTION IMP." ~ "MCCONNELLSBURG BOROUGH MUNICIPAL AUTH - WTP DISINFECTION IMP.", 
                                 applicant == "KNOX TWP MA WATER TREATMENT PLANT & SYSTEM UPGRADES PROJECT" ~ "KNOX TWP MA - WATER TREATMENT PLANT & SYSTEM UPGRADES PROJECT", 
                                 applicant == "HAZLETON CITY AUTHORITY LEHIGH RIVER PUMP STATION REPAIR" ~ "HAZLETON CITY AUTHORITY - LEHIGH RIVER PUMP STATION REPAIR", 
                                 applicant == "CURRYVILLE WATER AUTHORITY INTERCONNECTION AND WATER TANK REHAB" ~ "CURRYVILLE WATER AUTHORITY - INTERCONNECTION AND WATER TANK REHAB", 
                                 applicant == "LINESVILLE BORO. WEST ERIE ST. LEAD LINE ABATEMENT. WIFTA" ~ "LINESVILLE BORO. - WEST ERIE ST. LEAD LINE ABATEMENT.",
                                 applicant == "HAZLETON CITY AUTHORITY SR 940 UPGRADE PROJECT" ~ "HAZLETON CITY AUTHORITY - SR 940 UPGRADE  PROJECT", 
                                 applicant == "WIFTA - RIMERSBURG BOROUGH M. A. LEAD LINE REPLACEMENT PROJECT" ~ "RIMERSBURG BOROUGH M.A. - LEAD LINE REPLACEMENT PROJECT",
                                 applicant == "WIFTA MINERSVILLE LEAD LINE REPLACEMENT PROJECT" ~ "WIFTA MINERSVILLE - LEAD LINE  REPLACEMENT PROJECT",
                                 applicant == "WIFTA - REDBANK VALLEY MUNICIPAL AUTHORITY METER REPLACEMENT" ~ "REDBANK VALLEY MUNICIPAL AUTHORITY - METER REPLACEMENT",
                                 applicant == "WIFTA POTTSTOWN BOROUGH AUTHORITY 2021 LEAD SERVICE REPL." ~ "WIFTA POTTSTOWN BOROUGH AUTHORITY - 2021 LEAD SERVICE REPL.",
                                 applicant == "AQUA PENNSYLVANIA EDGELY (BRISTOL)" ~ "AQUA PENNSYLVANIA - EDGELY (BRISTOL)",
                                 applicant == "AQUA PENNSYLVANIA CHALFONT WELL NO 11 TREATMENT" ~ "AQUA PENNSYLVANIA - CHALFONT WELL NO 11 TREATMENT",
                                 applicant == "AQUA PENNSYLVANIA HATBORO" ~ "AQUA PENNSYLVANIA - HATBORO",
                                 applicant == "DOYLESTOWN TOWNSHIP MUNICIPAL AUTHORITY (DTMA) MAIN" ~ "DOYLESTOWN TOWNSHIP MUNICIPAL AUTHORITY (DTMA) - MAIN", 
                                 TRUE ~ applicant)) %>%
    # extract based on presence of hyphen: 
    mutate(applicant = str_to_title(applicant),
           applicant = gsub("wifta-|wifta -|wifta|wifta partial -", "", applicant, ignore.case = T),
           ## borrower is text before dash
           borrower = trimws(str_extract(applicant, "[^-]+")), 
           ## project name is text after dash
           project_name = trimws(str_extract(applicant, "(?<=-).*")), 
           # note there are only 3 instances where the applicant was just 
           # "Saegertown Borough", or "Emmaus Borough", etc. and therefore
           # the actual project name was not present. 
           project_name = case_when(is.na(project_name) ~ borrower, 
                                    TRUE ~ project_name))
  
  # one project "Haines-Aaronsburg Municipal Authority Water System 2" 
  # appears on the fundable list but not the comp list - prepping it 
  # for an rbind: 
  missing_fundable_project <- pa_fundable %>%
    filter(epic_merge_id == "?") %>%
    mutate(borrower = "Haines-Aaronsburg Municipal Authority", 
           project_name = "Water System 2 Improvement Project", 
           disadvantaged = "No Information", 
           project_type = "General", 
           requested_amount = as.character(NA), 
           project_description = "No Information", 
           # clean numeric columns: 
           project_cost = "No Information", 
           project_score = "No Information",
           project_rank = "No Information",
           population ="No Information", 
           pwsid = "No Information", 
           state = "Pennsylvania",
           state_fiscal_year = "2023", 
           community_served = "No Information") %>%
    select(-c(list, epic_merge_id, disadvantage_ej_area))
  
  # bring it back: 
  pa_clean <- merge(pa_comp, pa_fundable, by = "epic_merge_id",
                    all = T) %>%
    filter(epic_merge_id != "?") %>%
    rename(disadvantaged = disadvantage_ej_area,
           project_name = project_name.x) %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information"), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           funding_amount = clean_numeric_string(funding_amount), 
           project_type = case_when(grepl(lead_str, project_description, ignore.case = T) ~ "Lead", 
                                    grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
                                    # the presence of a project on a list should 
                                    # override the string matches above
                                    list == "lead_fundable" ~ "Lead", 
                                    list == "ec_fundable" ~ "Emerging Contaminants", 
                                    TRUE ~ "General"), 
           state = "Pennsylvania",
           state_fiscal_year = "2023", 
           project_id = clean_numeric_string(project_id), 
           expecting_funding = replace_na(expecting_funding, "No")) 
  
  # bring back that missing fundable project
  pa_clean <- bind_rows(pa_clean, missing_fundable_project) %>%
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

  ####### Decision: none classified as both
  
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

  # seven unknown 

  ####### SANITY CHECKS END #######

  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}