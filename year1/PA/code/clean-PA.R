clean_pa_y1 <- function() {
  
  # NOTE: because PA does not provide a project ID and the applicant 
  # name changes across lists in unpredicatable ways, an epic_project_id
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
           project_name = str_to_title(trimws(gsub("\n", "", project_name)))) %>%
    select(-c(green, small_system, ppl_rank_dep, 
              categorical_green_or_business_case_green, 
              single_audit_required, equivalency_project, 
              total_assistance_amount, loan_number, loan))

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
                                    TRUE ~ project_name))%>%
    select(-c(street_address, project_type, fund_source, mtgdate, region, 
              pv_rating, green_category, green_project, business_case, 
              green_amount, city, county, dep_project_rating, projrank, 
              prob_description, proj_description, applicant)) 
  
  # bring it back: 
  # TODO - one project "Haines-Aaronsburg Municipal Authority Water System 2" 
  # that I can't match, and same with "Ford City Borough". Pinged at 2:33 on 
  # Nov 11th
  pa_clean <- merge(pa_comp, pa_fundable, by = "epic_merge_id",
                    all = T) %>%
    rename(disadvantaged = disadvantage_ej_area,
           project_name = project_name.x) %>%
    mutate(disadvantaged = replace_na(disadvantaged, "No Information"), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness), 
           funding_amount = clean_numeric_string(funding_amount), 
           project_type = case_when(grepl(lead_str, project_description, ignore.case = T) ~ "Lead", 
                                    grepl(ec_str, project_description, ignore.case = T) ~ "Emerging Contaminants", 
                                    list == "lead_fundable" ~ "Lead", 
                                    list == "ec_fundable" ~ "Emerging Contaminants", 
                                    TRUE ~ "General"), 
           state = "Pennsylvania",
           state_fiscal_year = "2023", 
           project_id = clean_numeric_string(project_id), 
           expecting_funding = replace_na(expecting_funding, "No")) %>%
    select(-c(project_name.y, list)) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  
  # (62,23)
  # pa_raw <- fread("year1/PA/data/38-Pennsylvania_manual_fundable_PPL.csv",
  #                 colClasses = "character", na.strings = "") %>%
  #   clean_names()
  # # 
  # 
  # # -> (62,14)
  # pa_clean <- pa_raw %>%
  #   mutate(population = clean_numeric_string(population),
  #          funding_amount = clean_numeric_string(assistance_amount),
  #          principal_forgiveness_amount = clean_numeric_string(principal_forgiveness),
  #          project_cost = clean_numeric_string(project_cost)) %>%
  #   ## split applicant name to borrower and project name
  #   ## get rid of wifta- then split at - if there is one
  #   mutate(applicant = str_to_title(applicant),
  #          applicant = gsub("wifta-|wifta -|wifta|wifta partial -", "", applicant, ignore.case = T),
  #          ## remove parenthesis and anything in between
  #          applicant = str_replace_all(applicant, "\\s*\\([^\\)]+\\)", ""),
  #          ## borrower is text before dash
  #          borrower = str_squish(str_extract(applicant, "[^-]+")),
  #          ## project name is text after dash
  #          project_name = str_squish(str_extract(applicant, "[^-]+$")),
  #          ## append PA to beginning of pwsids
  #          pwsid = paste0("PA", pwsid),
  #          ## recategorize project type column
  #          project_type = case_when(grepl("lsl", project_type, ignore.case = T) ~ "Lead",
  #                                   grepl("lead service", project_type, ignore.case = T) ~ "Lead",
  #                                   grepl("lead service", proj_description, ignore.case = T) ~ "Lead",
  #                                   grepl("pfas", proj_description, ignore.case = T) ~ "Emerging Contaminants",
  #                                   grepl("pfas", applicant, ignore.case = T) ~ "Emerging Contaminants",
  #                                   grepl("lsl", project_type, ignore.case = T) &
  #                                     grepl("pfas", proj_description, ignore.case = T) ~ "Lead,
  #                                                                                       Emerging Contaminants",
  #                                   TRUE ~ "General"),
  #          project_description = str_squish(proj_description),
  #          ## fundable column may need to be done manually because no column to join on
  #          expecting_funding = case_when(fundable == "yes" ~ "Yes",
  #                                     TRUE ~ "No"),
  #          community_served = str_to_title(city),
  #          project_rank = gsub(",", "", projrank),
  #          project_score = gsub(",", "",pv_rating),
  #          # only three EC projects have a disadvantaged distinction
  #          disadvantaged = case_when(
  #            borrower == "Susquehanna Area Reg. Airport Authority" ~ "No",
  #            project_name == "Pa American Water Company Frackville" ~ "Yes",
  #            project_name == "Saegertown Borough" ~ "No",
  #            TRUE ~ "No Information"
  #          ),
  #          state = "Pennsylvania",
  #          state_fiscal_year = "2023",
  #          project_id = as.character(NA),
  #          requested_amount = as.character(NA),
  #          principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
  #   ) %>%
  #   select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
  #          requested_amount, funding_amount, principal_forgiveness, population, project_description,
  #          disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}