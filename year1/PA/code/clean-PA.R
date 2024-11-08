source("resources.R")

clean_pa <- function() {
  
  # (62,23)
  pa_raw <- fread("year1/PA/data/38-Pennsylvania_manual_fundable_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (62,14)
  pa_clean <- pa_raw %>%
    mutate(population = clean_numeric_string(population),
           funding_amount = clean_numeric_string(assistance_amount),
           principal_forgiveness_amount = clean_numeric_string(principal_forgiveness),
           project_cost = clean_numeric_string(project_cost)
    ) %>%
    ## split applicant name to borrower and project name
    ## get rid of wifta- then split at - if there is one
    mutate(applicant = str_to_title(applicant),
           applicant = gsub("wifta-|wifta -|wifta|wifta partial -", "", applicant, ignore.case = T),
           ## remove parenthesis and anything in between
           applicant = str_replace_all(applicant, "\\s*\\([^\\)]+\\)", ""),
           ## borrower is text before dash
           borrower = str_squish(str_extract(applicant, "[^-]+")),
           ## project name is text after dash
           project_name = str_squish(str_extract(applicant, "[^-]+$")),
           ## append PA to beginning of pwsids
           pwsid = paste0("PA", pwsid),
           ## recategorize project type column
           project_type = case_when(grepl("lsl", project_type, ignore.case = T) ~ "Lead",
                                    grepl("lead service", project_type, ignore.case = T) ~ "Lead",
                                    grepl("lead service", proj_description, ignore.case = T) ~ "Lead",
                                    grepl("pfas", proj_description, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("pfas", applicant, ignore.case = T) ~ "Emerging Contaminants",
                                    grepl("lsl", project_type, ignore.case = T) &
                                      grepl("pfas", proj_description, ignore.case = T) ~ "Lead,
                                                                                        Emerging Contaminants",
                                    TRUE ~ "General"),
           project_description = str_squish(proj_description),
           ## fundable column may need to be done manually because no column to join on
           expecting_funding = case_when(fundable == "yes" ~ "Yes",
                                      TRUE ~ "No"),
           community_served = str_to_title(city),
           project_rank = gsub(",", "", projrank),
           project_score = gsub(",", "",pv_rating),
           # only three EC projects have a disadvantaged distinction
           disadvantaged = case_when(
             borrower == "Susquehanna Area Reg. Airport Authority" ~ "No",
             project_name == "Pa American Water Company Frackville" ~ "Yes",
             project_name == "Saegertown Borough" ~ "No",
             TRUE ~ "No Information"
           ),
           state = "Pennsylvania",
           state_fiscal_year = "2023",
           project_id = as.character(NA),
           requested_amount = as.character(NA),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(pa_clean)
  rm(list=setdiff(ls(), "pa_clean"))
  
  return(pa_clean)
}