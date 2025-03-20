source("resources.R")

clean_tn_y2 <- function() {
  
  # (70, 18) - gen PPL
  # TODO - check to see how to handle the project with multiple pwsids listed 
  # asked Lauren on 3/20 at 8:23am 
  gen_ppl <- fread("year2/TN/data/tn_ppl.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names() 
  
  # (70, 18)
  tn_clean <- gen_ppl %>%
    mutate(community_served = str_squish(county),
           # removing weird characters from local government column
           borrower = str_replace_all(local_government, "\\*", ""),
           borrower = str_replace_all(borrower, "\\+", ""),
           borrower = str_squish(borrower),
           pwsid = str_squish(pwsid_number), 
           project_id = as.character(NA),
           project_name = as.character(NA), 
           project_type = case_when(grepl("lead", project_description, ignore.case = TRUE) ~ "Lead", 
                                    grepl("PFAS|PFOS|Emerging.Contaminants", 
                                          project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
                                    TRUE ~ "General"),            
           project_cost = clean_numeric_string(total_project_amount), 
           requested_amount = as.character(NA), 
           funding_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = str_squish(project_description), 
           population = clean_numeric_string(pop_served), 
           disadvantaged = case_when(grepl("\\*", local_government) ~ "Yes",
                                     TRUE ~ "No"),
           project_rank = replace_na(rank_order, "No Information"), 
           project_score = replace_na(priority_points, "No Information"),
           expecting_funding = case_when(project_rank == "No Information" ~ "No", 
                                         TRUE ~ "Yes"), 
           state = "Tennessee",
           state_fiscal_year = "2024")%>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  run_tests(tn_clean)
  rm(list=setdiff(ls(), "tn_clean"))
  
  return(tn_clean)
}
