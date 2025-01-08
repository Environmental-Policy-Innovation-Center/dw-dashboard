source("resources.R")

clean_az_y1 <- function() {
  
  # (54,9)
  # ppl is most up to date, but iup has columns not otherwise included - pwsid and project type
  az_iup <- fread("year1/AZ/data/az-sfy23-iup-revision2.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_number = str_replace_all(project_number, "\\r", " "),
           project_type = case_when(
             grepl("2", applicant) ~ "Lead",
             grepl("3", applicant) ~ "Emerging Contaminants",
             TRUE ~ "General")) %>%
    select(project_number, pws_number, project_type)
    
  # -> (49,3)
  az_ppl <- fread("year1/AZ/data/az-ppl-sfy23-revised-ppl.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (54,11)
  az_clean <- az_ppl %>%
    left_join(az_iup, by=c("project_number")) %>%
    mutate(
           # get rid of linebreak character, then for projects with multiple $ values, remove the second
           amount_requested_probable_green_amount = str_replace_all(amount_requested_probable_green_amount, "\\r", " "),
           amount_requested_probable_green_amount = str_replace_all(amount_requested_probable_green_amount, "/.*", ""),
           requested_amount = clean_numeric_string(amount_requested_probable_green_amount),
           funding_amount = case_when(
             grepl("4", applicant) ~ requested_amount,
             TRUE ~ "No Information"),
           project_cost = as.character(NA),
           principal_forgiveness = as.character(NA)
           ) %>%
    mutate(
      # remove as much of the 1,3 footnotes from the borrowers, then clean up by removing numbers not caught in the regex
      borrower = str_replace_all(applicant, "\\s*\\d+\\s*,\\s*\\d+\\s*", ""),
           borrower = str_replace_all(borrower, "4", ""),
           borrower = str_replace_all(borrower, "1", ""),
           borrower = str_replace_all(borrower, ",", ""),
           borrower = str_squish(borrower),
           # fix applicant that gets chopped up because of above mutations
           borrower = case_when(
             ppl_rank == "1" ~ "Sun Valley Farms Unit VI Water Company, Inc.",
             TRUE ~ borrower),
           pwsid = str_squish(pws_number),
           pwsid = replace_na(pwsid, "No Information"),
           population = clean_numeric_string(population),
           project_id = str_squish(project_number),
           project_name = as.character(NA),
           project_type = case_when(
            is.na(project_type) ~ "General",
            TRUE ~ project_type),
           project_description = str_squish(description),
           disadvantaged = case_when(
             grepl("1", applicant) ~ "Yes",
             TRUE ~ "No"),
           project_rank = str_squish(ppl_rank),
           expecting_funding = ifelse(funding_amount > 0, "Yes", "No"),
           state = "Arizona",
           state_fiscal_year = "2023",
      community_served = as.character(NA),
      project_score = as.character(NA)
      ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(az_clean)
  rm(list=setdiff(ls(), "az_clean"))
  
  return(az_clean)
}