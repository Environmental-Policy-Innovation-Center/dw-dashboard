source("resources.R")

clean_mi_y0 <- function() {
  

  mi_iup <- read.csv("year0/MI/data/mi-fy22-iup-final.csv") %>%
    clean_names()
  
  mi_clean <- mi_iup %>%
    filter(!is.na(rank)) %>%
    mutate(community_served = str_squish(county),
           borrower = str_squish(project_name),
           project_id = str_squish(project),
           project_type = ifelse(grepl("LSLR", project_description), "Lead", "No Information"),
           requested_amount = clean_numeric_string(project_amount),
           #TODO: Confirm how this should be treated when WIFTA funds are involved
           funding_amount = "1",
           principal_forgiveness = clean_numeric_string(total_principal_forgiveness_non_wifta),
           population = clean_numeric_string(population),
           project_description = str_squish(project_description),
          #TODO: Confirm whether the blank entries should be listed as No or No Info
           disadvantaged = str_squish(disadvantaged_community),
          project_rank = str_squish(rank),
          project_score = str_squish(total_points),
          #TODO: Confirm this works based on clarified definition above
          expecting_funding = ifelse((funding_amount != "0" & funding_amount != "No Information"),
                                        "Yes", "No"),
          project_name = as.character(NA),
          project_cost = as.character(NA),
          pwsid = as.character(NA),
           state = "Michigan",
           state_fiscal_year = "2022"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
  
  
  run_tests(mi_clean)
  rm(list=setdiff(ls(), "mi_clean"))
  
  return(mi_clean)
}
