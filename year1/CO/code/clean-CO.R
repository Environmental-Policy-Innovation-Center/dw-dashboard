library(tidyverse)
library(data.table)
library(janitor)

clean_co <- function() {
  
  
  ## Appendix A
  
  co_a <- fread("year1/CO/data/co-appendix-a.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    
    mutate(id = str_extract(project_number, "^.{6}"))
  
  ## Appendix B
  
  # (165,11) -> 144,10
  co_b <- fread("year1/CO/data/co-appendix-b.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    
    # remove columns with differing loan information that isn't being used for `funding_amount` to make the rows for same project distinct
    select(-approved_loan_amount, -term_yrs, -loan_type, -interest_rate, -estimated_project_cost_2) %>%
    distinct() %>%
    
    # manually remove duplicates that weren't caught by distinct
    # keep projects where the project_description is NOT NA or they are not in the list of projects to delete
    filter(!is.na(project_description) | !project_number %in% c("140521D-Q", "140951D-I", "132321D-Q", "140771D-I", "140401D-M",
                                                                "142361D-Q", "140421D-I")) %>%
    
    # fill in missing project descriptions
    mutate(
      project_description = case_when(
        project_number == "190101D-I" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "170080D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "230030D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        project_number == "230340D" ~ "Construction or Rehabilitation of Distribution and/or Transmission Lines",
        TRUE ~ project_description),
      
      project_type = "General"
    )
    
  
  ## Appendix B.1 and B.2
  
  # (104,13)
  co_b1 <- fread("year1/CO/data/co-appendix-b1.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead")
  
  # (26,13)
  co_b2 <- fread("year1/CO/data/co-appendix-b2.csv",
                 colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants")
  
  
  ## Combined Appendix B Tables
  co_b_combined <- bind_rows(co_b, co_b1, co_b2) %>%
    select(-approved_loan_amount, -term_yrs, -loan_type, -interest_rate) %>%
    mutate(id = str_extract(project_number, "^.{6}"),
           funding_status = "Funded")
  
  co_clean <- co_a %>%
    left_join(co_b_combined, by="id") %>%
    
    mutate(
      population = as.numeric(str_replace_all(population,"[^0-9.]", "")),
      funding_amount = as.numeric(str_replace_all(estimated_project_cost,"[^0-9.]", "")),
      funding_amount = replace_na(funding_amount, 0),
      project_cost = as.numeric(str_replace_all(project_cost,"[^0-9.]", "")),
    ) %>%
    mutate(
      city_served = str_squish(project_city),
      borrower = str_squish(entity),
      pwsid = str_squish(pwsid_number),
      project_name = str_squish(id),
      project_description = str_squish(project_description.x),
      project_type = replace_na(project_type, "No Information"),
      disadvantaged = case_when(
        dac == "Y" ~ "Yes",
        dac == "N" ~ "No",
        TRUE ~ "No Information"),
      state_score = str_squish(pts),
      funding_status = case_when(
        is.na(funding_status) ~ "Not Funded",
        TRUE ~ funding_status),
      state = "Colorado",
      category = "1"
    ) %>%
    select(city_served, borrower, pwsid, project_name, project_type, project_cost, funding_amount, population, 
           project_description, disadvantaged, state_score, funding_status, state, category)
  

  
  rm(list=setdiff(ls(), "co_clean"))
  
  return(NULL)

}