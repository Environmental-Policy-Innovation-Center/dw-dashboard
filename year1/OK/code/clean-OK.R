library(tidyverse)
library(data.table)
library(janitor)

clean_ok <- function() {
  
  
  ## base / bil
  # (69,12) -> (69,14)
  ok_b <- fread("year1/OK/data/36-Oklahoma_PPL-AppB.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "General") %>%
    rename(disadvantaged = severly_disadvantged_disadvantged_or_no)
  
  ok_b$funding_status <- ""
  ok_b$funding_status[1:46] <- "Funded"
  ok_b$funding_status[47:69] <- "Not Funded"
  
  # for fundable projects, loan amount is funding amount
  ok_b <- ok_b %>%
    mutate(funding_amount = case_when(
      funding_status == "Funded" ~ as.numeric(str_replace_all(loan_amount,"[^0-9.]","")),
      TRUE ~ 0
    ))
  
  ## lead
  # (42,11) -> (42,13)
  ok_e <- fread("year1/OK/data/36-Oklahoma_PPL-AppE.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead") %>%
    rename(disadvantaged = disadvantaged_y_or_n)
  
  # only first 24 projects are fundable 
  ok_e$funding_status <- ""
  ok_e$funding_status[1:24] <- "Funded"
  ok_e$funding_status[25:42] <- "Not Funded"
  
  # for fundable projects, loan amount is funding amount
  ok_e <- ok_e %>%
    mutate(funding_amount = case_when(
      funding_status == "Funded" ~ as.numeric(str_replace_all(loan_amount,"[^0-9.]","")),
      TRUE ~ 0
    ))
  
  
  ## emerging contaminants
  # (3,11) -> (3,13)
  ok_f <- fread("year1/OK/data/36-Oklahoma_PPL-AppF.csv",
                colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           funding_status = "Funded",
           # all projects on EC list are funded
           funding_amount = as.numeric(str_replace_all(loan_amount,"[^0-9.]",""))) %>%
    rename(disadvantaged = disadvantaged_y_or_n)
  
  
  # (114, 12)
  ok_combined <- bind_rows(ok_b, ok_e, ok_f)
  
  # -> (114,10)
  ok_clean <- ok_combined %>%
    # drop columns
    select(-base, -cumulative_amount, -anticipated_binding_commitment_date, -anticipated_construction_date) %>%
    # process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           # for all projects on all lists, loan amount is requested amount, even if it is also funding_amount for funded projects
           requested_amount = as.numeric(str_replace_all(loan_amount,"[^0-9.]","")),) %>%
    # process text columns
    mutate(state_score = case_when(
      priority_points == "Being Ranked" ~ "No Information",
      TRUE ~ str_squish(priority_points)),
      borrower = str_squish(system),
      project_description = str_squish(project_description),
      project_name = str_squish(project_number),
      # disadvantaged can either be S/D/Y for some degree of disadvantaged or N for No
      disadvantaged = case_when(
        disadvantaged == "N" ~ "No",
        TRUE ~ "Yes"),
      state = "Oklahoma",
      category = "1",
    ) %>%
    select(borrower, state_score, project_name, project_description, requested_amount, funding_amount,
           disadvantaged, population, project_type, state, category, funding_status)
  
  
  rm(list=setdiff(ls(), "ok_clean"))
  
  return(ok_clean)
}