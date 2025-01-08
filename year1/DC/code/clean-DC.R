library(tidyverse)
library(data.table)
library(janitor)

clean_dc_y1 <- function() {

  # 13,3
  dc_ppl <- fread("year1/DC/data/DistrictofColumbia_PPL.csv",
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (13,5)
  dc_clean <- dc_ppl %>%
    mutate(
      project_description = str_squish(project),
      funding_amount = as.numeric(str_replace_all(grant_funding,"[^0-9.]","")),
      state = "District of Columbia",
      project_type = "General",
      funding_status = "Funded",
      category = "1"
    ) %>%
    select(project_description, funding_amount, project_type, state, funding_status, category)
  
  rm(list=setdiff(ls(), "dc_clean"))
  
  return(dc_clean)
  }