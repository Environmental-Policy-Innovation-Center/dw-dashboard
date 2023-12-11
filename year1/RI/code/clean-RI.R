library(tidyverse)
library(data.table)
library(janitor)

clean_ri <- function() {
  
  ri_comp <- fread("year1/RI/data/39-RhodeIsland_PPL_Comprehensive.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    # drop columns
    select(-a, -b, -c, -d, -e, -f, -g, -source_fund, -new, -est_start_date) %>%
    # process numeric columns
    mutate(
      funding_amount = as.numeric(str_replace_all(funds_requested,"[^0-9.]", "")),
      population = as.numeric(str_replace_all(pop_served,"[^0-9.]", "")),
      
    ) %>%
    # process text columns
    mutate(borrower = str_squish(system_name),
           borrower = str_remove(borrower, "\\*"),
           state_score = str_replace_all(scores_total,"[^0-9.]", ""),
           pwsid = paste0("RI", pws_id),
           project_description = str_squish(project_description),
           state = "Rhode Island",
           category = ""
    )
  
  ri_fund <- fread("year1/RI/data/39-RhodeIsland_PPL_Fundable_Manual.csv",
                   colClasses = "character", na.strings = "") %>% 
    clean_names() %>%
    mutate(funding_amount = as.numeric(str_replace_all(loan_amount,"[^0-9.]", "")),
           funding_status = "Funded",
           borrower = str_remove(borrower, "\\*")) %>%
    select(-loan_amount) %>%
    left_join(ri_comp, by="borrower")
  
  rm(list=setdiff(ls(), "ri_clean"))
  
  return(NULL)
}