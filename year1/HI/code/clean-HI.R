library(tidyverse)
library(data.table)
library(janitor)

clean_hi <- function() {
  
  # (33,8)
  # note this if from the july 22 IUP, but it is the same in the March 23 amended IUP
  # https://health.hawaii.gov/sdwb/files/2023/03/SFY2023.IUP_Amendment1_2023.03.pdf
  hi_fund <- fread("year1/HI/data/11-Hawaii_PPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (33,10)
  hi_clean <- hi_fund %>%
    ## process numeric columns
    mutate(population = as.numeric(str_replace_all(population,"[^0-9.]","")),
           requested_amount = as.numeric(str_replace_all(estimated_eligible_amount,"[^0-9.]", "")),
           # when project is in funded list, treat estimated_eliglbe_amount as funding_amount as well as requested_amount
           funding_amount = case_when(
             priority_ranking %in% c("1","2","6","11","13","16","20","21","29","33") ~ requested_amount,
             TRUE ~ 0
           ),
           ## process text columns       
           # manually add disadvantaged based on subscript from IUP that couldn't be scraped
           # denoted by "E" footnote subscript in fundable list, table 7 in IUP
           # projects that are listed are either DAC or not, but full applicant list is No Information because they don't denote disadvantaged in the full table in Appendix A
           disadvantaged = case_when(priority_ranking %in% c("1","2","11","13","20","21") ~ "Yes",
                                     priority_ranking %in% c("6", "16", "29", "33") ~ "No",
                                     TRUE ~ as.character(NA)),
           project_name = str_squish(project_name),
           borrower = str_squish(borrower),
           state_rank = str_squish(priority_ranking),
           # manually mark projects as funded from amended IUP
           funding_status = case_when(funding_amount > 0 ~ "Funded",
                                      TRUE ~ "Not Funded"),
           project_type = "General",
           state = "Hawaii",
           category = "1"
    ) %>%
    ## keep relevant columns
    select(borrower, project_name,
           state_rank, requested_amount, funding_amount, project_type,
           disadvantaged, population, funding_status, state, category)
  
  # amendments - in the march 2023 IUP, a number of changes are highlighted in yellow
  hi_clean <- hi_clean %>%
    mutate(
      # manually modify funding amounts that are highlighted yellow in amendment table
      funding_amount = case_when(
        state_rank == "1"  ~ 10000000,
        state_rank == "11" ~ 1572484.20,
        state_rank == "12" ~ 225964,
        state_rank == "20" ~ 1770889.20,
        state_rank == "21" ~ 165867.30,
        state_rank == "29" ~ 459731.84,
        # remove three projects scratched through
        state_rank %in% c("2", "6", "33") ~ 0,
        TRUE ~ funding_amount),
      # manually modifying yellow highlighted project descriptions
      project_name = case_when(
        state_rank == "21" | state_rank == "29" ~ "Backup Generator and SCADA Upgrades",
        TRUE ~ project_name),
      funding_status = case_when(
        # set three scratched through projects as not funded
        state_rank %in% c("2", "6", "33") ~ "Not Funded",
        TRUE ~ funding_status)
    )
  
  rm(list=setdiff(ls(), "hi_clean"))
  
  return(hi_clean)
}