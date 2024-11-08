source("resources.R")

clean_hi_y1 <- function() {
  
  # (33,8)
  # note this if from the july 22 IUP, but it is the same in the March 23 amended IUP
  # https://health.hawaii.gov/sdwb/files/2023/03/SFY2023.IUP_Amendment1_2023.03.pdf
  hi_fund <- fread("year1/HI/data/11-Hawaii_PPL.csv",
                   colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # -> (33,10)
  hi_clean <- hi_fund %>%
    ## process numeric columns
    mutate(population = clean_numeric_string(population),
           requested_amount = clean_numeric_string(estimated_eligible_amount),
           # when project is in funded list, treat estimated_eliglbe_amount as funding_amount as well as requested_amount
           funding_amount = case_when(
             priority_ranking %in% c("1","2","6","11","13","16","20","21","29","33") ~ requested_amount,
             TRUE ~ "No Information"
           ),
           ## process text columns       
           # manually add disadvantaged based on subscript from IUP that couldn't be scraped
           # denoted by "E" footnote subscript in fundable list, table 7 in IUP
           # projects that are listed are either DAC or not, but full applicant list is No Information because they don't denote disadvantaged in the full table in Appendix A
           disadvantaged = case_when(priority_ranking %in% c("1","2","11","13","20","21") ~ "Yes",
                                     priority_ranking %in% c("6", "16", "29", "33") ~ "No",
                                     TRUE ~ "No Information"),
           project_name = str_squish(project_name),
           project_id = str_squish(project_number),
           borrower = str_squish(borrower),
           project_rank = str_squish(priority_ranking),
           # manually mark projects as funded from amended IUP
           expecting_funding = case_when(funding_amount > 0 ~ "Yes",
                                      TRUE ~ "No"),
           project_type = "General",
           state = "Hawaii",
           state_fiscal_year = "2023",
           community_served = as.character(NA),
           pwsid = as.character(NA),
           project_cost = as.character(NA),
           principal_forgiveness = as.character(NA),
           project_description = as.character(NA),
           project_score = as.character(NA),
    )
  
  # amendments - in the march 2023 IUP, a number of changes are highlighted in yellow
  hi_clean <- hi_clean %>%
    mutate(
      # manually modify funding amounts that are highlighted yellow in amendment table
      funding_amount = case_when(
        project_rank == "1"  ~ "10000000",
        project_rank == "11" ~ "1572484.20",
        project_rank == "12" ~ "225964",
        project_rank == "20" ~ "1770889.20",
        project_rank == "21" ~ "165867.30",
        project_rank == "29" ~ "459731.84",
        # remove three projects scratched through
        project_rank %in% c("2", "6", "33") ~ "No Information",
        TRUE ~ funding_amount),
      # manually modifying yellow highlighted project descriptions
      project_name = case_when(
        project_rank == "21" | project_rank == "29" ~ "Backup Generator and SCADA Upgrades",
        TRUE ~ project_name),
      expecting_funding = case_when(
        # set three scratched through projects as not funded
        project_rank %in% c("2", "6", "33") ~ "No",
        TRUE ~ expecting_funding)
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  run_tests(hi_clean)
  rm(list=setdiff(ls(), "hi_clean"))
  
  return(hi_clean)
}