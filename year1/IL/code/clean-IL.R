library(tidyverse)
library(data.table)
library(janitor)

clean_il <- function() {

## Read in the manually separated versions of the PPL and lead data 
## from the excel spreadsheets sent by the state

# (70,11)
il_ppl_f <- fread("year1/IL/data/13-Illinois_PPL_Fundable.csv",
                  colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  mutate(funding_status = "Funded",
         project_type = "General",
         # replace "N/E" with 0 for principal forgiveness since N/E is different from the NAs in other docs
         disadvantaged_community_principal_forgiveness = str_replace(disadvantaged_community_principal_forgiveness, "N/E", "0")
  )

# (19,11)
il_ppl_a <- fread("year1/IL/data/13-Illinois_PPL_Applicant.csv",
                  colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  mutate(funding_status = "Not Funded",
         project_type="General")

# concat files of the same structure
# determine disadvantaged based on defined PF column, to use in merging with lead below
# (89,11)
il_ppl <- bind_rows(il_ppl_a, il_ppl_f) %>%
  mutate(disadvantaged = case_when(
    as.numeric(str_replace_all(disadvantaged_community_principal_forgiveness, "[^0-9.]", "")) > 0 ~ "Yes",
    TRUE ~ "No"))

# create (85,2) list of communities and DAC status
dacs <- il_ppl %>%
  select(loan_applicant, disadvantaged) %>%
  distinct()

# Preprocess Lead Projects
# (18,10)
il_lead_f <- fread("year1/IL/data/13-Illinois_Lead_Fundable.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  mutate(funding_status = "Funded",
         project_type = "Lead")

# (1,10)
il_lead_a <- fread("year1/IL/data/13-Illinois_Lead_Applicant.csv",
                   colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  mutate(funding_status = "Not Funded",
         project_type = "Lead")

# merge lead projects together
# (19,10)
il_lead <- bind_rows(il_lead_a, il_lead_f)

# merge lead projects with dac list
il_lead <- merge(il_lead, dacs, all.x=TRUE, by="loan_applicant") %>%
  mutate(disadvantaged = replace_na(disadvantaged, "No Information") )


# (108, 11)
il_merge <- bind_rows(il_ppl, il_lead)

# -> (108,11)
il_clean <- il_merge %>%
  # drop columns
  select(-l17_number, -estimated_construction_start) %>%
  # process numeric columns
  mutate(funding_amount = as.numeric(str_replace_all(estimated_loan_amount, "[^0-9.]", "")),
         principal_forgiveness_amount = as.numeric(str_replace_all(disadvantaged_community_principal_forgiveness, "[^0-9.]", "")),
         population = as.numeric(str_replace_all(service_population, "[^0-9.]", "")),
  ) %>%
  # process text columns
  mutate(borrower = str_squish(loan_applicant),
         borrower = str_to_title(borrower, locale="en"),
         state_score = str_replace_all(loan_priority_score, "[^0-9.]", ""),
         project_description = str_squish(project_description),
         project_description = str_to_sentence(project_description),
         state = "Illinois",
         category = "3"
  ) %>%
  # rename columns
  rename(pwsid = facility_no) %>%
  # keep relevant columns
  select(state_score, borrower, pwsid, project_description, population, disadvantaged, funding_amount, principal_forgiveness_amount, project_type, funding_status, state, category)

rm(list=setdiff(ls(), "il_clean"))

return(il_clean)
}