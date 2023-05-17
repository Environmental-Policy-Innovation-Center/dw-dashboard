# Data Cleaning Script for DW Dashboard  # 

## Pulls in ## 
## Sabs Data 
## PPL Data 
## DWINSA Data
## PDF Documentation Data 
## Cleans and hosts all on AWS for Application ## 
library(tidyverse)
library(aws.s3)
library(googledrive)


## Data Import ## 
## PPL Data ### 
PPL_Data <- read_csv(get_object(object = "clean_data/srf_project_priority_lists/web_ppl_combined_clean_v3.csv", bucket = "water-team-data"))%>%
  mutate(across('Project Type', str_replace, 'Other', 'General'))

## SABs Data ### 
Sabs_raw <- read_csv(get_object(object = "service_area_boundaries/sabs_app/Dev_Data_v1.csv", bucket = "tech-team-data"))

Sabs_cleaned <- Sabs_raw %>%
  mutate(Population = pop_tier1 + pop_tier2 + pop_tier3)%>%
  mutate(Systems = num_sys_tier1 + num_sys_tier2 + num_sys_tier3)%>%
  rename(State = State_Name)%>%
  select(State,Population,Systems)

## Additional Data 
## TO DO: Put this on AWS ## 
## Sourced from Attachment A DWISNA Allotments and Pipe Estimates: https://www.epa.gov/system/files/documents/2023-04/Final_FY23%20DWSRF%20Allotment%20Memo%20and%20Attachments_April%202023.pdf
## Sourced from the 20-year need (page 9 + 10): https://www.epa.gov/system/files/documents/2023-04/Final_DWINSA%20Public%20Factsheet%204.4.23.pdf
AdditionalData <- read.csv("www/AdditionalData-v4.csv", stringsAsFactors = FALSE)%>%
  rename(`Emerging Contaminants` = "Emerging.Contaminants")

## Data For Application ## 
PPL_State_Data_Geo <- PPL_Data %>%
  mutate(Count = 1)%>%
 # left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
  mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
  mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%
  select(State, `Funding Amount`,`Principal Forgiveness`,DAC,Count)%>%
  group_by(State)%>%
  summarize_if(is.numeric,sum)%>%
  left_join(.,Sabs_cleaned)%>%
  left_join(.,AdditionalData, by = c("State"= "State"))%>%
  mutate(FundPer100k = (`Funding Amount`/ Population))

## Pushing to AWS For Application ##


### To put in an AWS bucket, you need these credentials
## Contact gabe@policyinnovation.org for questions/access
## !!! NOTE !!! NEVER WRITE CREDENTIALS IN CODE, ENTER IN CONSOLE AS ENVIRONMENT VARIABLES !!!! NOTE !!!! #### 


# Writing to temp 

write.csv(PPL_State_Data_Geo, file.path(tempdir(), "dw-dashboard-data.csv"), row.names = FALSE)

#Putting in Bucket
put_object(
  file = file.path(tempdir(), "dw-dashboard-data.csv"),
  object = "apps/dw-dashboard/dw-dashboard-data.csv",
  bucket = "water-team-data",
  acl = "public-read"
)


### PPL Parsing ###
## TO DO ## 
## Pull in data dictionaries from Google Drive 
## Link:..
## Split into 50 pdfs
## Run through loop of all states and put in AWS Folder 
## Aws folder: s3://water-team-data/apps/dw-dashboard/state-data-dict/

# GET FROM DRIVE
# SPLIT INTO 50 
# For Each State 
# Post to AWS folder-root-name/state-name-data-dict.pdf 








