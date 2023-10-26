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
library(googlesheets4)

## Data Import ## 
## PPL Data ### 
PPL_Data <- read_csv(get_object(object = "clean_data/srf_project_priority_lists/web_ppl_combined_clean_v1-1.csv", bucket = "water-team-data"))%>%
  mutate(across('Project Type', str_replace, 'Other', 'General')) 

## Filtering down to states approved based on this document: https://docs.google.com/document/d/1sapr_7U6mLciUpS7u-Yj3g44G3S-WchaukyiA2N8e-M/edit
PPL_Data <- PPL_Data %>%
            filter(State %in% c("Alabama", "Arkansas", "District of Columbia", "Delaware", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Louisiana", "Kansas", "Massachusetts",
                                "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "North Dakota", "Nebraska", "New Jersey", "New Mexico", "Nevada", "New York", "Oklahoma",
                                "Pennsylvania", "South Dakota", "Texas", "Vermont", "West Virginia", "Wisconsin"))

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

URL <- "https://docs.google.com/spreadsheets/d/1hznoLfB8zMzs3jKfLjs-uy9R68mcFsYMAUg1gKXC17Y/edit#gid=0"

AdditionalData <- read_sheet(URL, sheet = "AdditionalData")
 # rename(`Emerging Contaminants` = "Emerging.Contaminants")
## Summary Data For Application ## 
PPL_State_Data_Geo <- PPL_Data %>%
  filter(`Funding Status` == "Funded")%>%
  mutate(Count = 1)%>%
 # left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
  mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
  mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%
  select(State, `Funding Amount`,`Principal Forgiveness`,DAC,Count)%>%
  group_by(State)%>%
  summarize_if(is.numeric,sum,na.rm = TRUE)%>%
  left_join(.,Sabs_cleaned)%>%
  left_join(.,AdditionalData, by = c("State"= "State"))%>%
  mutate(FundPer100k = (`Funding Amount`/ Population))

## Note manually addition of states which have only applicant date - TODO please switch to a smart filter/group by in next update! 
OnlyApplicant <- PPL_Data %>%
  filter(State %in% c("New York", "New Jersey", "Indiana", "Kansas"))%>%
  mutate(Count = 1)%>%
  # left_join(Geo_Data, ., by = c("NAME"= "State"))%>%
  mutate(DAC = as.numeric(ifelse(`Meets State Disadvantaged Criteria` == "Yes","1","0")))%>%
  mutate(`Principal Forgiveness` = as.numeric(`Principal Forgiveness`))%>%
  select(State, `Funding Amount`,`Principal Forgiveness`,DAC,Count)%>%
  group_by(State)%>%
  summarize_if(is.numeric,sum,na.rm = TRUE)%>%
  left_join(.,Sabs_cleaned)%>%
  left_join(.,AdditionalData, by = c("State"= "State"))%>%
  mutate(FundPer100k = (`Funding Amount`/ Population))

PPL_State_Data_Geo <- rbind(PPL_State_Data_Geo,OnlyApplicant)

## Pushing to AWS For Application ##


### To put in an AWS bucket, you need these credentials
## Contact gabe@policyinnovation.org for questions/access
## !!! NOTE !!! NEVER WRITE CREDENTIALS IN CODE, ENTER IN CONSOLE AS ENVIRONMENT VARIABLES !!!! NOTE !!!! #### 

 # Sys.setenv("AWS_ACCESS_KEY_ID" = "",
 #             "AWS_SECRET_ACCESS_KEY" = "",
 #             "AWS_DEFAULT_REGION" = "us-east-1")

# Writing to temp 

write.csv(PPL_State_Data_Geo, file.path(tempdir(), "dw-dashboard-data_v1-1.csv"), row.names = FALSE)

#Putting in Bucket
## Updating to dw_dashboard_data_v2 to include just fundable data for summary based on design.
put_object(
  file = file.path(tempdir(), "dw-dashboard-data_v1-1.csv"),
  object = "apps/dw-dashboard/dw-dashboard-data_v1-1.csv",
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









