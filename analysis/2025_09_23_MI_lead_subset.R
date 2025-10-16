all_projects <- s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") %>%
  clean_names()

all_projects |>
  dplyr::filter(state=="Michigan" & project_type =="Lead") |>
  dplyr::mutate(lead_type = case_when(
   stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
   stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
   # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
   stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
   stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
    TRUE ~ "other"
  )) |>
  dplyr::select("project_id", "project_description", "lead_type" ,"state_fiscal_year") |>
  googlesheets4::sheet_write(ss="https://docs.google.com/spreadsheets/d/14IFnI5ojdclDh1z1RElTuQnir2XNzdvkIiDUgpMyvt4", sheet = "updated")
