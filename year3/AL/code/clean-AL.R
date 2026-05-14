clean_al_y3 <- function() {
  base_path <- "year3/AL/data"
  
 # list files
  PPL_files <- stringr::str_subset(list.files("year3/AL/data"), "curated")

  # read file
  priority_list <- dplyr::tibble()
  for (file_name in PPL_files){
    tmp_priority_list <- data.table::fread(paste0("year3/AL/data/", file_name), colClasses = "character", na.strings = "") |> 
      janitor::clean_names() |>
      dplyr::mutate(
        source_list="Priority",
        list = dplyr::case_when(
          stringr::str_detect(file_name, "_BIL") ~ "SFY25 BIL Gen Supp PPL",
          stringr::str_detect(file_name, "_LSL") ~ "SFY25 LSLR PPL",
          stringr::str_detect(file_name, "_EC") ~ "SFY25 EC PPL",
          TRUE ~ "SFY25 Base PPL"
        )
      )
    priority_list<- priority_list |>
      dplyr::bind_rows(tmp_priority_list)
  }
 
  al_clean <- priority_list |>
    dplyr::mutate(
      community_served = stringr::str_squish(city_town),
      borrower = stringr::str_squish(applicant_name),
      pwsid = as.character(NA),
      project_id = project_number,
      project_name = stringr::str_squish(attachment_project_title)
    )|>
    dplyr::mutate(
      project_type = dplyr::case_when(
        list == "SFY25 LSLR PPL" ~ "Lead",
        list == "SFY25 EC PPL" ~ "Emerging Contaminants",
        grepl("lsl|lead", attachment_project_description, ignore.case = TRUE) ~ "Lead",
        grepl("BIL LSL", fund, ignore.case = TRUE) ~ "Lead", 
        grepl(ec_str, attachment_project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      )
    )|>
    dplyr::mutate(
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(applied_for_project_amount)
    )|>
    dplyr::mutate(
      funding_amount = dplyr::case_when(
        list =="SFY25 Base PPL" ~ dw_srf_amount_granted,
        list == "SFY25 BIL Gen Supp PPL" ~ dw_bil_amount_granted,
        list == "SFY25 LSLR PPL" ~ dw_bil_lead_amount_granted,
        list =="SFY25 EC PPL" ~ dw_bil_ec_amount_granted  
      ), 
      funding_amount = clean_numeric_string(funding_amount)
    ) |>
    dplyr::mutate(
      principal_forgiveness = dplyr::case_when(
        list == "SFY25 Base PPL" ~ dw_srf_amount_of_pf,
        list == "SFY25 BIL Gen Supp PPL" ~ dw_bil_amount_of_pf,
        list == "SFY25 LSLR PPL" ~ dw_bil_lsl_amount_of_pf,
        list == "SFY25 EC PPL" ~ dw_bil_ec_amount_granted  
        ),
      principal_forgiveness = clean_numeric_string(principal_forgiveness)
    ) |>
    dplyr::mutate(
      population = clean_numeric_string(population),
      project_description = stringr::str_squish(attachment_project_description)
    ) |>
    dplyr::mutate(
      disadvantaged = dplyr::case_when( ##confirm this is supposed to be 
        as.numeric(disadvantaged_score) > 1 ~ "Yes",
        as.numeric(disadvantaged_score) < 1 ~ "No",
        is.na(disadvantaged_score) ~ "No Information",
        disadvantaged_score == "SUPP" ~ "No Information"
      )
    ) |>
    dplyr::mutate(
      project_rank = as.character(NA) ,
      project_score = priority_ranking_points,
      project_score = ifelse(project_score == "SUPP", "No Information", project_score),
      expecting_funding = "Yes",
      state = "Alabama",
      state_fiscal_year = "2025"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
      requested_amount, funding_amount, principal_forgiveness, population, project_description,
      disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)

    # # there is one project number also in al_supp but a mismatch in applicant
    # # name: 
    # mutate(applicant_name = case_when(
    #   project_number == "FS010096-11" ~ paste0(applicant_name, "**"), 
  
    # # there is one overlapping lsl project: "FS010488-02" but it has a different 
    # # project name, confirmed it should be listed twice

####### SANITY CHECKS #######
# Hone in on project id duplication
# al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

# al_clean |>
#   dplyr::filter(project_id == "FS010096-11") 
####### Decision: Keep both as separate projects, they have different descriptions

# al_clean |>
#   dplyr::filter(project_id == "FS010488-02") 
####### Decision: Keep both as separate projects, per Janet's request in Data Dictionary 
 
# Check for disinfection byproduct in description
# al_clean |>
#   dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string
 
# Check for lead subtypes: Both
# al_clean |>
#   dplyr::filter(project_type=="Lead") |>
#   dplyr::mutate(
#     lead_type = dplyr::case_when(
#       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#       TRUE ~ "unknown"
#     )
#   ) |>
#   dplyr::filter(lead_type == "both")

####### Decision: No lead projects classified as both
  
# Check for lead subtypes: Unknown
# al_clean |>
#   dplyr::filter(project_type=="Lead") |>
#   dplyr::mutate(
#     lead_type = dplyr::case_when(
#       stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#       stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#       stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#       # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#       stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#       stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#       TRUE ~ "unknown"
#     )
#   ) |>
#   dplyr::filter(lead_type == "unknown") 

### 1 unknown 
  
al_clean <- al_clean |>
  dplyr::mutate(
    project_description = dplyr::case_when(
      project_id == "FS010488-02" & project_name == "Waterline Replacement" ~ paste0(project_description, " | FT: LSLR"),
      .default = project_description)
    )

  
####### SANITY CHECKS END #######
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}



