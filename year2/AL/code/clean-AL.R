clean_al_y2 <- function() {
  base_path <- "year2/AL/data"
  
  # (40, 16)
  al_base <- fread(file.path(base_path, "al-y2-base-ppl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate_all(., ~str_squish(.)) |>
    dplyr::mutate(source_file = "Base")
  
  # (9, 17)
  al_supp <- fread(file.path(base_path, "al-y2-bilgensupp-ppl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate_all(., ~str_squish(.)) |>
    dplyr::mutate(source_file = "BIL")
  
  # (47,)
  # two overlapping projects in base and supp: FS010168-05, FS010096-04, 
  # requiring a merge
  al_gen <- merge(al_base, al_supp,
                  by = c("project_number", "applicant_name", 
                         "project_description", "city_town", 
                         "county", "justice_40_map_coverage", 
                         "population", "financial_rank", "dw_ratio", 
                         "disadvantged_rank", "priority_ranking_points"), all = T) %>%
    # replace NAs in non-matching columns to zero for math
    mutate(across(.cols = c("dw_bil_amount_granted", "dw_srf_amount_granted", 
                            "dw_srf_principal_forgiveness", "dw_bil_principal_forgiveness"), 
                  ~case_when(is.na(.) ~ "$0", 
                             TRUE ~ .)),
           project_name = ifelse(is.na(project_name.x), project_name.y, project_name.x),
           project_description_attachment = ifelse(is.na(project_description_attachment.x),
                                                   project_description_attachment.y, project_description_attachment.x),
           requested_amount = ifelse(is.na(applied_for_project_amount.x), 
                                     clean_numeric_string(applied_for_project_amount.y),
                                     clean_numeric_string(applied_for_project_amount.x)),
           funding_amount = ifelse(is.na(dw_srf_amount_granted), dw_bil_amount_granted, dw_srf_amount_granted),
           principal_forgiveness = ifelse(is.na(dw_srf_principal_forgiveness), dw_bil_principal_forgiveness, dw_srf_principal_forgiveness)
           ) |>
    dplyr::mutate(source_file = "Merged Base and BIL-duplicates")
  
  # (23, 16)
  al_lsl <- fread(file.path(base_path, "al-y2-lsl-ppl.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead", 
           requested_amount = clean_numeric_string(applied_for_project_amount), 
           funding_amount = clean_numeric_string(dw_bil_lead_amount_granted), 
           principal_forgiveness = clean_numeric_string(dw_bil_lsl_amount_of_pf)) |>
    dplyr::mutate(source_file = "LSL")
  

  # (4, 16)
  al_ec <- fread(file.path(base_path, "al-y2-ec-ppl-2.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    dplyr::mutate(source_file = "EC") |>
    mutate(project_type = "Emerging Contaminants", 
           funding_amount = clean_numeric_string(dw_bil_ec_amount_granted), 
           principal_forgiveness = clean_numeric_string(dw_bil_ec_amount_granted), 
           expecting_funding = "Yes",
           project_name = "No Information",
           project_number = str_squish(project_number), 
           priority_ranking_points = str_squish(priority_ranking_points),
           # create variable from other tables for matching below
           project_description_attachment = project_description)
  
  
  # binding: 
  # (75, )
  al_clean <- bind_rows(al_gen, al_lsl, al_ec) %>%
    mutate(community_served = str_squish(city_town), 
           borrower = str_squish(applicant_name), 
           pwsid =  as.character(NA),
           project_id = str_squish(project_number), 
           project_type = case_when(
             !is.na(project_type) ~ project_type,
             grepl(lead_str, project_description_attachment, ignore.case=TRUE) ~ "Lead",
             grepl(ec_str, project_description_attachment, ignore.case=TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"), 
           project_cost =  as.character(NA),
           requested_amount = clean_numeric_string(requested_amount),
           funding_amount = clean_numeric_string(funding_amount), 
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           population = clean_numeric_string(population), 
           project_description = str_squish(project_description_attachment), 
           disadvantaged = case_when(
             disadvantged_rank == "SUPP" | disadvantged_rank == "N/A" ~ "No Information",
             convert_to_numeric(disadvantged_rank) < 1 ~ "No", 
             TRUE ~ "Yes"), 
           project_rank =  as.character(NA),
           project_score = case_when(
             # supp grants are not scored
             priority_ranking_points %in% c("Supp", "SUPP") ~ "No Information", 
             TRUE ~  str_squish(priority_ranking_points), 
           ),
           expecting_funding = case_when(
             is.na(expecting_funding) ~ "Yes", 
             TRUE ~ expecting_funding),
           state = "Alabama",
           state_fiscal_year = "2024") %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, source_file)
  
####### SANITY CHECKS START #######
# Hone in on project id duplication
al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision:
# West Morgan-East Lawrence Water & Sewer Authority** will be the only one receiving funding (see Page 13), 
# drop the rest of the projects, i.e., only keep the project that serves Decatur.
  
al_clean <- al_clean |>
  dplyr::filter(
    (project_id == "FS010090‐04" & grepl("Decatur", community_served)) |
    (project_id != "FS010090‐04")
  )

# clean_al_y2 |>
#   dplyr::filter(project_id == "FS010096-04")

# clean_al_y2 |>
#   dplyr::filter(project_id == "FS010168-05")

####### Decision: Merging of Base and BIL see al_gen

# Check for disinfection byproduct in description
  al_clean |>
    dplyr::filter(grepl("disinfection byproduct", project_description)) 

####### Decision : No disinfection byproduct string

####### SANITY CHECKS END #######

  al_clean <- al_clean |>
    dplyr::select(-source_file)
  
  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}


