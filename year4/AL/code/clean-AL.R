clean_al_y4 <- function() {

  al_base <- data.table::fread(
    "year4/AL/data/SFY26_Base_Final_Amended_PPL.csv",
    colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
    dplyr::mutate(list = "SFY26 Base PPL")

  al_gen <- data.table::fread(
    "year4/AL/data/SFY26_Gen_Supp_PPL.csv",
    colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
    dplyr::mutate(list = "SFY26 Gen Supp PPL")
    
  al_lslr <- data.table::fread(
    "year4/AL/data/SFY26_LSLR_Draft_PPL.csv",
    colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Lead",
      list = "SFY26 LSLR PPL"
    )

  al_ec <- data.table::fread(
    "year4/AL/data/SFY26_EC_Draft_PPL.csv",
    colClasses = "character", na.strings = "") |>
      janitor::clean_names() |>
    dplyr::mutate(
      project_type = "Emerging Contaminants",
      list = "SFY26 EC PPL")


  al_iup <- dplyr::bind_rows(al_base, al_gen, al_ec, al_lslr) |>
    dplyr::rename(attachment_description = attachment_project_description) |>
    dplyr::mutate(
      project_id = as.character(project_number),
      dw_srf_amount_granted = convert_to_numeric(dw_srf_amount_granted),
      dw_srf_pf = convert_to_numeric(dw_srf_pf)
    )

  # [keep] after processing, 3 duplicates surfaced and the following decisions were made:
  # [keep] for FS010369-01, this is the same project, the dw_srf_amount_granted should be added together (this is just getting funding from carryover and from regular funds)
  # [keep] for FS010387-09, this is the same project, the dw_srf_amount_granted values and the dw_srf_pf values should be added together (this is just getting funding from carryover and from regular funds)
  # [keep] for FS010387-10, these are different phases of the same project please treat as the same project but it should appear twice -- it received Base funding for planning and design and Gen Supp funding for phase 1 of construction
  
  al_iup_collapsed <- al_iup |>
    dplyr::filter(project_id != "FS010387-10") |> #keep this duplicate as it corresponds to 2 phases
    dplyr::group_by(project_id) |>
    dplyr::summarise(
      dw_srf_amount_granted = dplyr::case_when(
        any(project_id %in% c("FS010369-01", "FS010387-09")) ~ sum(dw_srf_amount_granted, na.rm = TRUE), #aggregate dw_srf_amount_granted
        .default = dplyr::first(dw_srf_amount_granted)
      ),
      dw_srf_pf = dplyr::case_when(
        any(project_id == "FS010387-09") ~ sum(dw_srf_pf, na.rm = TRUE), #aggregate dw_srf_pf
        .default = dplyr::first(dw_srf_pf)
      ),
      across(dplyr::everything(), dplyr::first),
      .groups = "drop"
    )

  al_iup <- dplyr::bind_rows(
    al_iup_collapsed,
    # re-attach untouched duplicate
    dplyr::filter(al_iup, project_id == "FS010387-10")  
  )

  al_clean <- al_iup |>
    mutate(
      community_served = str_squish(city_town),
      community_served = dplyr::case_when(
        is.na(community_served) ~ "No Information",
        .default = community_served
      ),
      borrower = str_squish(applicant_name),
      pwsid = as.character(NA),
      project_name = str_squish(attachment_project_name),
      project_description = str_squish(attachment_description),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl("lsl|lead", project_description, ignore.case = TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      project_cost = as.character(NA),
      requested_amount = clean_numeric_string(applied_for_project_amount),
      funding_amount = dplyr::case_when(
        list == "SFY26 Base PPL" ~ "No Information",
        .default = clean_numeric_string(dw_srf_amount_granted)
      ),
      principal_forgiveness = dplyr::case_when(
        list == "SFY26 Base PPL" ~ "No Information",
        .default = clean_numeric_string(dw_srf_pf)),
      population = clean_numeric_string(population),
      disadvantaged = dplyr::case_when(
        list == "SFY26 Base PPL" & as.numeric(disadvantaged_score) > 1 ~ "Yes",
        list == "SFY26 Base PPL" & as.numeric(disadvantaged_score) <= 1 ~ "No",
        list %in% c("SFY26 Gen Supp PPL", "SFY26 LSLR PPL", "SFY26 EC PPL") & as.numeric(disadvantaged_rank) > 1 ~ "Yes",
        list %in% c("SFY26 Gen Supp PPL", "SFY26 LSLR PPL", "SFY26 EC PPL") & as.numeric(disadvantaged_rank) <= 1 ~ "No"
      ),
      project_rank = as.character(NA),
      project_score = str_squish(priority_ranking_points),
      project_score = ifelse(project_score %in% c("SUPP", "N/A", "NA"), "No Information", project_score),
    ) |>

    dplyr::mutate(
      expecting_funding = "Yes",
      state = "Alabama",
      state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS #######
# Hone in on project id duplication
# al_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision: see implementation in al_iup_collapsed 
# for FS010369-01 -- this is the same project, the dw_srf_amount_granted should be added together (this is just getting funding from carryover and from regular funds)
# for FS010387-09 -- this is the same project, the dw_srf_amount_granted values and the dw_srf_pf values should be added together (this is just getting funding from carryover and from regular funds)
# For FS010387-10, these are different phases of the same project please treat as the same project but it should appear twice -- it received Base funding for planning and design and Gen Supp funding for phase 1 of construction
  
# Check for disinfection byproduct in description
# al_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
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

####### Decision: 3 lead projects classified as both -->
#  see project_name
#   FS010256-07 --> both
#   FS010003-03 --> replacement
#   FS020411-02 --> replacement

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

### 1 unknown --> LSLR  

al_clean <- al_clean |>
  dplyr::mutate(
    project_description = ifelse(project_id == "FS010027-03", 
    paste0(project_description , " | FT: LSLR"),
    project_description
  )
  )
  
####### SANITY CHECKS END #######
 

  run_tests(al_clean)
  rm(list=setdiff(ls(), "al_clean"))
  
  return(al_clean)
}

