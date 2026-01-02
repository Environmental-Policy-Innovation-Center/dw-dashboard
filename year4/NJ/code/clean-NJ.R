clean_nj_y4 <- function() {
  
  nj_raw <- data.table::fread("year4/NJ/data/NJ_SFY26_PartA.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names()
  
  nj_disadvantaged <- data.table::fread("year4/NJ/data/NJ_SFY26_disadvantaged.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(as.numeric(ac_score)<=86.19)

  nj_clean <- nj_raw |>
    dplyr::select(-cat_a, -cat_c_a, -cat_c_b, -cat_c_c, -cat_c_d, -cat_e) |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(project_sponsor),
      pwsid = paste0("NJ", stringr::str_extract(project_number, "^[^-]+")),
      project_id = stringr::str_squish(project_number),
      project_name = str_squish(project_name),
      project_type = dplyr::case_when(
        bil_eligibility == "BIL(GEN)" ~ "General",
        bil_eligibility == "BIL (GEN)" ~ "General", 
        bil_eligibility == "IL (GEN)/ BIL (LSLR)" ~ "Lead",
        bil_eligibility == "BIL (GEN)/ BIL (EC)" ~ "Emerging Contaminants",
        grepl(lead_str, project_name, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_name, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(estimated_cost),
      requested_amount = as.character(NA),     
      funding_amount = as.character(NA),     
      principal_forgiveness = as.character(NA),
      project_description = str_squish(project_name),
      population = clean_numeric_string(population),
      disadvantaged = dplyr::case_when(
        pwsid %in% nj_disadvantaged$pwsid ~ "Yes",
        cat_b == "80" ~ "Yes", 
        .default = "No"
      ),
      project_rank = str_squish(rank),
      project_score = str_squish(rank_points),
      expecting_funding = dplyr::case_when(
        actual_anticipated_funding_status == "2026" ~ "Yes",
        actual_anticipated_funding_status == "Funded" ~ "No Information",
        actual_anticipated_funding_status == "Beyond 2026" ~ "No",
        .default = "check"
      ),
      state = "New Jersey",
      state_fiscal_year = "2026"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  nj_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::arrange(dplyr::desc(n)) |> dplyr::filter(n>1)

  ### Decision: No duplicates

  # Check for disinfection byproduct in description
  nj_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))

  ### Decision: No disinfection byproduct string

  # Check for lead subtypes
  nj_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "both")

  ### Decision: No lead projects classified as both

  # Check for lead subtypes: Unknown
  nj_clean |>
    dplyr::filter(project_type=="Lead") |>
    dplyr::mutate(
      lead_type = dplyr::case_when(
        stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
        stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
        stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
        # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
        stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
        stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::filter(lead_type == "unknown") 

  ### Decision: 2 Unknown
  ####### SANITY CHECKS END #######
  
  run_tests(nj_clean)
  rm(list=setdiff(ls(), "nj_clean"))
  
  return(nj_clean)
}