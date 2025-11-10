clean_ar_y2 <- function() {
  
  #comprehensive ppl
  ar_ppl <- data.table::fread("year2/AR/data/Comprehensive.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      borrower = str_squish(pws), 
      pws_id = str_squish(pws_id),
      pwsid = dplyr::case_when(
        stringr::str_count(pws_id) == 2 ~ paste0("AR00000", pws_id),
        stringr::str_count(pws_id) == 3 ~ paste0("AR0000", pws_id),
        stringr::str_count(pws_id) == 4 ~ paste0("AR000", pws_id),
        is.na(pws_id) | pws_id == "" ~ "No Information",
        .default = "check" #manually check
      ),
      iup_no = no,
      project_cost = clean_numeric_string(project_cost),
      project_description = str_squish(project_description),
      population = clean_numeric_string(population),
      disadvantaged = str_to_title(disadvantaged_y_n),
      project_score = str_squish(total_points)
    ) |>
    dplyr::select(iup_no, borrower, pwsid, project_cost, project_description,
           population, disadvantaged, project_score)
  
  # Fundable list
  ar_c3 <- data.table::fread("year2/AR/data/Chart3.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      borrower = stringr::str_squish(entity), 
      borrower = ifelse(iup_no == "7", "Central Arkansas Water", borrower), #this project has this name in the comprehensive list and does not make a distinction, assuming it's three fundable phases colllapse into one
      pws_id = stringr::str_squish(pws_id),
      pwsid = dplyr::case_when(
        stringr::str_count(pws_id) == 2 ~ paste0("AR00000", pws_id),
        stringr::str_count(pws_id) == 3 ~ paste0("AR0000", pws_id),
        stringr::str_count(pws_id) == 4 ~ paste0("AR000", pws_id),
        is.na(pws_id) | pws_id == "" ~ "No Information",
        .default = "check" #manually check
      ),
      project_score = stringr::str_squish(total_points),
      project_description = stringr::str_squish(project_description),
      project_cost = clean_numeric_string(project_cost),
      expecting_funding = "Yes",
      disadvantaged = stringr::str_to_title(disadvantaged_y_n),
      commitment_date = stringr::str_squish(est_binding_commitment)
    ) |>
    dplyr::select(iup_no, borrower, pwsid, project_score, project_description, project_cost, expecting_funding, disadvantaged, commitment_date) |>
    dplyr::distinct() |>
    dplyr::group_by(iup_no, borrower, pwsid, project_score, expecting_funding, disadvantaged, commitment_date) |>
    dplyr::summarise(
      project_description = paste(unique(project_description), collapse = " | "),
      project_cost = as.character(sum(as.numeric(project_cost)),.groups = "drop"))
  
  # DAC/PF Fundable List
  ar_c4 <- data.table::fread("year2/AR/data/Chart4.csv",
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      borrower = str_squish(project_name), 
      borrower = ifelse(iup_no == "7", "Central Arkansas Water", borrower), #this project is not differentiated in the comprehensive list
      commitment_date = stringr::str_squish(b_c_date),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_amt),
      disadvantaged = stringr::str_to_title(disadvantaged_eligible_y_n),
    ) |>
    dplyr::select(iup_no, borrower, disadvantaged, commitment_date, principal_forgiveness) |>
    dplyr::group_by(across(-principal_forgiveness)) |>
    dplyr::summarise(principal_forgiveness = clean_numeric_string(sum(as.numeric(principal_forgiveness), na.rm = TRUE))) |>
    dplyr::mutate(expecting_funding = "Yes")   

  ar_comb_fundable <- ar_c3 |>
    dplyr::left_join(ar_c4) |>
    dplyr::mutate(
      list = "fundable"
    )
  
  ar_clean <- ar_ppl |>
    dplyr::full_join(ar_comb_fundable, by = join_by(iup_no, pwsid, disadvantaged, project_score)) |>
    #default to values in fundable lists; these columns will be selected downstream
    dplyr::rename(
      borrower = borrower.y, 
      project_cost = project_cost.y,
      project_description = project_description.y
    ) |>
    dplyr::mutate(
      borrower = ifelse(is.na(borrower), borrower.x, borrower),
      project_cost = ifelse(is.na(project_cost), project_cost.x, project_cost),
      project_description = ifelse(is.na(project_description), project_description.x, project_description),
      community_served = as.character(NA),
      project_id = as.character(NA),
      project_name = as.character(NA),
      project_type =  case_when(
        grepl(lead_str, project_description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
      requested_amount = as.character(NA),
      funding_amount = as.character(NA),
      expecting_funding = replace_na(expecting_funding, "No"),
      principal_forgiveness = replace_na(principal_forgiveness, "0"),
      pwsid = replace_na(pwsid, "No Information"),
        project_rank = as.character(NA),
      state = "Arkansas",
      state_fiscal_year = "2024"
    ) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type,
           project_cost, requested_amount, funding_amount, principal_forgiveness,
           project_description, population, disadvantaged, project_rank, project_score,
           expecting_funding, state, state_fiscal_year)
  
    ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ar_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No project_id
  
  # Check for disinfection byproduct in description
  ar_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: No change, classified as expected
  
  # Check for lead subtypes
  ar_clean |>
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
  )) |>
    dplyr::filter(lead_type == "both")
  ####### Decision: No lead projects classified as both
  
  ####### SANITY CHECKS END #######
  
  run_tests(ar_clean)
  rm(list=setdiff(ls(), "ar_clean"))
  
  return(ar_clean)
}
