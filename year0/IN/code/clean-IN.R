clean_in_y0 <- function() {

  in_iup_q4 <- read.csv("year0/IN/data/IN-SFY21-DWSRF-Q4-PPL.csv") %>%
    clean_names()
  
  in_clean <- in_iup_q4 |>
    dplyr::mutate(
    community_served = as.character(NA),
    borrower = str_squish(participant),
    pwsid = str_split(pwsid_no, "[,\n]") %>%
      lapply(str_trim) %>%
      lapply(function(x) paste0("IN", x)) %>%
      lapply(paste, collapse = ", ") %>%
      unlist(),
    project_id = str_squish(srf_project_no),
    project_name = as.character(NA),
    project_description = stringr::str_squish(project_description),
    project_type =  case_when(
        grepl(lead_str, project_description, ignore.case=TRUE)  ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        TRUE ~ "General"),
    project_cost = dplyr::case_when(
      estimated_total_project_cost == "" ~ "No Information",
      .default = clean_numeric_string(estimated_total_project_cost)
    ),
    requested_amount = dplyr::case_when(
      requested_funds == "" ~ "No Information",
      .default = clean_numeric_string(requested_funds)
    ),
    funding_amount = as.character(NA),
    principal_forgiveness = as.character(NA),
    population = clean_numeric_string(population_served),
    estimated_post_user_rate = convert_to_numeric(estimated_post_project_user_rate_per_4_000_gallons, TRUE),
    mhi = convert_to_numeric(mhi, TRUE),
    disadvantaged = case_when(
      mhi < 43460 ~ "Yes",
      estimated_post_user_rate > 45 ~ "Yes",
      estimated_post_user_rate > mhi * .01 ~ "Yes",
      TRUE ~ "No"),
  project_rank = dplyr::case_when(
    is.na(ppl_rank) ~ "No Information",
    .default = str_squish(ppl_rank)
  ),
  project_score = dplyr::case_when(
    is.na(ppl_score) ~ "No Information",
    .default = str_squish(ppl_score)
  ),
  expecting_funding =  as.character(NA),
  state = "Indiana",
  state_fiscal_year = "2021"
  ) |>
  select(community_served, borrower, pwsid, project_id, project_name, project_type,
         project_cost, requested_amount, funding_amount, principal_forgiveness,
         project_description, population, disadvantaged, project_rank, project_score,
         expecting_funding, state, state_fiscal_year)

  ####### SANITY CHECKS START #######

# Hone in on project id duplication
in_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
####### Decision : No duplicates

# Check for disinfection byproduct in description
in_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string

####### SANITY CHECKS END #######
  
  run_tests(in_clean)
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}
