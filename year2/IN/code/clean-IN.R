clean_in_y2 <- function() {

  in_iup_q1 <- fread("year2/IN/data/IN_SFY23_Q1_Base_fundable.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "fundable",
      project_id = str_squish(srf_project_no)
    )
 
  in_ppl_comprehensive <- fread("year2/IN/data/IN-SFY23-DWSRF-Q4-PPL-Comprehensive.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "comprehensive",
      project_id = str_squish(srf_project_no)
    ) |>
    dplyr::filter(!project_id %in% in_iup_q1$project_id)

  in_iup_q1_lslr <- fread("year2/IN/data/IN_SFY23_Q1_Lead_fudable.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "fundable",
      project_type = "Lead", 
      project_id = str_squish(srf_project_no)
    )
  
  in_ppl_lslr <- fread("year2/IN/data/IN-SFY23-DWSRF-Q4-PPL-LSLR.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead") |>
    dplyr::mutate(
      list = "comprehensive",
      project_id = str_squish(srf_project_no)
    ) |>
    dplyr::filter(!project_id %in% in_iup_q1_lslr$project_id)
  
  in_combined <- dplyr::bind_rows(in_iup_q1, in_ppl_comprehensive, in_iup_q1_lslr, in_ppl_lslr)
  
  in_clean <- in_combined |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(participant),
      pwsid = str_split(pwsid_no, "[,\n]") %>%
        lapply(str_trim) %>%
        lapply(function(x) paste0("IN", x)) %>%
        lapply(paste, collapse = ", ") %>%
        unlist(),
      pwsid = ifelse(pwsid == "INTBD", "No Information", pwsid),
      project_name = as.character(NA),
      project_description = stringr::str_squish(project_description),
      project_type =  case_when(
        !is.na(project_type) ~ project_type,
          grepl(lead_str, project_description, ignore.case=TRUE) | convert_to_numeric(lead_service_line_replacement_cost, TRUE)>0  ~ "Lead",
          grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
          TRUE ~ "General"),
      project_cost = as.character(NA),
      requested_amount = dplyr::case_when(
        is.na(requested_funds) ~ clean_numeric_string(estimated_total_project_cost),
        .default = clean_numeric_string(requested_funds)
      ),
      funding_amount = as.character(NA),
      principal_forgiveness = as.character(NA),
      population = clean_numeric_string(population_served),
      disadvantaged = disadvantaged_community,
      project_rank = dplyr::case_when(
        is.na(ppl_rank) | ppl_rank == "-" ~ "No Information",
        .default = str_squish(ppl_rank)
      ),
      project_score = dplyr::case_when(
        is.na(ppl_score) ~ "No Information",
        .default = str_squish(ppl_score)
      ),
    expecting_funding =  dplyr::case_when(
      list == "fundable" ~ "Yes",
      .default = "No"
    ),
    state = "Indiana",
    state_fiscal_year = "2023",
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
in_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

# in_clean |> dplyr::filter(project_id == "DW210251 01")
####### Decision : Keep, distinct projects, id typo

# Check for disinfection byproduct in description
#in_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string
  
# Check for lead subtypes: Both
# in_clean |>
#     dplyr::filter(project_type=="Lead") |>
#     dplyr::mutate(
#       lead_type = dplyr::case_when(
#         stringr::str_detect(tolower(project_description), lsli_str) & stringr::str_detect(tolower(project_description), lslr_str) ~ "both",
#         stringr::str_detect(tolower(project_description), lsli_str) ~ "lsli",
#         stringr::str_detect(tolower(project_description), lslr_str) ~ "lslr",
#         # catch weird exceptions where replacement/inventory doesn't appear next to LSL but should still be marked lslr/i
#         stringr::str_detect(tolower(project_description), "replacement") & stringr::str_detect(tolower(project_description), lead_str) ~ "lslr",
#         stringr::str_detect(tolower(project_description), "inventory") & stringr::str_detect(tolower(project_description), lead_str) ~ "lsli",
#         TRUE ~ "unknown"
#       )
#     ) |>
#     dplyr::filter(lead_type == "both")

  ####### Decision: DW230970 01 is both
  
  # Check for lead subtypes: Unknown
  # in_clean |>
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
  
  ######## Decision: Can't amend based on description

####### SANITY CHECKS END #######
  
  run_tests(in_clean)
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}