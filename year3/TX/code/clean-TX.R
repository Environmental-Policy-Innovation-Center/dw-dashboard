clean_tx_y3 <- function() {
  base_path <- file.path("year3", "TX", "data")
  
  # this includes all projects
  tx_ppl <- fread(file.path(base_path, "tx-y3-iup-appendix-j.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names()
  
  # expecting funding projects
  tx_invite <- fread(file.path(base_path, "tx-y3-iup-appendix-k.csv"),
                     colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(pif_number, expecting_funding) |>
    tibble::add_row(pif_number = "15941", expecting_funding = "Yes") #this project was missing
  
  
  tx_ppl <- tx_ppl %>%
    left_join(tx_invite, by="pif_number")

  
  # lead applicant list
  tx_lsl <- fread(file.path(base_path, "tx-y3-appendix-i-lsl-625-ammendment.csv"),
                   colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Lead",
           disadvantaged = "Yes")
  
  tx_lsl_invite <- data.table::fread(file.path(base_path, "TX_Y3_LSLR_invited_appendix_j.csv"),
                          colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(expecting_funding = "Yes") |>
    dplyr::select(rank, pif_no, expecting_funding)
  
  tx_lsl <- tx_lsl %>%
    left_join(tx_lsl_invite, by=c("pif_no", "rank")) %>%
    rename(pif_number = pif_no,
           population = population_served,
           pws_id = pws_id_no)
  
  # ec applicant list
  tx_ec <- fread(file.path(base_path, "tx-y3-appendix-j-ec.csv"),
                  colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(project_type = "Emerging Contaminants",
           disadvantaged = "Yes")
  
  tx_ec_invite <- fread(file.path(base_path, "tx-y3-appendix-k-ec.csv"),
                         colClasses = "character", na.strings = "") %>%
    clean_names() %>%
    mutate(expecting_funding = "Yes") %>%
    select(rank, expecting_funding)
  
  tx_ec <- tx_ec %>%
    left_join(tx_ec_invite, by="rank")
  
  # combine general, lead, and ec
  tx_all <- bind_rows(tx_ppl, tx_lsl, tx_ec)

  # join invited by project id and then process for output
  tx_clean <- tx_all %>%
    mutate(community_served = as.character(NA),
           borrower = str_squish(entity),
           pwsid = str_squish(pws_id),
           project_id = str_squish(pif_number),
           project_name = as.character(NA),
           project_cost = clean_numeric_string(total_project_cost),
           requested_amount = as.character(NA),
           principal_forgiveness = as.character(NA),
           population = clean_numeric_string(population),
           project_description = str_squish(project_description),
           project_rank = str_squish(rank),
           project_score = str_squish(points),
           project_type = case_when(
             # ec and led docs already defined
             !is.na(project_type) ~ project_type,
             # search for keywords from full PPL, otherwise General project
             grepl(lead_str, project_description, ignore.case = TRUE) ~ "Lead",
             grepl(ec_str, project_description, ignore.case = TRUE) ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # lead docs already defined - if still NA and disavd_percent from PPL is NA, Not DAC
           disadvantaged = ifelse(is.na(disadv_percent) & is.na(disadvantaged), "No", "Yes"),
           project_id = replace_na(project_id, "No Information"),
           expecting_funding = replace_na(expecting_funding, "No"),
           funding_amount = as.character(NA),
           pwsid = replace_na(pwsid, "No Information"),
           state = "Texas",
           state_fiscal_year = "2025"
    ) %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
tx_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : project_id = 16056 Different projects, same id

# Check for disinfection byproduct in description
tx_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  
####### Decision : No disinfection byproduct string

# Check for lead subtypes: Both
# tx_clean |>
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
#     dplyr::filter(lead_type == "both") |> View()

  ####### Decision: 
  
  # Check for lead subtypes: Unknown
  # tx_clean |>
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
  #   dplyr::filter(lead_type == "unknown") |> View()


####### SANITY CHECKS END #######

  
  # Run validation tests
  run_tests(tx_clean)
  rm(list=setdiff(ls(), "tx_clean"))
  
  return(tx_clean)
}