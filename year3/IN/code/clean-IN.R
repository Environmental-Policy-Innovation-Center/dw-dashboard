clean_in_y3 <- function() {

  in_ppl_comprehensive <- data.table::fread("year3/IN/data/IN-SFY24-DWSRF-Q4-PPL-Comprehensive.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY24 Q4 comprehensive",
      project_id = str_squish(srf_project_no)
    )

  in_iup_q1 <- data.table::fread("year3/IN/data/IN_SFY24_Q1_fundable.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY24 Q1 fundable",
      project_id = str_squish(srf_project_no)
    ) 
  
  # inner_join_and_order_q1_q4(
  #   df_q1       = in_iup_q1,
  #   df_q4       = in_ppl_comprehensive,
  #   fiscal_year = "2024",
  #   output_path = "./output/tmp/SFY24_base_overlap_Q1_Q4.csv"
  # )

  in_ppl_lslr <- data.table::fread("year3/IN/data/IN-SFY24-DWSRF-Q4-PPL-Lead.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(project_type = "Lead") |>
     dplyr::mutate(
      list = "SFY24 Q4 LSLR comprehensive",
      project_id = str_squish(srf_project_no)
    )

  in_iup_q1_lslr <- data.table::fread("year3/IN/data/IN_SFY24_Q1_LSLR.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY24 Q1 LSLR fundable",
      project_type = "Lead", 
      project_id = str_squish(srf_project_no)
    ) 
  
  # inner_join_and_order_q1_q4(
  #   df_q1       = in_iup_q1_lslr,
  #   df_q4       = in_ppl_lslr,
  #   fiscal_year = "2024",
  #   output_path = "./output/tmp/SFY24_lslr_overlap_Q1_Q4.csv"
  # )
  
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
        (grepl("lsl|lead", project_description, ignore.case=TRUE) | convert_to_numeric(lead_service_line_replacement_cost, TRUE)>0) &
        (grepl("Yes", emerging_contaminants, ignore.case = TRUE) |  grepl(ec_str, project_description, ignore.case=TRUE)) ~ "Both Lead and EC",
        grepl("lsl|lead", project_description, ignore.case=TRUE) | convert_to_numeric(lead_service_line_replacement_cost, TRUE)>0  ~ "Lead",
        grepl("Yes", emerging_contaminants, ignore.case = TRUE) ~ "Emerging Contaminants",
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
      expecting_funding = dplyr::case_when(
        grepl("fundable", list) ~ "Yes",
        .default = "No"
      ),
      state = "Indiana",
      state_fiscal_year = "2024",
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
####### SANITY CHECKS START #######

# Hone in on project id duplication
## duplicate_ids <-  in_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts)) |> dplyr::filter(counts >1)

  in_clean <- in_clean |>
    dplyr::left_join(
      tibble::tribble(
        ~project_id,    ~list,                         ~keep, ~add_to_list,                    
        "DW160935 02", "SFY24 Q1 fundable",           "y",   "not ef in Q4; not dac in Q4", 
        "DW160935 02", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW220482 04", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW220482 04", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW222156 01", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW222156 01", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW223648 01", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW223648 01", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW230648 01", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW230648 01", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW233070 03", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW233070 03", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW233461 02", "SFY24 Q1 fundable",           "y",   "not ef in Q4; recat ec in Q4",
        "DW233461 02", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW234049 02", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW234049 02", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW234171 04", "SFY24 Q1 LSLR fundable",      "y",   "not ef in Q4",                
        "DW234171 04", "SFY24 Q4 LSLR comprehensive", "n",   NA,                            
        "DW234671 03", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW234671 03", "SFY24 Q4 comprehensive",      "n",   NA,                            
        "DW240392 02", "SFY24 Q1 fundable",           "y",   "not ef in Q4",                
        "DW240392 02", "SFY24 Q4 comprehensive",      "n",   NA
      )
    ) |>
    dplyr::mutate(
      keep = tidyr::replace_na(keep,"y")
    ) |>
    dplyr::filter(keep == "y") |>
    dplyr::mutate(
      list = ifelse(!is.na(add_to_list), paste0(list, "; ", add_to_list), list)
    ) |>
    dplyr::select(-c(keep, add_to_list))

####### Decision: 2026/07/16 we’ll stick with Q1 info since more comparable with other states. 
# And make separate note that project may have moved into Q4 list for internal tracking

# Check for disinfection byproduct in description
# in_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
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

  ####### Decision: No projects classified as both
  
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
  
  ######## Decision: DW224189 02 LSLR, DW241207 01 LSLI
  in_clean <- in_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_type == "Lead" & project_id == "DW224189 02" ~ paste0(project_description, "|FT: LSLR"),
        project_type == "Lead" & project_id == "DW241207 01" ~ paste0(project_description, "|FT: LSLI"),
        .default = project_description
      )
    )
  
  # Check pwsid lengths
  # in_clean |>
  #   dplyr::mutate(
  #     length_pwsid = stringr::str_length(pwsid)
  #   ) |> 
  #   dplyr::filter(!length_pwsid == 9) |>
  #   dplyr::filter(!pwsid == "No Information")   

  in_clean <- in_clean |>
    dplyr::mutate(
      pwsid = dplyr::case_when(
        borrower == "Jackson County Water Utility" & pwsid == "IN36003/52360" ~ "IN5236003",
        borrower == "Tipton" & pwsid == "IN528004" ~"IN5280004",
        .default = pwsid
      )
    )    

####### SANITY CHECKS END #######
  
  run_tests(in_clean)
  rm(list=setdiff(ls(), "in_clean"))
  
  return(in_clean)
}