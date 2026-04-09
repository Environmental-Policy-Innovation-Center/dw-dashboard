clean_md_y3 <- function() {

  md_ppl_comprehensive <- data.table::fread("year3/MD/data/MD_Y3_SFY26_Comprehensive_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 comprehensive"
    ) |>
    tidyr::drop_na(project_number) 
  
  #the fundable table was extracted with claude and some dollar amount columns with "-" were substituted with 0, contrast with raw tables for true 0s
                  
  md_ppl_fundable <- data.table::fread("year3/MD/data/MD_Y3_SFY26_Fundable_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY26 fundable",
      total_funding = clean_numeric_string(total_funding),
      expecting_funding = ifelse(total_funding == "0", "No Information", "Yes"),
      principal_forgiveness = 
        convert_to_numeric(dwsrf_base_pf, fill_na_0 = TRUE) + 
        convert_to_numeric(bil_general_pf, fill_na_0 = TRUE) +
        convert_to_numeric(bil_lsl_pf, fill_na_0 = TRUE) + 
        convert_to_numeric(bil_pfas_ec, fill_na_0 = TRUE),
      project_type = dplyr::case_when(
        convert_to_numeric(bil_pfas_ec, fill_na_0 = TRUE) > 0 ~ "Emerging Contaminants",
        convert_to_numeric(bil_lsl_loan, fill_na_0 = TRUE) > 0 ~ "Lead",
        convert_to_numeric(bil_lsl_pf, fill_na_0 = TRUE) > 0 ~ "Lead",
        .default = NA
      )
    ) |>
    dplyr::select(project_number, list, total_funding, expecting_funding, principal_forgiveness, project_type) 
  
  # sum(md_ppl_fundable$project_number %in% md_ppl_comprehensive$project_number)

  # 37 bypassed projects
  # bypassed_projects <- c("DW0079", "DW0032", "DW0035", "DW0021", "DW0043", "DW0047", "DW0034", "DW0078", "DW0018", "DW0016", "DW0006", "DW0014", "DW0009", "DW0061", "DW0070", "DW0066", "DW0088", "DW0052", "DW0049", "DW0019", "DW0080", "DW0074", "DW0017", "DW0020", "DW0013", "DW0033", "DW0025", "DW0084", "DW0004", "DW0082", "DW0057", "DW0054", "DW0085", "DW0007", "DW0005", "DW0091", "DW0037")
  # sum(md_ppl_fundable$project_number %in% bypassed_projects)
  # [keep] all bypassed projects are present in the comprehensive list
  # sum(bypassed_projects %in% md_ppl_comprehensive$project_number)
  
  md_combined <- md_ppl_comprehensive |>
    dplyr::left_join(md_ppl_fundable, by= "project_number") |>
    # [keep] we default to list value of the fundable list
    dplyr::mutate(
      list = ifelse(is.na(list.y), list.x , list.y)
    ) |>
    dplyr::select(-list.x, -list.y)
  
  md_clean <- md_combined |>
    dplyr::mutate(
      # [keep] geographic reference from the dd is encoded as community_served here (this has no downstream effects) 
      community_served = stringr::str_squish(county),
      borrower = stringr::str_squish(applicant_name),
      # [keep] Extracted from Applicant Name/County in Comprehensive List by removing the text after the first comma, reviewed in md_clean
      borrower = stringr::str_remove(borrower, '^[^,]+,\\s*'),
      # [keep] extracted from source as pwsid in raw data
      pwsid = stringr::str_squish(pwsid),
      pwsid = ifelse(is.na(pwsid), "No Information", pwsid),
      pwsid = ifelse(pwsid == "MD??", "No Information", pwsid),
      # [keep] noted by the policy analist that the project number should not be used as project id (as it is not unique)
      project_id = as.character(NA),
      project_name = stringr::str_squish(project_title),
      project_description = stringr::str_squish(project_description),
      # [keep] project details column separated as lsl and pfas_ec columns, when Y, the strings were present in the cell
      # [keep] ec_str consists of the keywords presented in the DD
      project_type =  case_when(
        !is.na(project_type.y) ~ project_type.y,
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl("lsl|lead", project_name, ignore.case=TRUE) ~ "Lead",
        grepl("Y", lsl, ignore.case=TRUE)  ~ "Lead",  
        grepl(ec_str, project_description, ignore.case=TRUE)  ~ "Emerging Contaminants",
        grepl(ec_str, project_name, ignore.case=TRUE)  ~ "Emerging Contaminants",
        grepl("Y", pfas_ec, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"),
      project_cost = clean_numeric_string(total_cost),
      requested_amount = clean_numeric_string(mde_requested_funding),
      # [keep] projects with “-” have been scraped as empty, and the clean_numeric_string funtion will coerce to "No Information"
      funding_amount = clean_numeric_string(total_funding),
      # [keep] verified that clean_md has No Information for PF when funding amount is No Information, further true 0s are kept as 0
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      principal_forgiveness =ifelse(funding_amount == "No Information", "No Information", principal_forgiveness),
      # [keep] population is found in the benefited_population column , claude scraped Ben.Pop= ? as 0
      population = ifelse(benefited_population %in% c("?", "0"), "No Information", benefited_population),
      # [keep] Disadv was encoded as 1 and 0 for Yes and No, respectively
      disadvantaged = ifelse(disadvantaged == "1", "Yes", "No"),
      project_rank = stringr::str_squish(rank),
      project_score = stringr::str_squish(points),
      # [keep] all bypassed projects are on the comprehensive list and not in the fundable list, therefore NOT expecting funding
      expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding),
      state = "Maryland",
      state_fiscal_year = "2026",
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
 
  md_clean <- md_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_type == "Lead" & project_description == "D/C REPLACING CAST IRON SECTIONS OF 130-YEAR OLD SPRINGLINE WITH LEAD SEALED JOINTS TO IMPROVE WATER FLOW/QUALITY" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "P/D/C REPLACEMENT OF AGED MAIN LINES AND SERVICE LINES TO MITIGATE DRINKING WATER CONTAMINATION AND POLLUTION" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "PLANNING PROJECT & FUNDS ONLY" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "P PRESUME DETERMINE LEAD IN LINES - NO REPLY" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "C IMPLEMENT LEADCAST PREDICT MODELING & DATA-DRIVEN TARGETED FIELD VERIFICATION" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "D/C REPLACE ~330 PUBLIC/PRIVATE PROPERTIES (50 FT OF 1 INCH PIPES FROM MAIN TO EA BLDG) IN SW QUADRANT OF CITY'S WATER SYS - EJ" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "PRELIM INVENT. REVIEW OF ALL EXISTING RECORDS; PUBLISH UPDATED INVENT.; PREP FINAL REPORT & INVENT. PER LCRR TO MDE--10/16/24" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "D/C POTENTIALLY REPLACE UP TO 1.9M LF OF LSLs COVERING UP TO 5500 PRIVATE GALVANIZED SYSTEMS & 33,000 UNKNOWN LSLs TBD" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "PLAN CONFIRM VIA FIELD INVESTIGATION METHODS TOTAL UNKNOWN LSL & GP TO REPLACE TO COMPLY WITH LCRR IN COUNTY BOUNDARIES" ~ paste0(project_description, " | FT: LSLR"),
        .default = project_description 
      )
    )
    
####### SANITY CHECKS START #######

# Hone in on project id duplication
# md_clean |> dplyr::distinct() |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))

####### Decision : No project_id

# Check for disinfection byproduct in description
# md_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
####### Decision : No disinfection byproduct string
  
# Check for lead subtypes: Both
# md_clean |>
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

####### Decision: 1 projects classified as both --> replacement
  
# Check for lead subtypes: Unknown
# md_clean |>
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
  
######## Decision: 9 projects classified as unknown, adressed upstream
  
####### SANITY CHECKS END #######
  

  run_tests(md_clean)

  rm(list=setdiff(ls(), "md_clean"))
  
  return(md_clean)
}