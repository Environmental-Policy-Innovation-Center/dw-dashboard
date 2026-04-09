clean_md_y2 <- function() {

  md_ppl_comprehensive <- data.table::fread("year2/MD/data/MD_Y2_SFY25_Comprehensive_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "SFY25 comprehensive"
    ) |>
    tidyr::drop_na(project_number) 
  
  #the fundable table was extracted with claude and some dollar amount columns with "-" were substituted with 0, contrast with raw tables for true 0s

  # [keep] TOWN OF NORTH EAST CECIL NORTH EAST WATER QUALITY IMPROVEMENTS - SOURCE was inputed as found in comprehensive but project id is na in fundable
  md_ppl_fundable <- data.table::fread("year2/MD/data/MD_Y2_SFY25_Fundable_List.csv",
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      project_number = ifelse(applicant == "TOWN OF NORTH EAST" & county == "CECIL" & is.na(project_number), "DW0042", project_number),
      project_number = ifelse(project_number=="DW0067)", "DW0067", project_number),
      project_number = ifelse(project_number=="DW007", "DW0007", project_number),
      list = "SFY25 fundable",
      total_funding = clean_numeric_string(total_funding),
      expecting_funding = ifelse(total_funding == "0", "No Information", "Yes"),
      principal_forgiveness = 
        convert_to_numeric(dwsrf_base_pf, fill_na_0 = TRUE) + convert_to_numeric(bil_general_pf, fill_na_0 = TRUE) +
        convert_to_numeric(bil_lsl_ffy22_pf, fill_na_0 = TRUE) + convert_to_numeric(bil_lsl_ffy23_pf, fill_na_0 = TRUE) +
        convert_to_numeric(bil_pfas_ec_ffy22, fill_na_0 = TRUE) + convert_to_numeric(bil_pfas_ec_ffy23, fill_na_0 = TRUE),
      project_type = dplyr::case_when(
        convert_to_numeric(bil_pfas_ec_ffy22, fill_na_0 = TRUE) > 0 ~ "Emerging Contaminants",
        convert_to_numeric(bil_pfas_ec_ffy23, fill_na_0 = TRUE) > 0 ~ "Emerging Contaminants",
        convert_to_numeric(bil_lsl_ffy22_loan, fill_na_0 = TRUE) > 0 ~ "Lead",
        convert_to_numeric(bil_lsl_ffy22_pf, fill_na_0 = TRUE) > 0 ~ "Lead",
        convert_to_numeric(bil_lsl_ffy23_loan, fill_na_0 = TRUE) > 0 ~ "Lead",
        convert_to_numeric(bil_lsl_ffy23_pf, fill_na_0 = TRUE) > 0 ~ "Lead",
        .default = NA
      )
    ) |>
    dplyr::select(project_number, list, total_funding, expecting_funding, principal_forgiveness, project_type) 
  
  # sum(md_ppl_fundable$project_number %in% md_ppl_comprehensive$project_number)

  # [keep] project number '/25' suffix was removed from the raw data
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
      # [keep] Extracted from Applicant Name/County in Comprehensive List by keeping text preceeding the first comma, reviewed in md_clean
      borrower = stringr::str_remove(borrower, '^[^,]+,\\s*'),
      # [keep] extracted from source as pwsid in raw data
      pwsid = stringr::str_squish(pwsid),
      pwsid = ifelse(is.na(pwsid), "No Information", pwsid),
      pwsid = ifelse(pwsid == "MDREG", "No Information", pwsid),
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
      population = ifelse(benefited_population=="?", "No Information", benefited_population),
      # [keep] Disadv was encoded as 1 and 0 for Yes and No, respectively
      disadvantaged = ifelse(disadvantaged == "1", "Yes", "No"),
      project_rank = stringr::str_squish(rank),
      project_score = stringr::str_squish(points),
      expecting_funding = ifelse(is.na(expecting_funding), "No", expecting_funding),
      state = "Maryland",
      state_fiscal_year = "2025",
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
 
  md_clean <- md_clean |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        project_type == "Lead" & project_description == "REPLACE APPROX 48,000 LF OF 2-IN OR LESS GALVANIZED SECONDARY SERVICE LINES UP TO EACH RESIDENTIAL WATER METER" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "REPLACE WATER METERS AND CRADLES BECAUSE THEY CONTAIN LEAD" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "REPLACE 5,000 LF KNOWN CAST IRON SERVICE MAINS & CONNECTORS" ~ paste0(project_description, " | FT: LSLR"),
        project_type == "Lead" & project_description == "HIRE CONTRACTOR TO DEVELOP AND MAINTAIN SERVICE LINE INVENTORIES" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "HIRE COMPANY TO DEVELOP AND MAINTAIN SERVICE LINE INVENTORIES PER EPA LEAD & COPPER RULE REVISIONS" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "CATALOG ALL WATER SERVICES WITHIN WATER DISTRIBUTION SYSTEM" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "EMPLOY 3RD PARTY TO HELP IDENTIFY AREAS THAT MAY HAVE LEAD SERVICE LINES OR LEAD PARTS" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "CATALOG ALL WATER SERVICES WITHIN THE WATER DISTRIBUTION SYSTEM" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "CONTRACT TO DEVELOP AND MAINTAIN SERVICE LINE INVENTORIES TO COMPLY WITH EPA LCRR" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "CATALOG OF ALL WATER SERVICES WITHIN SYSTEM" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "HIRE PROFESSIONAL ENGR FIRM TO DEVELOP, MANAGE, IMPLEMENT LCRR" ~ paste0(project_description, " | FT: LSLI"),
        project_type == "Lead" & project_description == "APPROX 58,000 SERVICE LINES NOT IDENTIFIED IN COUNTY MAINTENANCE RECORDS" ~ paste0(project_description, " | FT: LSLI"),
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
  
######## Decision: 12 projects classified as unknown, adressed upstream
  
####### SANITY CHECKS END #######
  

  run_tests(md_clean)

  rm(list=setdiff(ls(), "md_clean"))
  
  return(md_clean)
}