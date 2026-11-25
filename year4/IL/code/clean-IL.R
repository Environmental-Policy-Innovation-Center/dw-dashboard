clean_il_y4 <- function() {

  # Fundable Lists -----
  ## Fundable PPL -----
  il_ppl_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Fundable_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = clean_numeric_string(projected_loan_amount),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      list = "fundable"
    )
  
  ## Lead Fundable PPL -----
  il_lead_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Fundable_List.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = as.character(convert_to_numeric(principal_forgiveness_reserved, fill_na_0 = TRUE) + convert_to_numeric(loan_funding_reserved, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness_reserved),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Lead",
      list = "lead fundable"
    )

  ## EC Fundable PPL -----
  il_ec_f <- data.table::fread("year4/IL/data/IL_Y4_SFY26_EC_Fundable_List.csv",
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = as.character(convert_to_numeric(principal_forgiveness, fill_na_0 = TRUE) + convert_to_numeric(pwslp_funds_reserved, fill_na_0 = TRUE)),
      principal_forgiveness = clean_numeric_string(principal_forgiveness),
      project_score = loan_priority_score,
      expecting_funding = "Yes",
      project_type = "Emerging Contaminants",
      list = "ec fundable"
    )
  

  # Other Lists -----
  ## Exhausted Funding List -----
  il_ppl_a <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "exhausted"
    )
  
  ## Planning Approval List -----
  il_ppl_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      list = "planning approval"
    )
  
  ## No Planning Approval List -----
  il_ppl_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_Base_IIJAGenSupp_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      list = "no planning approval"
    )

  ## Lead Exhausted Funding List -----
  il_lead_a <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      project_score = loan_priority_score,
      expecting_funding = "No",
      list = "lead exhausted"
    )
  
  ## Lead Planning Approval List -----
  il_lead_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_Planning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead planning approval"
    )
  #Note all loan_priority_score 0 or NA, therefore will be No Information downstream as per DD

  ## Lead No Planning Approval List -----
  il_lead_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_LSLR_NoPlanning_Approval_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Lead",
      list = "lead no planning approval"
    )
    
  ## EC Exhausted Funding List -----
  il_ec_no_pla_ap <- data.table::fread("year4/IL/data/IL_Y4_SFY26_EC_Exhausted_Funding_List.csv",
                    colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      funding_amount = "No Information",
      principal_forgiveness = "No Information",
      expecting_funding = "No",
      project_type = "Emerging Contaminants",
      project_score = loan_priority_score,
      list = "ec exhausted"
    )

  # Bind all lists (note: any data frame that starts with il_ will be bound) -----
  dfs <- mget(ls(pattern = "^il_"))
  dfs <- dfs[sapply(dfs, is.data.frame)]
  
  il_merge <- dplyr::bind_rows(dfs)

  # Clean and process data -----
  il_clean <- il_merge |>
    dplyr::mutate(
      community_served = as.character(NA),
      borrower = str_squish(loan_applicant),
      borrower = str_to_title(borrower, locale="en"),
      pwsid = coalesce(facility_no, "No Information"),
      pwsid = ifelse(grepl("^[0-9]+$", pwsid), paste0("IL", pwsid), pwsid), #prepend IL if character is only numeric
      project_id = str_squish(l17_number),
      project_id = ifelse(is.na(project_id), "No Information", project_id),
      project_name = as.character(NA),
      project_cost = as.character(NA),
      requested_amount = dplyr::case_when(
        list == "exhausted" ~ clean_numeric_string(projected_loan_amount),
        list == "planning approval" ~ clean_numeric_string(projected_loan_amount),
        list == "no planning approval" ~ clean_numeric_string(projected_loan_amount),
        list == "ec exhausted" ~ clean_numeric_string(requested_loan_amount),
        list == "lead exhausted" ~ clean_numeric_string(projected_loan_amount), ## DD may need to be corrected; see error 1
        list == "lead planning approval" ~ clean_numeric_string(projected_loan_amount),
        list == "lead no planning approval" ~ clean_numeric_string(projected_loan_amount), ## DD may need to be corrected; see error 2
        .default = "No Information"
      ),
      project_description = str_squish(project_description),
      project_description = str_to_sentence(project_description),
      project_type = dplyr::case_when(
        !is.na(project_type) ~ project_type,
        grepl("lsl|lead", project_description, ignore.case=TRUE) ~ "Lead",
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        TRUE ~ "General"
      ),
      population = dplyr::case_when(
        list == "fundable" ~ clean_numeric_string(population),
        list == "exhausted"  ~ clean_numeric_string(population),
        .default = "No Information"
      ),
      disadvantaged = as.character(NA),
      project_rank = as.character(NA),
      project_score = ifelse(is.na(project_score), "No Information", project_score),
      state = "Illinois",
      state_fiscal_year = "2026"
    ) |>
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  #il_clean |> dplyr::group_by(project_id) |> dplyr::tally() |> dplyr::filter(n>1)

  # Check project id
  # dupes_list <- il_clean |> dplyr::filter(project_id %in% c("6742","6744", "6810", "7030", "7281", "7594")) |> arrange(project_id) |> View()
  
  il_clean <- il_clean |>
    dplyr::mutate(keep = dplyr::case_when(
      # drop the ec fundable duplicates because they lack the population column, data needed from ec fundable is manually entered below
      project_id == "6742" & list == "ec fundable" ~ FALSE, 
      project_id == "6744" & list == "ec fundable" ~ FALSE, 
      project_id == "6810" & list == "ec fundable" ~ FALSE, 
      project_id == "7030" & list == "ec fundable" ~ FALSE, 
      project_id == "7281" & list == "exhausted" ~ FALSE, # default to ec exhausted
      project_id == "7594" & list  == "lead planning approval" ~ FALSE, # keep first instance (same info)
      .default = TRUE)
  ) |>
  dplyr::filter(keep) |>
  dplyr::select(-keep) |>
  dplyr::mutate(
      pwsid =ifelse(project_id == "6744", "IL0335030", pwsid),
      pwsid =ifelse(project_id == "6810", "IL1434750", pwsid),
      pwsid =ifelse(project_id == "7281", "IL1190200", pwsid)
    )

  ####### Decision:  
  # Notes for projects on multiple project lists:
  
  # For projects that appear in both the Base/IIJA Gen Supp fundable List and EC Fundable List,
  # we will default to using the funding amount info in the ec fundable List AND
  # PF is sum of Principal Forgiveness from both lists is the total principal forgiveness 
  
  # Projects: 6742, 6744, 6810, 7030
  
  il_clean <- il_clean |>
    dplyr::mutate(
      funding_amount = case_when(
        project_id == "6742" ~ "10698301",
        project_id == "6744" ~ "27000000",
        project_id == "6810" ~ "8500000",
        project_id == "7030" ~ "8499000",
        TRUE ~ funding_amount
      ),
      principal_forgiveness = case_when(
        project_id == "6742" ~ "9709490",
        project_id == "6744" ~ "9984540",
        project_id == "6810" ~ "7100000",
        project_id == "7030" ~ "8499000",
        TRUE ~ principal_forgiveness
      )
    )
  
  #For projects that appear in both the Base/IIJA Gen Supp Fundable List and EC Exhausted Funding List, 
  # default to funding amount info in the Base/IIJA Gen Supp Fundable List (since only projects on the fundable list are expecting funding)
  
  # no projects meet this criteria for Y4
  
  
  # Check for disinfection byproduct in description
  # il_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string
    
  # Check for lead subtypes
  # il_clean |>
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

  ####### Decision: No lead projects classified as both
  
  # # Check for lead subtypes: Unknown
  # il_clean |>
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

  ####### Decision: 7428 project_id --> replacement

  il_clean <- il_clean |>
      dplyr::mutate(
          project_description = ifelse(project_type=="Lead" & project_id == "7428", paste0(project_description, " |FT: LSLR"), project_description)
        )
  
  # Check pwsid lengths
  # il_clean |>
  #   dplyr::mutate(
  #     length_pwsid = stringr::str_length(pwsid)
  #   ) |> 
  #   dplyr::filter(!length_pwsid == 9) |>
  #   dplyr::filter(!pwsid == "No Information")   

  il_clean <- il_clean |>
    dplyr::mutate(
      pwsid = dplyr::case_when(
        borrower == "Argenta" & pwsid == "IL50050" ~ "IL1150050",
        borrower == "Brookfield" & pwsid == "IL031330" ~ "IL0310330",
        borrower == "Sandwich" & pwsid == "IL034850" ~ "IL0374850",
        borrower == "Morris" & pwsid == "IL630600" ~ "IL0630600",
        borrower == "Winnebago" & pwsid == "IL20105000" ~ "IL2010500",
        borrower == "Apple River" & pwsid == "IL08550050" ~ "IL0850050",
        borrower == "Mount Vernon" & pwsid == "IL810300" ~ "IL0810300",
        borrower == "Scales Mound" & pwsid == "IL850400" ~ "IL0850400", 
           
        borrower == "Breese" & pwsid == "No Information" & project_id == "4189" ~ "IL0270250",
        borrower == "Calumet City" & pwsid == "No Information" & project_id == "5766" ~ "IL0310390",
        borrower == "Calumet City" & pwsid == "No Information" & project_id == "5767" ~ "IL0310390",
        borrower == "Aurora" & pwsid == "No Information" & project_id == "6017" ~ "IL0894070",
        borrower == "Lansing" & pwsid == "No Information" & project_id == "6119" ~ "IL0311590",
        borrower == "Channahon" & pwsid == "No Information" & project_id == "6271" ~ "IL1970200",
        borrower == "Palos Heights" & pwsid == "No Information" & project_id == "6286" ~ "IL0312370",
        borrower == "Palos Heights" & pwsid == "No Information" & project_id == "6287" ~ "IL0312370",
        borrower == "Palos Heights" & pwsid == "No Information" & project_id == "6288" ~ "IL0312370",
        borrower == "Palos Heights" & pwsid == "No Information" & project_id == "6289" ~ "IL0312370",
        borrower == "Evanston" & pwsid == "No Information" & project_id == "6321" ~ "IL0310810",
        borrower == "Peoria Heights" & pwsid == "No Information" & project_id == "6333" ~ "IL1434750",
        borrower == "Markham" & pwsid == "No Information" & project_id == "6347" ~ "IL0311770",
        borrower == "Hanover Park" & pwsid == "No Information" & project_id == "6355" ~ "IL0314480",
        borrower == "Fox Lake" & pwsid == "No Information" & project_id == "6376" ~ "IL0970200",
        borrower == "Dolton" & pwsid == "No Information" & project_id == "6393" ~ "IL0310690",
        borrower == "Seaton" & pwsid == "No Information" & project_id == "6541" ~ "IL1310350",
        borrower == "West Dundee" & pwsid == "No Information" & project_id == "6563" ~ "IL0890950",
        borrower == "West Dundee" & pwsid == "No Information" & project_id == "6564" ~ "IL0890950",
        borrower == "Rockdale" & pwsid == "No Information" & project_id == "6778" ~ "IL1970850",
        borrower == "Cary" & pwsid == "No Information" & project_id == "6813" ~ "IL1110100",
        borrower == "Harwood Heights" & pwsid == "No Information" & project_id == "6935" ~ "IL0311140",
        borrower == "Summit" & pwsid == "No Information" & project_id == "6952" ~ "IL0310060",
        borrower == "Summit" & pwsid == "No Information" & project_id == "6953" ~ "IL0310060",
        borrower == "Summit" & pwsid == "No Information" & project_id == "6954" ~ "IL0310060",
        borrower == "Summit" & pwsid == "No Information" & project_id == "6955" ~ "IL0310060",
        borrower == "Westmont" & pwsid == "No Information" & project_id == "6958" ~ "IL0430950",
        borrower == "Westmont" & pwsid == "No Information" & project_id == "6959" ~ "IL0430950",
        borrower == "Westmont" & pwsid == "No Information" & project_id == "6960" ~ "IL0430950",
        borrower == "Harvey" & pwsid == "No Information" & project_id == "6979" ~ "IL0311110",
        borrower == "Blue Island" & pwsid == "No Information" & project_id == "6985" ~ "IL0310240",
        borrower == "Dalzell" & pwsid == "No Information" & project_id == "7000" ~ "IL0110250",
        borrower == "Dalzell" & pwsid == "No Information" & project_id == "7001" ~ "IL0110250",
        borrower == "Mazon" & pwsid == "No Information" & project_id == "7030" ~ "IL0630500",
        borrower == "Pana" & pwsid == "No Information" & project_id == "7047" ~ "IL0210500",
        borrower == "Gorham" & pwsid == "No Information" & project_id == "7051" ~ "IL0770350",
        borrower == "Decatur" & pwsid == "No Information" & project_id == "7055" ~ "IL1150150",
        borrower == "Murdale Water District" & pwsid == "No Information" & project_id == "7080" ~ "IL0775200",
        borrower == "Forest Park" & pwsid == "No Information" & project_id == "7101" ~ "IL0310900",
        borrower == "North Park Water District" & pwsid == "No Information" & project_id == "7110" ~ "IL2015500",
        borrower == "Fox Lake" & pwsid == "No Information" & project_id == "7140" ~ "IL0970200",
        borrower == "Fox Lake" & pwsid == "No Information" & project_id == "7141" ~ "IL0970200",
        borrower == "White Hall" & pwsid == "No Information" & project_id == "7165" ~ "IL0610400",
        borrower == "Jacksonville" & pwsid == "No Information" & project_id == "7166" ~ "IL1370200",
        borrower == "Fox Lake" & pwsid == "No Information" & project_id == "7173" ~ "IL0970200",

        borrower == "Shannon"	& pwsid == "IL0150330" & project_id == "6913" ~ "IL0150300", #https://water.epa.state.il.us/dww/JSP/WaterSystemDetail.jsp?tinwsys_is_number=715960&tinwsys_st_code=IL&wsnumber=IL0150300
        borrower == "Buda"	& pwsid == "IL0010100" & project_id == "7011" ~ "IL0110100",  #https://water.epa.state.il.us/dww/JSP/WaterSystemDetail.jsp?tinwsys_is_number=715919&tinwsys_st_code=IL&wsnumber=IL0110100   
        
        .default = pwsid
      )
    )
  
  ####### SANITY CHECKS END #######

  run_tests(il_clean)
  rm(list=setdiff(ls(), "il_clean"))
  
  return(il_clean)

}