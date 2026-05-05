clean_oh_y3 <- function() {
  
  base_path <- "year3/OH/data"
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  not_ef_list <- c("44", "59", "77", "101", "103", "110", "215", "227", "247", "258", "276", "294",
                   "334", "367", "451", "461", "468", "479", "504", "NF1")
  
  # ec_sdc_list <- c("132", "59", "93", "109", "478", "383", "30", "443", "421", "506", "491", "SDC1", "SDC2")
  # epic id "59" is not in the sdc list (it seems to be the construction counterpart to SDC1 (Design) )
  ec_sdc_list <- c("132", "93", "109", "478", "383", "30", "443", "421", "506", "491", "SDC1", "SDC2")

  oh_missing_not_funded <- data.table::fread(file.path(base_path, "oh-missing-not-funded.csv"),
                                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::mutate(
      list = "Missing Not Funded"
    )
  
  # import SDC-funded EC projects, keep two projects not in fundable list to add in
  oh_ec_sdc <- data.table::fread(file.path(base_path, "oh-sdc-ec.csv"),
                     colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(entity %in% c("Beverly", "Oxford")) |>
    dplyr::rename(estimated_loan_amount = sdc_ec_grant) |>
    dplyr::mutate(
    #Assigned EC project type and list provenance
      project_type = "Emerging Contaminants",
      list = "EC SDC List",
      expecting_funding = "No Information"
    )

  # Comprehensive Project List
  oh_fundable <- data.table::fread(file.path(base_path, "oh-ppl-base.csv"),
                   colClasses = "character", na.strings = "") |>
    janitor::clean_names()
  
  oh_fundable <- bind_rows(oh_fundable, oh_missing_not_funded, oh_ec_sdc) |>
      dplyr::mutate(
        borrower = stringr::str_squish(entity),
        project_description = stringr::str_squish(project),
        population = clean_numeric_string(sdwis_population),
        pwsid = stringr::str_squish(pws_id),
        community_served = stringr::str_squish(county),
        expecting_funding = ifelse(epic_project_id %in% not_ef_list, "No", "Yes"),
        #  # SDC-funded list over-rides not expecting funding list (only applicable to state funds)
        #  expecting_funding = ifelse(epic_project_id %in% ec_sdc_list, "Yes", expecting_funding),
        requested_amount = clean_numeric_string(estimated_loan_amount),
        project_type = ifelse(epic_project_id %in% ec_sdc_list, "Emerging Contaminants", project_type)
        ) |>
      dplyr::select(epic_project_id, borrower, project_description, requested_amount,
           population, pwsid, community_served, expecting_funding,
           rate, project_type, list)
  
  #manually update an estimated loan amount from ec sdc per comment from policy team
  oh_fundable <- oh_fundable |>
      dplyr::mutate(requested_amount = ifelse(epic_project_id == "421", "888500", requested_amount))

  # DAC Principal Forgiveness List
  oh_dac <- data.table::fread(file.path(base_path, "oh-ppl-pf.csv"),
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # removing numeric strings from estimated pf for the clean_numeric_string
    # function later
    dplyr::mutate(dac_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), "Bypass", estimated_principal_forgiveness), 
           # these are all disadvantaged 
           disadvantaged = "Yes",
           list = "SFY25 DAC PPL") |> 
    dplyr::select(epic_project_id, disadvantaged, project_score, rate, 
           dac_pf, list)
  
  # Regionalization Principal Forgiveness List
  oh_reg <- data.table::fread(file.path(base_path, "oh-ppl-regional-pf.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # NOTE - there was one project "Phillipsburg - Drinking Water PFAS Remediation"
    # that didn't have a project ID but ends up on the ec_extra list, so it's safe
    # to filter here
    filter(!is.na(epic_project_id)) |>
    dplyr::mutate(
      reg_pf = ifelse(estimated_principal_forgiveness %in% 
                             c("Bypass 1", "Bypass 2"), "Bypass", estimated_principal_forgiveness),
      list = "SFY25 Regionalization PPL"
      ) |>
    dplyr::select(epic_project_id, reg_pf, project_score, rate, list)
  
  # discount list - NOTE - this table is not really mentioned in the DD (except
  # for project_type) and the regionalization list already has that information, 
  # so opting to skip adding this table. 
  # oh_dis <- data.table::fread(file.path(base_path, "oh-discount.csv"),
  #                 colClasses = "character", na.strings = "") |>
  #   janitor::clean_names()
  
  # merging DAC & regionalization PPLs to handle pf cols: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) |>
    # pasting rates together for string matching 
    dplyr::mutate(rate = paste(rate.x, rate.y, sep = " "), 
           # can confirm project scores are the same where the lists overlap
           project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x), 
           # handling pf overlaps 
           principal_forgiveness = case_when(!is.na(reg_pf) & !(reg_pf %in% c("Bypass", "EC List")) ~ reg_pf, 
                                             !is.na(dac_pf) & !(dac_pf %in% c("Bypass", "EC List", "REG List")) ~ dac_pf),
           # All reg obs overlap with dac, defaulting to reg list
           list = ifelse(!is.na(list.y), list.y, list.x)
           ) |>
    # trimming extra cols: 
    dplyr::select(-c(rate.x, dac_pf, reg_pf, project_score.y, project_score.x, rate.y, list.x, list.y))
  
  
  # merging these together for project types:
  oh_main <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) |>
    dplyr::mutate(
      rate = paste(rate.x, rate.y, sep = " "),
      project_type = case_when(
        !is.na(project_type) ~ project_type,
        grepl("LSL", rate, ignore.case = T) ~ "Lead", 
        grepl("HAB|EC|PFAS", rate, ignore.case = T) ~ "Emerging Contaminants", 
        grepl(ec_str, project_description, ignore.case=TRUE) ~ "Emerging Contaminants",
        grepl("lead|lsl", project_description, ignore.case=TRUE) ~ "Lead",
        TRUE ~ "General"),
        #preserving list provenance of dac and reg
        list = dplyr::coalesce(list.y, list.x)    
      ) |>
    # trim extra columns
    select(-c(rate.x, rate.y, rate, list.x, list.y))

  # Emerging Contaminants List
  oh_ec <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                 colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are all not on the fundable list but will be added later
    dplyr::filter(grepl("$", estimated_ec_amount)) |>
    dplyr::mutate(
      project_type = "Emerging Contaminants",
      list = "SFY25 EC PPL",
      ec_pf = ifelse(est_ec_principal_forgiveness %in% 
                            c("Bypass 1", "Bypass 2"), "Bypass", est_ec_principal_forgiveness),
      project_score = stringr::str_squish(score_total_points)) |>
    dplyr::select(epic_project_id, ec_pf, project_score, project_type, list)

  # Get extra EC projects that don't match fundable list & prepping for 
  # a bind_rows later: 
  oh_ec_extra <- data.table::fread(file.path(base_path, "oh-ppl-ecr.csv"),
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    dplyr::filter(!grepl("$", estimated_ec_amount)) |>
    dplyr::mutate(borrower = str_squish(entity),
           project_description = stringr::str_squish(project),
           pwsid = stringr::str_squish(pws_id),
           community_served = stringr::str_to_sentence(county),
           project_score = stringr::str_squish(score_total_points),
           project_type = "Emerging Contaminants",
           list = "SFY25 EC PPL",
           requested_amount = clean_numeric_string(estimated_loan_amount),
           disadvantaged = "No",
           expecting_funding = "No",
           # principal_forgiveness = clean_numeric_string(est_ec_principal_forgiveness),
           principal_forgiveness = "No Information",
           population = "No Information") |>
    dplyr::select(borrower, project_description, pwsid, community_served,
           project_score, project_type, disadvantaged,requested_amount,
           expecting_funding, principal_forgiveness, population, list)
  
  # merging ec list and our main list to handle pf cols: 
  oh_main_ec <- merge(oh_main, oh_ec, by = "epic_project_id", all = T) |>
    # fixing project types
    dplyr::mutate(
      project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                   TRUE ~ project_type.x), 
      # can confirm pf does not overlap
      principal_forgiveness = case_when(!is.na(ec_pf) ~ ec_pf, 
                                             TRUE ~ principal_forgiveness), 
      # note the is a project w/ the desscription "PFAS Well Mitigation" 
      # that has a different project score from the DAC PPL. Confirmed 
      # that it should receive the project score on the EC list
      project_score = case_when(!is.na(project_score.y) ~ project_score.y, 
                                     TRUE ~ project_score.x),
      list = ifelse(!is.na(list.y), list.y, list.x)) |>
    dplyr::select(-c(project_type.x, project_type.y, project_score.x, project_score.y, 
              ec_pf, list.x, list.y))

  # Lead Service Line List  
  oh_lead <- data.table::fread(file.path(base_path, "oh-lslr.csv"),
                  colClasses = "character", na.strings = "") |>
    janitor::clean_names() |>
    # these are all lead projects
    dplyr::mutate(project_type = "Lead",
           list = "SFY25 Lead PPL",
           disadvantaged = ifelse(grepl("PF", rate), "Yes", "No"),
           # if PF present, eligible for PF, but unknown how much, so No Info
           principal_forgiveness = ifelse(grepl("PF", rate), "No Information", as.character(NA))) |>
    dplyr::select(epic_project_id, project_type, disadvantaged, principal_forgiveness, list)

  # Combine fundable, dc, regionalization, and discount ppls: 
  oh_comp <- merge(oh_main_ec, oh_lead, by = "epic_project_id", all = T) |>
    # default to project type for lead or ec lists: 
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y,
                                    TRUE ~ project_type.x),
           disadvantaged = ifelse(!is.na(disadvantaged.y), disadvantaged.y, disadvantaged.x),
           list = ifelse(!is.na(list.y), list.y, list.x),
           principal_forgiveness = ifelse(!is.na(principal_forgiveness.y), principal_forgiveness.y, principal_forgiveness.x)
           )  |>
    # remove extra columns
    dplyr::select(-c("project_type.x", "project_type.y", "disadvantaged.x", "disadvantaged.y",
              "principal_forgiveness.x", "principal_forgiveness.y", "list.x", "list.y"))
  
  # Add extra EC projects and final cleanup
  oh_clean <- bind_rows(oh_comp, oh_ec_extra) |>
    dplyr::mutate(
           principal_forgiveness = case_when(principal_forgiveness %in% c("Bypass", "*") ~ "0", 
                                             TRUE ~ principal_forgiveness)) |>
    dplyr::mutate(state = "Ohio",
           state_fiscal_year = "2025",
           # cleaning numeric strings: 
           principal_forgiveness = replace_na(principal_forgiveness, "0"),
           # set PF to 0 if not expecting funding
           principal_forgiveness = ifelse(epic_project_id %in% not_ef_list, "0", principal_forgiveness),
           principal_forgiveness = clean_numeric_string(principal_forgiveness),
           project_score = clean_numeric_string(project_score),
           # fill in extra cols: 
           funding_amount = as.character(NA),
           project_id = as.character(NA),
           project_name = as.character(NA),
           project_cost = as.character(NA),
           disadvantaged = ifelse(is.na(disadvantaged), "No", disadvantaged),
           project_rank = as.character(NA),
           list = ifelse(epic_project_id %in% ec_sdc_list, "EC SDC List", list),
           list = replace_na(list, "SFY25 Fundable List and Comprehensive List")
          ) 
  
  
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  ####### Decision: No project id
  
  # Check for disinfection byproduct in description
  # oh_clean |> dplyr::filter(grepl("disinfection byproduct", project_description))
  ####### Decision: No disinfection byproduct string

  # Check for lead subtypes: Both
  # oh_clean |>
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
  
  # Check for lead subtypes: Unknown
  # oh_clean |>
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
  #Decision: 37 classified as unknown

  oh_clean <- oh_clean |>
    dplyr::left_join(
      data.table::data.table(
        community_served = c("Hamilton","Hamilton",
                            "Hamilton","Hamilton","Hamilton","Hamilton","Hamilton",
                            "Hamilton","Hamilton","Hamilton","Hamilton","Hamilton",
                            "Pickaway","Franklin","Summit","Putnam","Ashtabula",
                            "Fulton","Ashtabula","Hamilton","Hardin","Hardin",
                            "Washington","Carroll","Williams","Meigs","Ottawa",
                            "Ottawa","Portage","Portage","Harrison","Clark",
                            "Trumbull","Ross","Washington","Harrison","Harrison"),
        borrower = c("Cincinnati","Cincinnati",
                            "Cincinnati","Cincinnati","Cincinnati","Cincinnati",
                            "Cincinnati","Cincinnati","Cincinnati","Cincinnati",
                            "Cincinnati","Cincinnati","Circleville","Columbus","Akron",
                            "Columbus Grove","Conneaut","Fayette","Geneva",
                            "Glendale","Kenton","Kenton","Lowell","Malvern",
                            "Montpelier","Pomeroy","Port Clinton","Port Clinton",
                            "Portage County","Portage County","Scio","South Charleston",
                            "Warren","Bainbridge","Belpre","Bowerston","Cadiz"),
        requested_amount = c("2772572", "2772572", "2910982", "1816307", "1065191", "2186800", "4835650", "2485156", "4944400", "2994900", "3474200", "1456500", "1978000", "500000", "5472350", "2680051", "3100000", "3167500", "1000000", "4491800", "1558577", "5839599", "1066995", "6643500", "2561200", "2814800", "11458755", "325000", "508102", "3316800", "500000", "4305175", "3372975", "4830148", "1212564", "2000000", "5845000"),
        project_description = c("McMillan - Calhoun Area Water Main Replacement",
                                  "Burch - Shaw Area Water Main Replacement",
                                  "Erie - Kendall Area Water Main Replacement",
                                  "Branch Only- Beech, Eighth, St. Lawrence LSL",
                                  "Branch Only- Jonathan, Ruth, Woodburn LSL","Southern Hawthorne Water Main Replacement",
                                  "Monastery - Mt. Adams Water Main Replacement",
                                  "MLK - Lakewood Area Water Main Replacement",
                                  "McHenry - Wooster Area Water Main Replacement",
                                  "Lyon - Wheeler Area Water Main Replacement",
                                  "Fire Flow 23 Water Main Replacement",
                                  "Baker - Beechcrest Water Main Replacement","Court Street Waterline Replacement",
                                  "N. Sixth St & E. 3rd Ave Area WL Imps CIP",
                                  "Water Main Replacement Program 2024",
                                  "Main Street Downtown Water Project LSL",
                                  "Park & Day Streets Waterline Replacement",
                                  "Water Distribution System Replacement - Phase 2","LSL Mapping","Water System Upgrades",
                                  "Detroit Street",
                                  "Downtown Waterline Replacement Phase 2B","Water Tank and Watermain Replacement",
                                  "Water Line Replacement Phase 2",
                                  "Main Street Waterline LSL","Breezy Heights Tank Proposed Wells",
                                  "Water and Sanitary Sewer Infrastructure Improvements",
                                  "Laurel Street Reconstruction",
                                  "Mantua Water Treatment Plant Liquid Chlorine",
                                  "Village of Mantua Distribution Replacement Ph 2","2023 Waterline and LSL",
                                  "Water Treatment Systems Upgrades",
                                  "2022 Waterline Replacement Program (Areas B and C)",
                                  "Waterline Replacement Project",
                                  "Water Tank Supply Main and Various WM Replacements",
                                  "Distribution System and Meter Replacement","Phase II Water System Improvements"),
        list = c("SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL",
                            "SFY25 Lead PPL","SFY25 Lead PPL","SFY25 Lead PPL"),
        new_lead_type = c("lslr","lslr","lslr","lslr",
                            "lslr","lslr","lslr","lslr","lslr","lslr","lslr",
                            "lslr","lslr","lslr","lslr","lslr","lslr","lslr",
                            "lsli","lslr","unknown","lslr","lslr","lslr","lslr",
                            "lslr","lslr","lslr","lslr","lslr","lslr","lslr","lslr",
                            "lslr","lslr","lslr","lslr")
      ),
      by = c("community_served", "borrower", "project_description", "requested_amount", "list")
    ) |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        !is.na(new_lead_type) ~ paste0(project_description, " | FT: ", stringr::str_to_upper(new_lead_type)),
        .default = project_description
      )
    ) |>
    dplyr::select(-new_lead_type)
    # Decision: 2 were left as unknown

  ####### SANITY CHECKS END #######


  # Produce Other Federal and State Funds dataset
  ec_sdc_table <- data.table::fread(file.path(base_path, "OH_Y3_SFY25_State_Funds.csv"),
                       colClasses = "character", na.strings = "") |>
    janitor::clean_names()

  oh_ofsf <- oh_clean |>
    dplyr::filter(epic_project_id %in% ec_sdc_list) |>
    dplyr::left_join(ec_sdc_table |> dplyr::select(epic_project_id,sdc_ec_grant), by= "epic_project_id") |>
    dplyr::mutate(
      project_cost_ofsf = as.character(NA), #full column is No Information, keeping consistency to core dataset 
      requested_amount_ofsf = as.character(NA),
      funding_amount_ofsf = sdc_ec_grant,
      expecting_funding_ofsf = "Yes"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, project_cost_ofsf,
           requested_amount, requested_amount_ofsf, funding_amount, funding_amount_ofsf, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, expecting_funding_ofsf, state, state_fiscal_year, list)
  
  save_update_ofsf(oh_ofsf)

  oh_clean <- oh_clean |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year, list)
  

  # Run validation tests
  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}