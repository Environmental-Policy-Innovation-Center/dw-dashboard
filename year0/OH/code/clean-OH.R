clean_oh_y0 <- function() {
  
  # NOTE: Because Ohio does not provide a project_id and does not maintain consistent entries for projects across tables
  # where borrower/descriptions/pwsid/funding amount can all vary slightly, an "epic_project_id" was manually created by
  # comparing and validating projects to join projects to the comprehensive table
  
  # NOTE: 6 projects on the fundable list are noted separately as unlikely to receive funding.
  # using the epic_project_id, manually identified programs based on borrower and estimated project cost.
  # Use this list when analysts discuss these outliers in the data dictionary.
  not_ef_list <- c("30", "57", "118", "127", "265", "317")
  
  # dac PPL: 
  oh_dac <- data.table::fread("./year0/OH/data/oh_dac_ppl.csv") |>
    janitor::clean_names() |>
    # these are all disadvantaged
    dplyr::mutate(disadvantaged = "Yes",
           list = "SFY22 DAC PPL") |>
    # selecting just the columns we need
    dplyr::select(epic_project_id, disadvantaged, 
           principal_forgiveness, project_score, rate, list)
  
  # regionalization projects: 
  oh_reg <- data.table::fread("./year0/OH/data/oh_regionalization_ppl.csv") |>
    janitor::clean_names() |>
    dplyr::mutate(list = "SFY22 Regionalization PPL") |>
    dplyr::select(epic_project_id, project_score, rate, 
           estimated_principal_forgiveness, list)
  
  # merging dac and reg ppls together since they may overlap: 
  oh_dac_reg <- merge(oh_dac, oh_reg, by = "epic_project_id", all = T) |>
    # fill in gaps for project score - can projects on both lists have the
    # same project score 
    dplyr::mutate(
      #commented out to avoid No Information in the clean data set for projects that do have a score
      # project_score = case_when(is.na(project_score.y) ~ project_score.x, 
      #                                is.na(project_score.x) ~ project_score.y, 
      #                                TRUE ~ NA), 

      project_score = dplyr::coalesce(project_score.x, project_score.y), #consulted w lauren give priority to DAC list (in this case values are the same in both list)                               
           # paste the rates together, which will be string matched to identify 
           # project type 
           rate = paste(rate.x, rate.y, sep = " "), 
           # fill in gaps for PF
           principal_forgiveness = case_when(!is.na(principal_forgiveness) ~ principal_forgiveness, 
                                             TRUE ~ estimated_principal_forgiveness),
           principal_forgiveness = ifelse(principal_forgiveness=="", "0", principal_forgiveness),
           list = case_when(
             #NOTE: when overlaps exist, prioritize DAC list over REG list for tracking
             is.na(list.x) ~ list.y,
             TRUE ~ list.x)
           ) |>
    # remove extra columns from the merge
    dplyr::select(-c("project_score.x", "project_score.y", 
              "rate.x", "rate.y", "estimated_principal_forgiveness", "list.x", "list.y"))
  
  # projects eligible for funding (all projects will appear on 
  # this list, aside from one lead project)
  oh_fundable <- data.table::fread("./year0/OH/data/oh_comp_ppl.csv") |>
    janitor::clean_names() |>
    # all of these are expecting funding
    dplyr::mutate(expecting_funding = case_when(
      epic_project_id %in% not_ef_list ~ "No",
      TRUE ~ "Yes")) |>
    # removing some extra cols we don't need
    dplyr::select(!c("loan_type", "estimated_award_date", "district_office"))
  
  # merging fundable, and the dac-regionalization lists
  oh_comp <- merge(oh_fundable, oh_dac_reg, by = "epic_project_id", all = T) |>
    # adding the rates together for string matching
    dplyr::mutate(rate = paste(rate.x, rate.y, sep = " ")) |>
    # finding the presence of HAB or LSL in the rate columns: 
    dplyr::mutate(project_type = case_when(grepl("HAB|PFAS", rate) ~ "Emerging Contaminants", 
                                    grepl("LSL", rate) | grepl("lead", project, ignore.case=TRUE) ~ "Lead", 
                                    # and the other strings listed for the 
                                    # fundbale list
                                    grepl(ec_str, project, ignore.case=TRUE) ~ "Emerging Contaminants", 
                                    TRUE ~ "General")) |>
    # trimming extra cols
    dplyr::select(-c("rate.x", "rate.y"))
    
  
  # HAB/PFAS list: 
  oh_ec <- data.table::fread("./year0/OH/data/oh_pfas_ppl.csv") |>
    janitor::clean_names() |>
    # these are all EC
    dplyr::mutate(project_type = "Emerging Contaminants",
           list = "SFY22 EC PPL") |>
    dplyr::select(epic_project_id, project_type, list)
  
  
  # LSL list: note there is one project that does not appear on the fundable 
  # list, but should be considered fundable
  oh_lead_all <- data.table::fread("./year0/OH/data/oh_lead_ppl.csv") |>
    janitor::clean_names() |>
    # these are all lead
    dplyr::mutate(project_type = "Lead",
           list = "SFY22 Lead PPL")
  
  # these are the lead projects that appear on the fundable list
  oh_lead_fundable <- oh_lead_all |>
    dplyr::filter(epic_project_id %in% oh_fundable$epic_project_id) |>
    dplyr::select(epic_project_id, project_type, list)
  
  # the not fundable project but prepping for a bind_rows later
  oh_lead_not_fundable <- oh_lead_all |>
    dplyr::filter(!(epic_project_id %in% oh_fundable$epic_project_id)) |>
    dplyr::mutate(expecting_funding = "Yes") |>
    dplyr::select(-c("loan_type", "estimated_award_date", "district_office"))
  
  
  # combing ec and lead: 
  oh_lead_ec <- bind_rows(oh_ec, oh_lead_fundable)
  
  
  # bring everything back together
  oh_clean_lead_ec <- merge(oh_comp, oh_lead_ec, 
                            by = "epic_project_id", all = T) |>
    # handle project types - note it seems like the state does not consider 
    # THM projects as EC. They do not appear on the EC list but are captured 
    # by our sting matching
    dplyr::mutate(project_type = case_when(!is.na(project_type.y) ~ project_type.y, 
                                    is.na(project_type.y) ~ project_type.x, 
                                    .default = "check"),
                                    # is.na(project_type.x) ~ project_type.y), #replacing this condition with a default "check" string, since project_type.y should already have been assigned; verified no check string appears
                  list = dplyr::coalesce(list.x, list.y)                
                                  )|>
    select(-c("project_type.x", "project_type.y", "list.x", "list.y"))
  
  # bringing back the lead project that is not eligible for funding, and 
  # finish standardizing columns: 
  oh_clean <- bind_rows(oh_clean_lead_ec, oh_lead_not_fundable) |>
    # process numeric cols: 
    dplyr::mutate(
      principal_forgiveness = case_when(
      # not EF projects explicitly not receiving PF. in case of overlap, set to No Info first
      # then use normal function for keeping numeric value or setting NA to No Info
      epic_project_id %in% not_ef_list ~ "0",
      TRUE ~ clean_numeric_string(principal_forgiveness)), 
           requested_amount = clean_numeric_string(estimated_loan_amount), 
           funding_amount = as.character(NA),
           population = clean_numeric_string(population),
           project_score = clean_numeric_string(project_score), 
           disadvantaged = case_when(
             !is.na(disadvantaged) ~ disadvantaged,
             grepl("DIS", rate) ~ "Yes", #cautious with pasting, suggest using paste with " " separator, example DISDISNA , confirmed they are being correctly captured 
             TRUE ~ "No"), 
           pwsid = stringr::str_squish(pws_id), 
           borrower = stringr::str_squish(entity), 
           community_served = stringr::str_squish(county),
           project_id = as.character(NA), 
           project_name = as.character(NA),
           project_cost = as.character(NA), 
           project_description = stringr::str_squish(project), 
           project_rank = as.character(NA), 
           state = "Ohio", 
           state_fiscal_year = "2022",
           list = tidyr::replace_na(list, "SFY22 Fundable List and Comprehensive List")) |>
    dplyr::select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost, requested_amount,
           funding_amount, principal_forgiveness, population, project_description, disadvantaged, project_rank,
           project_score, expecting_funding, state, state_fiscal_year, list)
  
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

  ### 13 unknowns
  

  oh_clean <- oh_clean |>
    dplyr::left_join(
      data.table::data.table(
        community_served = c("Harrison",  "Columbiana","Highland","Lorain","Carroll","Muskingum", "Wood","Meigs","Ottawa","Lawrence","Tuscarawas", "Huron","Sandusky"),
        borrower = c("Cadiz", "East Palestine","Hillsboro","Lorain","Malvern", "New Concord","North Baltimore","Pomeroy","Port Clinton", "Proctorville","Sugarcreek","Wakeman","Woodville"),
        project_description = c(
          "Water Distribution and Storage System Imps","Waterline Replacement",
          "N. West St. Water System Imps",
          "East Lorain Waterline Replacement","Phase 1 Waterline Replacement",
          "Lead Service Line Investigation",
          "Water System Imps (Contract A WL; Contract B Tank)",
          "Water System Improvements",
          "Water and Sanitary Sewer Infrastructure Improvements","Water System Improvements",
          "Factory, Main, Maple & Broadway WL and LSL Repl",
          "Farmer Street and Cooper Street Waterline Imps",
          "Waterline Improvements Phase 3"),
        new_lead_type = c("lslr","lslr", "lslr","lslr","lslr","lsli","lslr","lslr","lslr", "lslr","lslr","lslr","lslr")
      ),
      by = c("community_served", "borrower", "project_description")
    ) |>
    dplyr::mutate(
      project_description = dplyr::case_when(
        !is.na(new_lead_type) ~ paste0(project_description, " | FT: ", stringr::str_to_upper(new_lead_type)),
        .default = project_description
      )
    ) |>
    dplyr::select(-new_lead_type)
  
  ####### SANITY CHECKS END #######

  run_tests(oh_clean)
  rm(list=setdiff(ls(), "oh_clean"))
  
  return(oh_clean)
}