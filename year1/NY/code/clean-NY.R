clean_ny_y1 <- function() {
  
## BIL PPL
  # (120,10) -> (120,11)
  ny_bil <- fread("year1/NY/data/32-NewYork_bil_ppl.csv",
                  colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "General",
           # “H” in Score on Annual List or project is included on BIL-GS PPL.
           disadvantaged = "Yes",
           expecting_funding = dplyr::case_when(
        row_number() <= 27 ~ "Yes",
        row_number() > 27 ~ "No"
      )
    )
  
## BIL AWARDS
  ny_bil_awards <- fread("year1/NY/data/ny-bil-gen-supp-awards.csv",
                         colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(funding_amount = convert_to_numeric(bil_gs_grant_award, TRUE) + convert_to_numeric(bil_gs_0_percent_loan_award, TRUE),
           funding_amount = clean_numeric_string(funding_amount),
           principal_forgiveness = clean_numeric_string(bil_gs_grant_award),
           expecting_funding = "Yes") %>%
    select(project_number, funding_amount, principal_forgiveness, expecting_funding)
  
## Combine BIL
  ny_bil_combined <- ny_bil %>%
    left_join(ny_bil_awards, by=c("project_number")) %>%
    # drop two projects that appear with more info on the EC funding list
    filter(!project_number %in% c("19171", "18971")) %>%
    mutate(population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost),
           ) %>%
    dplyr::mutate(
      expecting_funding = dplyr::case_when(
        !is.na(expecting_funding.y) ~ expecting_funding.y,
        .default = expecting_funding.x
      )
    )|>
    select(project_number, county, system_name, borrower, description, population, project_cost,
           score, project_type, disadvantaged, funding_amount, principal_forgiveness, expecting_funding)
  
  
  

## Lead PPL
  ny_lslr <- fread("year1/NY/data/32-NewYork_lead.csv",
                   colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-v1) %>%
    mutate(project_type = "Lead",
           # projects on lead iup are DAC is listed, confirmed No if not
           disadvantaged = ifelse(dac == "DAC" & !is.na(dac), "Yes", "No"))
  
## Lead Funding List
  ny_lslr_awards <- fread("year1/NY/data/ny-lslr-awards.csv",
                          colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(funding_amount = clean_numeric_string(total_bil_lslr_funding_award),
           principal_forgiveness = clean_numeric_string(bil_lslr_grant_award),
           expecting_funding = "Yes") %>%
    select(project_number, funding_amount, principal_forgiveness, expecting_funding)
  
  # 109
  ny_lslr_combined <- ny_lslr %>%
    left_join(ny_lslr_awards, by="project_number") %>%
    mutate(population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost)) %>%
    select(project_number, county, system_name, borrower, description, project_cost, score, project_type,
           population, disadvantaged, funding_amount, principal_forgiveness, expecting_funding)
  
  
## EC PPL
  # (37,10) -> (37,11)
  ny_ec <- fread("year1/NY/data/32-NewYork_ec.csv",
                 colClass="character", na.strings="") %>%
    clean_names() %>%
    select(-pfas) %>%
    mutate(project_type = "Emerging Contaminants",
           # projects on lead iup are DAC is listed, confirmed No if not
           disadvantaged = ifelse(dac == "DAC" & !is.na(dac), "Yes", "No"))
  
## Emerging Contaminants - Funding List
  ny_ec_awards <- fread("year1/NY/data/ny-ec-awards.csv",
                        colClass="character", na.strings="") %>%
    clean_names() %>%
    rename(project_number = srf_number) %>%
    mutate(funding_amount = clean_numeric_string(bil_ec_grant_award),
           principal_forgiveness = clean_numeric_string(bil_ec_grant_award),
           expecting_funding = "Yes",
    ) %>%
    select(project_number, funding_amount, principal_forgiveness, expecting_funding)
  
  # 37
  ny_ec_combined <- ny_ec %>%
    left_join(ny_ec_awards, by="project_number") %>%
    mutate(population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost)) %>%
    select(project_number, county, system_name, borrower, description, population, project_cost, score,
           project_type, disadvantaged, funding_amount, principal_forgiveness, expecting_funding)
    
  
## BIL-GS, Lead, EC
  #NOTE: more than half of these do not appear on the annual list by project number
  ny_combined <- bind_rows(ny_bil_combined, ny_lslr_combined, ny_ec_combined)
  
  
## Annual List, p33 of FFY 2023 Final IUP
  # (558,10) -> (558,12)
  ny_annual <- fread("year1/NY/data/32-NewYork_base_ppl.csv",
                     colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost)) %>%
    select(-pop)
  
  # for projects on the annual list, simplify which columns are appended on
  ny_combined_on_annual <- ny_combined %>%
    filter(project_number %in% ny_annual$project_number) %>%
    select(project_number, project_type, disadvantaged, funding_amount, principal_forgiveness, expecting_funding)
  
  # for projects NOT on annual list, keep all columns for binding rows
  ny_combined_not_on_annual <- ny_combined %>%
    filter(!project_number %in% ny_annual$project_number)
  
  ny_annual_combined <- ny_annual %>%
    left_join(ny_combined_on_annual, by="project_number") %>%
    # 94 is the Hardship Evaluation Eligibility Line
    dplyr::mutate(
      hardship = dplyr::case_when(
        row_number() <= 94 ~ "Yes",
        row_number() > 94 ~ "No"
      )
    )|>
    # 148 is the funding line for annual list, all projects above funded, all below not 
    # unless funding determined by gs/lead/ec funding lists
    mutate(expecting_funding = case_when(
      row_number() <= 148 & is.na(expecting_funding) ~ "Yes",
      row_number() > 148 & is.na(expecting_funding) ~ "No",
      TRUE ~ expecting_funding),
      disadvantaged = case_when(
        !is.na(disadvantaged) ~ disadvantaged,
        # projects with H are DACs, projects above line but not H are No, projects below line and only on annual list are No Info
        score == "H" & is.na(disadvantaged) ~ "Yes",
        hardship == "Yes" & score != "H" & is.na(disadvantaged) ~ "No",
        .default = "No Information"),
      # projects that are on lead/gs/ec but not on annual list, plus those on annual list, all No Information
      funding_amount = replace_na(funding_amount, "No Information"),
      principal_forgiveness = replace_na(principal_forgiveness, "No Information"))
  
  # once the shared project_numbers are joined, bind the remaining rows from GS/EC/Lead with all included columns
  ny_annual_combined <- bind_rows(ny_annual_combined, ny_combined_not_on_annual)
  
  ## Multi-year list - contains most, but not all of annual list, so repeat multi-step join process
  ny_multi <- fread("year1/NY/data/ny-multi-year-list.csv",
                    colClass="character", na.strings="") %>%
    clean_names() %>%
    mutate(project_number = str_squish(project),
           population = clean_numeric_string(pop),
           project_cost = clean_numeric_string(project_cost)) %>%
    select(-project, -pop)
  
  # for projects on the multi-year list, keep the columns determined by other docs and append it
  ny_annual_on_multi <- ny_annual_combined %>%
    filter(project_number %in% ny_multi$project_number) %>%
    select(project_number, project_type, disadvantaged, funding_amount, principal_forgiveness, expecting_funding)
  
  # append projects that are on annual and multi-year list
  ny_multi_annual_combined <- ny_multi %>%
    left_join(ny_annual_on_multi, by="project_number")
  
  # for projects no on the multi-year list, keep all columns  
  ny_annual_not_on_multi <- ny_annual_combined %>%
    filter(!project_number %in% ny_multi$project_number)
  
  # append projects not on multi-year list
  ny_multi_annual_combined <- bind_rows(ny_multi_annual_combined, ny_annual_not_on_multi)
    

  #final amendment no. 5: https://drive.google.com/file/d/1fsyBeEhaVFn9upghfnPngwJYs8Z76yGI/view
  ny_multi_annual_combined_amended <- ny_multi_annual_combined |>
    dplyr::mutate(
      project_cost = dplyr::case_when(
        project_number == "18907" ~ "5950000",
        # project_number = "18976" ~ "5,036,753", #already in list
        project_number == "18995" ~ "36000000",
        project_number == "19126" ~ "13500000",
        .default = project_cost)
    )


  ny_clean <- ny_multi_annual_combined_amended %>%  
    mutate(system_name_borrower = ifelse(is.na(system_name_borrower), paste0(system_name, " / ", borrower), system_name_borrower),
           borrower = str_squish(system_name_borrower),
           community_served = str_squish(county),
           project_id = str_squish(project_number),
           project_description = str_squish(description),
           project_score = dplyr::case_when(
            str_squish(score) == "H" ~ "No Information",
            .default = str_squish(score)
           ),
           project_type = case_when(
             !is.na(project_type) & project_type !="General" ~ project_type,
             grepl(lead_str, project_description, ignore.case=TRUE) | code=="BLSLR" ~ "Lead",
             grepl(ec_str, project_description, ignore.case=TRUE) | code=="BEC" ~ "Emerging Contaminants",
             TRUE ~ "General"),
           # all remaining projects on multi-year list are no info for DAC
           disadvantaged = replace_na(disadvantaged, "No Information"),
           # all remaining projects on multi-year are not expecting funding
           expecting_funding = replace_na(expecting_funding, "No"),
           funding_amount = replace_na(funding_amount,"No Information"),
           principal_forgiveness = replace_na(principal_forgiveness, "No Information"),
           state = "New York",
           state_fiscal_year = "2023",
           pwsid = as.character(NA),
           project_name = as.character(NA),
           requested_amount = as.character(NA),
           project_rank = as.character(NA),
           )  %>%
    select(community_served, borrower, pwsid, project_id, project_name, project_type, project_cost,
           requested_amount, funding_amount, principal_forgiveness, population, project_description,
           disadvantaged, project_rank, project_score, expecting_funding, state, state_fiscal_year)
  
  ny_clean <- ny_clean %>%
    filter(project_cost != "0")
   
  ####### SANITY CHECKS START #######
  
  # Hone in on project id duplication
  
  ny_clean |> dplyr::group_by(project_id) |> dplyr::summarise(counts = n()) |> dplyr::arrange(dplyr::desc(counts))
  ####### Decision: No duplicates
  
  # Check for disinfection byproduct in description
  ny_clean |> dplyr::filter(grepl("disinfection byproduct", tolower(project_description)))
  ####### Decision: disinfection byproducts is an EC project, 
  # but it's also a consolidation project, which is a general project type. 
  # I think we should go ahead and classify as EC however.
  # No change, classified as expected
    
  ####### SANITY CHECKS END #######

  run_tests(ny_clean)
  rm(list=setdiff(ls(), "ny_clean"))
  
  return(ny_clean)
}
