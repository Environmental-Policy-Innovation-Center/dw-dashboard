# Ask:
# Compare SFY25 (Year 4) projects found in NYs Fundable List: https://drive.google.com/file/d/1rMb1wI7xdxifjmFALoMzE_BQrlmAV91I/view 
# to those in the Annual IUP List https://drive.google.com/file/d/1kLHUOAem2Sg5YjYMTRPJ0hcE5onjWG9Y/view

# Annual list
base_path <- file.path("year4", "NY", "data")  
ny_annual <- data.table::fread(file.path(base_path, "Annual.csv"),
                     colClass="character", na.strings="") |>
      janitor::clean_names()  |>
      dplyr::mutate(
      cumulative_total = as.numeric(clean_numeric_string(cumulative_total)),
      hardship = dplyr::case_when(
        cumulative_total <= 718076036 ~ "Yes", #cummulative total amount in annual list for which hardship eligibility was evaluated
        .default = "No"
      ),
      expecting_funding = dplyr::case_when(
        cumulative_total <= 1885161283 ~ "Yes", #expanded subsidized interest rate funding line
        # if one of the BEC/BLSLR/BGS codes are present, expecting_funding
        !is.na(codes) & !(codes %in% c("FMP", "EQ")) ~ "Yes",
        .default = "No"
      ),
      description = stringr::str_squish(description)
    )

nrow(ny_annual) #782
ny_annual |> dplyr::filter(expecting_funding == "Yes") |> nrow() #329
ny_annual |> dplyr::filter(expecting_funding == "Yes" & project_cost == "$0") |> nrow() #98 (not in the clean dataset)

# Scraped fundable list
fundable_list <- tibble::tribble(
  ~project_id, ~list,    
  19939L,     "GenSupp",
  19527L,     "GenSupp",
  19902L,     "GenSupp",
  19780L,     "GenSupp",
  19954L,     "GenSupp",
  19940L,     "GenSupp",
  19824L,     "GenSupp",
  18877L,     "GenSupp",
  19788L,     "GenSupp",
  18430L,     "GenSupp",
  18915L,     "GenSupp",
  19976L,     "GenSupp",
  19937L,     "GenSupp",
  18717L,     "GenSupp",
  19571L,     "GenSupp",
  19951L,     "GenSupp",
  19945L,     "GenSupp",
  19539L,     "GenSupp",
  19972L,     "GenSupp",
  18128L,     "GenSupp",
  19541L,     "GenSupp",
  19933L,     "GenSupp",
  19964L,     "EC",     
  19101L,     "EC",     
  19942L,     "EC",     
  19962L,     "EC",     
  19938L,     "EC",     
  19653L,     "EC",     
  19907L,     "EC",     
  19997L,     "EC",     
  19727L,     "EC",     
  19972L,     "EC",     
  19992L,     "LSLR",   
  19619L,     "LSLR",   
  19862L,     "LSLR",   
  19861L,     "LSLR",   
  19409L,     "LSLR",   
  19292L,     "LSLR",   
  19617L,     "LSLR",   
  19903L,     "LSLR",   
  19631L,     "LSLR",   
  19916L,     "LSLR",   
  19915L,     "LSLR",   
  19906L,     "LSLR",   
  19909L,     "LSLR",   
  19597L,     "LSLR",   
  19924L,     "LSLR",   
  19990L,     "LSLR",   
  19991L,     "LSLR",   
  19605L,     "LSLR",   
  19899L,     "LSLR",   
  19892L,     "LSLR",   
  19893L,     "LSLR",   
  19894L,     "LSLR",   
  19930L,     "LSLR",   
  19931L,     "LSLR",   
  19620L,     "LSLR",   
  19622L,     "LSLR",   
  19628L,     "LSLR",   
  19823L,     "LSLR",   
  19464L,     "LSLR",   
  19917L,     "LSLR",   
  19919L,     "LSLR",   
  19895L,     "LSLR",   
  19911L,     "LSLR",   
  19472L,     "LSLR",   
  19410L,     "LSLR",   
  19639L,     "LSLR",   
  19994L,     "LSLR",   
  19993L,     "LSLR",   
  19607L,     "LSLR",   
  19366L,     "LSLR"
) |>
  dplyr::mutate(project_id = as.character(project_id))


nrow(fundable_list) #72

fundable_list[!fundable_list$project_id %in% ny_annual$project_number,]

#There is one project (project_id 19962) not found in the annual list

ny_annual_fundable <- ny_annual |>
  dplyr::inner_join(fundable_list, by= c("project_number" = "project_id")) 

nrow(ny_annual_fundable) #71

ny_annual_fundable |> 
  dplyr::mutate(zero_project_cost = ifelse(project_cost == "$0", "Yes", "No")) |>
  group_by(expecting_funding, zero_project_cost) |>
  dplyr::tally()

project_cost_amendment_id <- c("19554", "17629", "18768",  "18328",  "18127", "19036", "15616", "18764", "18715", "18423", "19537","19203", "18458", "19051")

fundable_list |>
  dplyr::filter(project_id %in% project_cost_amendment_id)





# all_projects |>
#   dplyr::filter(
#     project_id %in% fundable_list$project_id
#   ) |>
#   dplyr::filter(state_fiscal_year == "2025")

# ny_clean |>
#   dplyr::filter(
#        project_id %in% fundable_list$project_id
#   )
