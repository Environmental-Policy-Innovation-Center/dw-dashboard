source("resources/review_helpers.R")
source("resources/view_code_review.R")

year <- "year4"
sfy <- "SFY27"
state_abb <- "MD"
review_number <- "1"

year_title <- stringr::str_extract(year, "\\d+")

dict_path <- paste0(year,"/", state_abb, "/data/", state_abb, "_Y", year_title, "_",sfy , "_DD.docx")

data_paths<- c(
  paste0(year, "/", state_abb, "/data/", "MD_Y4_SFY27_Comprehensive_List.csv"), 
  paste0(year, "/", state_abb, "/data/", "MD_Y4_SFY27_Fundable_List.csv")
)

script_path <- paste0(year,"/", state_abb, "/code/clean-", state_abb, ".R")
log_path <- paste0(year,"/", state_abb, "/data")

run_review(
  dict_path = dict_path,
  data_paths = data_paths,
  script_path = script_path,
  log_dir = log_path
)

findings_path <- paste0(log_path, "/review_findings.csv")

my_data <- read.csv(findings_path, stringsAsFactors = FALSE)

code_review <- my_data |>
  dplyr::filter(call_label=="3_script_logic")

review_path <- paste0(log_path, "/code_review.html")
view_code_review(code_review, out = review_path, open= FALSE, title = paste0(state_abb, " Y", year_title, " Script Review #", review_number))
