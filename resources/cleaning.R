library(tidyverse)
library(janitor)


### Cleaning Functions ----


# takes a numeric column, removes non-numeric characters, then returns as a string column
clean_numeric_string <- function(column) {
  
  column_numeric <- convert_to_numeric(column)
  
  column_string <- as.character(column_numeric, FALSE)
  column_string <- replace_na(column_string, "No Information")
  
  return(column_string)
}

# takes a numeric column, removes non-numeric characters, then returns a numeric column
# can also automatically replace NAs with 0 for being able to add columns together
# but note that those columns will likely then need to be processed as strings separately
convert_to_numeric <- function(column, fill_na_0 = FALSE) {
  
  column_numeric <- as.numeric(str_replace_all(column, "[^0-9.-]", ""))
  
  if (fill_na_0 == TRUE) {
    column_numeric <- replace_na(column_numeric, 0)
  }
  
  return(column_numeric)
}


# takes a column and calculates the median without including 0s
# since they can often be equivalent to 'null' responses for our purposes
median_wo_zero <- function(col_name) {
  non_zero <- col_name[col_name != 0]  # Filter out 0s
  median(non_zero)  # Calculate median on the filtered data
}

### Project Type Lists

# create keywords to search for identifying inventorying and replacement efforts
lsli_str <- paste(c(
  "lsli", 
  "lead service line inventory", 
  "inventory", 
  "survey"
), collapse = "|")

lslr_str <- paste(c(
  "lslr",
  "lead service line replacement",
  "service repl",
  "replac\\w*\\s+(?:\\w+\\s+){0,5}?lead"
), 
collapse = "|")


# create keywords for determining project types when extracting info from project descriptions
lead_str <- paste(c(
  lsli_str, 
  lslr_str, 
  "lead",
  "lsl"), collapse = "|")

ec_str <- paste(c(
  "cyanotoxin", "dioxane", "emerging contaminant", "lithium", "manganese", "Mn",
  "Perfluoro-n-pentanoic acid", "PFPeA", "PFAS", "PFOA", "PFOS", "trihalomethane", "THM",
  "Unregulated Contaminant Monitoring Rule", "DBP", "disinfection byproduct", "HAA5",
  "haloacetic acid", "emerging containment", "BIL EC", "BIL-EC"
), collapse = "|")



