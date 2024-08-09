library(tidyverse)
library(data.table)
library(janitor)
source("cleaning-functions.R")


clean_df <- function() {
  
  
  
  run_tests(df_clean)
  rm(list=setdiff(ls(), "df_clean"))
  
  return(df_clean)
}