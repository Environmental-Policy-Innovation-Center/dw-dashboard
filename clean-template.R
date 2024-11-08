source("resources.R")


clean_df <- function() {
  
  
  
  run_tests(df_clean)
  rm(list=setdiff(ls(), "df_clean"))
  
  return(df_clean)
}