### Inputs ----
# data formating
library(tidyverse)
library(data.table)
library(janitor)

# import/export
library(googledrive)
library(googlesheets4)
library(aws.s3)

# data viz
library(scales)
library(ggpubr)
library(plotly)

### Options ----
options(scipen=999)


### AWS Access ---- 

# because this is used across so many notebooks and files, keep it in the most general resources
run_code_from_file <- function(file_path) {
  # Read the content of the file
  code <- readLines(file_path)
  
  # Join lines into a single string
  code <- paste(code, collapse = "\n")
  
  # Evaluate the code
  eval(parse(text = code))
}