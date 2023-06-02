library(tidyverse)
arfund <- read.csv("4-Arkansas_Fundable.csv")
ardraft <- read.csv("4-Arkansas.csv")
arfund <-
  dplyr::rename(arfund, No. = IUP.No.)
merge <- 
  inner_join(arfund,ardraft, by = "No.")

arfund$No. <- as.character(arfund$No.)


write.csv(merge, "4-Arkansas_Merged.csv")
