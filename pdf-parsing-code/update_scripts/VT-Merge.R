vtgeneral <- read_csv("45-Vermont_PPL.csv") %>%
  clean_names()
vtlead <-read_csv("45-Vermont_Lead.csv") %>%
  clean_names()
vtec <- read_csv("45-Vermont_Other-Cont.csv") %>%
  clean_names()
vtlead$project <- "Lead Service Line"

vtlead <- vtlead[-c(4,5),]

vtlead$wsid <- as.numeric(vtlead$wsid)

vtlead <-
  dplyr::rename(vtlead, score = application_score)
vtlead <- 
  dplyr::rename(vtlead, x2022_loan_amount = line_loan_amount)
firstmerge <- full_join(vtgeneral, vtlead) 
vtec <- dplyr::rename(vtec, water_system_borrower = water_system)
vtec <- dplyr::rename(vtec, score = plist_app_score)
vtec <- dplyr::rename(vtec, x2022_loan_amount = line_items)
secondmerge <-full_join(firstmerge, vtec)
secondmerge <- secondmerge[!is.na(secondmerge$wsid),]
vtec

write.csv(secondmerge, "45-Vermont_PPL_Merged.csv", row.names = FALSE)
unlink(c("45-Vermont_PPL.csv", "45-Vermont_Lead.csv","45-Vermont_Other-Cont.csv"))
