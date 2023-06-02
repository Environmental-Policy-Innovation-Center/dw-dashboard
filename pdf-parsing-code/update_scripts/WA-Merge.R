wa_approved <- read_csv("46-Washington_PPL.csv") %>%
  clean_names()
wa_approved <-
  dplyr::rename(wa_approved, health_application = health_applicati_on)
wa_approved <-
  dplyr::rename(wa_approved, population = populatio_n)
wa_appendix <- read_csv("46-Washington_Appendix_Approved_PPL.csv") %>%
  clean_names()
wa_appendix <- 
  dplyr::rename(wa_appendix, water_system_id = water_syste_m_id)
wa_merge <- full_join(wa_appendix, wa_approved)
write.csv(wa_merge, "46-Washington_PPL_Merged.csv", row.names = FALSE)
unlink(c("46-Washington_PPL.csv", "46-Washington_Appendix_Approved_PPL.csv"))
