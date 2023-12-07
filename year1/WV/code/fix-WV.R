library(tidyverse)
wvbase <- read.csv("48-West_Virginia_PPL1.csv")
details <- read.csv("48-West_Virginia_PPL2.csv")
wvbase$PROJECT.DESCRIPTION <- trimws(gsub("\\s+", " ", wvbase$PROJECT.DESCRIPTION))
details$PROJECT.DESCRIPTION <- trimws(gsub("\\s+", " ", details$PROJECT.DESCRIPTION))
wvbase$SYSTEM <- trimws(gsub("\\s+", " ", wvbase$SYSTEM))
details$SYSTEM <- trimws(gsub("\\s+", " ", details$SYSTEM))

merge <- full_join(wvbase, details, by = c("SYSTEM", "RANKING"))
wvbase$PROJECT.DESCRIPTION
details$RANKING <- as.numeric(details$RANKING)
View(merge)
write_csv(merge, "WVMergedFull.csv")
leadfilter <- merge %>%
  filter()


wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.BASE.GRANT <- parse_number(wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.BASE.GRANT)

wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.EMERGING.CONT..GRANT <- parse_number(
  wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.EMERGING.CONT..GRANT)

wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.LSLR.GRANT <- parse_number(
  wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.LSLR.GRANT)

wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.SUPPLEMENTAL.GRANT <- parse_number(
  wv$wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.SUPPLEMENTAL.GRANT)

wv <- wv %>%
  mutate_at(c('wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.BASE.GRANT',
              'wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.EMERGING.CONT..GRANT' ,
              'wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.LSLR.GRANT' ,
              'wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.SUPPLEMENTAL.GRANT'),
            as.numeric)

wv <- wv %>%
  mutate(wv_principal_forgiveness = purrr::pmap_dbl(list(
    wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.BASE.GRANT, 
    wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.EMERGING.CONT..GRANT,
    wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.LSLR.GRANT,
    wv_DWTRF.PRINCIPAL.FORGIVENESS.FROM.SUPPLEMENTAL.GRANT),
    sum, 
    na.rm = TRUE)) 
