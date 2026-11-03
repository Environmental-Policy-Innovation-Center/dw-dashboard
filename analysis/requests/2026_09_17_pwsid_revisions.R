pwsid_revisions <- tibble::tribble(
  ~pwsid,                        ~revised_pwsid,  ~borrower,                     ~notes,
  # --- Illinois ---
  "IL970200",                    "IL0970200",     NA_character_,                 NA_character_,
  "IL50050",                     "IL1150050",     "Argenta",                     NA_character_,
  "IL50020",                     "IL0050020",     NA_character_,                 NA_character_,
  "IL10100",                     "IL0110100",     NA_character_,                 NA_character_,
  "IL850100",                    "IL0850100",     NA_character_,                 NA_character_,
  "IL434140",                    "IL0434140",     NA_character_,                 NA_character_,
  "IL110250",                    "IL0110250",     NA_character_,                 NA_character_,
  "IL971250",                    "IL0971250",     NA_character_,                 NA_character_,
  "IL03010510",                  "IL0310510",     NA_character_,                 NA_character_,
  "IL00895550",                  "IL0895550",     NA_character_,                 NA_character_,
  "IL850400",                    "IL0850400",     NA_character_,                 NA_character_,
  "IL810300",                    "IL0810300",     NA_character_,                 NA_character_,
  "IL034850",                    "IL0374850",     NA_character_,                 NA_character_,
  "IL031330",                    "IL0310330",     NA_character_,                 NA_character_,
  "IL210150",                    "IL0210150",     NA_character_,                 NA_character_,
  "IL210050",                    "IL0210050",     NA_character_,                 NA_character_,
  "IL630600",                    "IL0630600",     NA_character_,                 NA_character_,
  "IL111100",                    "IL0111100",     NA_character_,                 NA_character_,
  "IL150330",                    "IL0150300",     NA_character_,                 NA_character_,
  "IL20105000",                  "IL2010500",     NA_character_,                 NA_character_,
  "IL210550",                    "IL0210550",     NA_character_,                 NA_character_,
  "IL310540",                    "IL0310540",     NA_character_,                 NA_character_,
  "IL312850",                    "IL0312850",     NA_character_,                 NA_character_,
  "IL03013000",                  "IL0313000",     NA_character_,                 NA_character_,
  "IL07500200",                  "IL0750200",     NA_character_,                 NA_character_,
  "IL08550050",                  "IL0850050",     NA_character_,                 "remaining IL (2) unresolved",

  # --- Ohio ---
  "OH700612",                    "OH0700612",     "Bridgeport",                  NA_character_,
  "OH500603",                    "OH0500603",     "Coolville",                   NA_character_,
  "OH-4300411",                  "OH4300411",     NA_character_,                 NA_character_,
  "OH36000614",                  "OH3600614",     NA_character_,                 NA_character_,
  "OH903012",                    "OH0903012",     NA_character_,                 NA_character_,
  "OH701212",                    "OH5701212",     NA_character_,                 NA_character_,
  "OH79001711",                  "OH7901711",     NA_character_,                 NA_character_,
  "OH450612",                    "OH4500612",     NA_character_,                 NA_character_,
  "OH34001121",                  "OH3400112",     NA_character_,                 NA_character_,
  "OH760011",                    "OH7600011",     NA_character_,                 NA_character_,
  "OH701012",                    "OH5701012",     NA_character_,                 NA_character_,
  "OH64022703",                  "OH6402703",     NA_character_,                 NA_character_,
  "OH501111",                    "OH0501111",     NA_character_,                 NA_character_,
  "OH403203",                    "OH8403203",     NA_character_,                 NA_character_,
  "OH402703",                    "OH6402703",     NA_character_,                 NA_character_,
  "OH201312",                    "OH0201312",     NA_character_,                 "remaining OH (2, 1 only 'OH') unresolved",

  # --- Texas ---
  "TX011013",                    "TX0110013",     NA_character_,                 NA_character_,
  "TX00150018",                  "TX0150018",     "San Antonio Water System",    "remaining TX (2: 1 composite, 1 pending) unresolved",

  # --- Maryland ---
  "MD300002",                    "MD0300002",     NA_character_,                 NA_character_,
  "MD210010",                    "MD0210010",     NA_character_,                 NA_character_,
  "MD023006",                    "MD0230006",     NA_character_,                 NA_character_,
  "MD020003",                    "MD0200003",     NA_character_,                 "remaining MD (1) unresolved",

  # --- Louisiana ---
  "LA106007",                    "LA1069007",     NA_character_,                 NA_character_,

  # --- Indiana ---
  "IN528004",                    "IN5280004",     NA_character_,                 NA_character_,
  "IN36003/52360",               "IN5236003",     NA_character_,                 "composite ID",
  "IN5236003/, IN5236008",       "IN5236003",     NA_character_,                 "composite ID",
  "IN5245047, IN, IN5245067",    "IN5245047",     NA_character_,                 "composite ID",
  "IN524015",                    "IN5245015",     NA_character_,                 NA_character_,
  "IN5236003, IN5263008",        "IN5236003",     NA_character_,                 "composite ID",

  # --- Tennessee ---
  "TN0000531/ TN0000520",        "TN0000531",     NA_character_,                 "composite, but LS; no project id"
)


all_projects <- s3read_using(read.csv, object="s3://water-team-data/clean_data/srf_project_priority_lists/dwsrf-funding-tracker-all-projects.csv") %>%
  clean_names() |>
  dplyr::mutate(state_fiscal_year = as.character(state_fiscal_year))

pwsid_revisions_w_borrower <-  pwsid_revisions |>
  dplyr::filter(!is.na(borrower))

pwsid_revisions_plain <-  pwsid_revisions |>
  dplyr::filter(is.na(borrower)) |>
  dplyr::select(-borrower) 

all_projects |>
  dplyr::inner_join(pwsid_revisions_w_borrower, by = c("pwsid", "borrower")) |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally()


all_projects |>
  dplyr::inner_join(pwsid_revisions_plain, by = "pwsid") |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally()

all_projects |>
  dplyr::inner_join(pwsid_revisions_w_borrower, by = c("pwsid", "borrower")) |> 
  dplyr::bind_rows(
    all_projects |>
      dplyr::inner_join(pwsid_revisions_plain, by = "pwsid") 
  ) |>
  dplyr::group_by(state, state_fiscal_year) |>
  dplyr::tally() |>
  print(n=100)


all_projects |>
  dplyr::inner_join(pwsid_revisions_w_borrower, by = c("pwsid", "borrower")) |> 
  dplyr::bind_rows(
    all_projects |>
      dplyr::inner_join(pwsid_revisions_plain, by = "pwsid") 
  ) |>
  dplyr::filter(grepl("TX", pwsid)) |>
  dplyr::select(state, state_fiscal_year, borrower, pwsid, revised_pwsid) |>
  dplyr::select(state_fiscal_year, borrower, pwsid, revised_pwsid) |>
  dplyr::distinct() |>
  dplyr::arrange(state_fiscal_year)

