# Notes: -----------------------------------------------------------------------
#
# Orig questions for JAT https://bit.ly/3bQVAI2
#
# Laws:
#   Philly City Ordinances:
#     https://codelibrary.amlegal.com/codes/philadelphia/latest/philadelphia_pa/0-0-0-184124
#
#   State
#     https://www.legis.state.pa.us/cfdocs/legis/LI/consCheck.cfm?txtType=HTM&ttl=18
#     https://www.legis.state.pa.us/cfdocs/legis/LI/consCheck.cfm?txtType=HTM&ttl=18&div=0&chpt=21
#                                                                            ^^^^^^^^^^^^^^^^^^^^
#     Also: https://www.legis.state.pa.us/wu01/li/li/ct/htm/18/18.htm
#
# Open Questions:
#   [ ] Does JAT have a hypothesis about Krasner's impact?
#   [ ] should we limit the charges to dockets with only non-violent offenses?
#   [ ] which date to use? arrest date or filing date (or complaint or initiation)?
#   [ ] can we get groupers for the statute_hierarchy? There are ~900 statute_hierarchy descriptions and ~1900 descriptions
#       do we need to scrape the web?
#
#   [ ] weird non-crimes (prize of $2,000)
#        filter(raw_offenses, docket_id %in% c(351109, 369057)) %>% print(n = Inf)
#
#   [ ] what metrics do we want? Can we roll it up to the docket?
#      * [ ] # distinct charges
#      * [ ] # dismissed charges
#      * [ ] total days sentenced (need to determine consecutive vs concurrent)
#      * [ ] #
#      * [ ] #
#      * [ ] #
#      * [ ] #



# workspace ----
library(tidyverse)
library(lubridate)

# data can be downloaded here: source("reports/data/raw/save_data.R")
raw_offenses <- readRDS("reports/data/raw/offenses_dispositions.Rds")
raw_dockets <- readRDS("reports/data/raw/defendant_docket_details.Rds")
raw_bail <- readRDS("reports/data/raw/bail.Rds")
statute_hierarchy <- read_csv("../../data/statute_hierarchy.csv")

# extract # of day from sentencing field
#' @example
#' extract_days(x = "2 weeks and 1 day")
extract_days <- function(x) {
  tolower(x) %>%
    str_remove("and ") %>%
    duration() %>%
    seconds_to_period() %>%
    day()
}


# prep_bail ----
prep_bail <-
  raw_bail %>%
  group_by(docket_id) %>%
  summarise(min_bail = min(total_amount, na.rm = TRUE)) %>%
  ungroup() %>%
  print()

# prep_dockets ----
prep_dockets <-
  raw_dockets %>%
  filter(
    !str_detect(court_office_types, "Supreme|Superior|Commonwealth"),
    between(year(filing_date), 2016, 2019)
  ) %>% # 16-17  vs  18-19
  rename_all(
    ~ .x %>%
      str_replace_all("_+", "_") %>%
      str_remove("current_processing_status_") %>%
      str_replace("court_office_(court)", "\\1")
  ) %>%
  select(
    -c(
      complaint_date, disposition_date, arrest_date, status_change_datetime,
      court_types, processing_status
    ),
    -starts_with("municipality")
  ) %>%
  mutate(
    # update
    court_office_types = # make alphabetical
      str_replace(court_office_types, "^(Municipal), (Criminal)$", "\\2, \\1"),
    judicial_districts = # group into top 5 most common district types
      fct_lump(judicial_districts, n = 5),
    court_display_name = # shorten name
      str_remove_all(court_display_name, "( - )?Philadelphia County( )?"),
    race =
      ifelse(race == "Asian", "Asian/Pacific Islander", race) %>%
      replace_na("Unknown/Unreported"),
    # new fields
    age = interval(date_of_birth, filing_date) / dyears(),
    filing_year = year(filing_date),
    timeframe = ifelse(filing_year < 2018, "pre-Krasner", "post-Krasner"),
    months_from_initiation =
      (interval(initiation_date, filing_date) / dmonths(1)) %>%
      floor(),
    distance_from_initiation =
      case_when(
        months_from_initiation < 1 ~ "< 1 month",
        months_from_initiation < 2 ~ "1 - 2 months",
        TRUE ~ "more than 2 months"
      )
  ) %>%
  select(-date_of_birth) %>%
  # count(district_group, sort = TRUE) %>%
  print()


# prep_offenses ----
prep_offenses <-
  raw_offenses %>%
  # slice(1:10000) %>%
  transmute(
    docket_id,
    statute =
      statute_name %>%
      str_remove_all(" §§.*") %>%
      str_replace_all("[^[A-Z0-9\\-\\.]]+", "_") %>%
      str_remove("_$"),
    title_id =
      statute %>%
      str_extract("^\\d+") %>%
      str_pad(2, "left", "0"),
    # description,
    # sequence_number,
    grade,
    judge_name = paste(
      disposing_authority__first_name,
      na.omit(disposing_authority__middle_name),
      disposing_authority__last_name
    ),
    disposition,
    min_period,
    sentence_type,
    min_sentencing = extract_days(min_period)
  ) %>%
  print()

# combine ----
docket_charges <-
  prep_offenses %>%
  inner_join(prep_dockets) %>%
  inner_join(prep_bail) %>%
  left_join(distinct(statute_hierarchy, title_id, title_text)) %>%
  relocate(title_text, .after = title_id) %>%
  print()

saveRDS(docket_charges, "reports/data/clean/docket_charges.Rds")