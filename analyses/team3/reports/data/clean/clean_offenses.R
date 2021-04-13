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
#   [ ] can we get groupers for the statutes? There are ~900 statutes descriptions and ~1900 descriptions
#       do we need to scrape the web? 
#
#   [ ] weird non-crimes (prize of $2,000) 
        filter(raw_offenses, docket_id %in% c(351109, 369057)) %>% print(n = Inf)
#
#   [ ] why aren't these matching??
        table(statutes$statute_description %in% unique(raw_offenses$statute_description))
        table(unique(raw_offenses$statute_description) %in% statutes$statute_description)
        
        raw_offenses$statute_description %>% 
          subset(!. %in% statutes$statute_description) %>% 
          unique()

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

# raw_offenses <- 
#   read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions.csv")
# 
# raw_dockets <- 
#   read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_details.csv")
# 
# saveRDS(raw_offenses, "reports/data/raw/offenses_dispositions.Rds")
# saveRDS(raw_dockets, "reports/data/raw/defendant_docket_details.Rds")

raw_offenses <- readRDS("reports/data/raw/offenses_dispositions.Rds")
raw_dockets <- readRDS("reports/data/raw/defendant_docket_details.Rds")
statutes <- read_csv("../../data/statute_hierarchy.csv")


raw_offenses %>% 
  drop_na(sentence_type) %>% 
  filter(!str_detect(sentence_type, "No Further")) %>% 
  inner_join(filter(statutes, title != 18)) %>% 
  #filter(title == 75) %>% 
  count(jurisdiction, title, title_text, chapter, chapter_text, sort = TRUE) %>% 
  print(n = Inf)



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





raw_offenses %>% 
  distinct(description, statute_description) %>% 
  filter(description != statute_description)



prep_offenses <-
  raw_offenses %>%
  # slice(1:10000) %>% 
  transmute(
    docket_id,
    statute = statute_name,
    description,
    #sequence_number,
    grade,
    judge_name = paste(
      # disposing_authority__title,
      disposing_authority__first_name, 
      # disposing_authority__middle_name,
      disposing_authority__last_name
    ),
    disposition,
    min_period,
    sentence_type,
    credit
  ) %>%
  mutate(
    statute = 
      statute %>% 
      str_remove_all(" §§.*") %>% 
      str_replace_all("[^[A-Z0-9\\-\\.]]+", "_") %>% 
      str_remove("_$"),
    min_days = extract_days(min_period)
  ) %>% 
  # filter(coalesce(sentence_type, "") != "No Further Penalty") %>% 
  print()


raw_offenses %>% 
  filter(
    str_detect(statute_name, "62.*14"),
    str_detect(description, "Brib")
    ) %>%
  count(statute) %>% 
  left_join(statutes)

  
  left_join(raw_dockets %>% select(docket_id, filing_date)) %>% 
  group_by(statute_description, description) %>%
  summarise(
    n = n(),
    min = min(filing_date),
    max = max(filing_date)
  ) %>% 
  ungroup()


  
raw_offenses %>% 
  filter(docket_id == 420) %>% view()
  distinct(docket_id, disposing_authority__document_name) %>%
  drop_na() %>% 
  count(docket_id, sort = TRUE)


offenses %>% 
  slice(1:1000) %>% 
  filter()
drop_na() %>% 
  count(docket_id)

raw_offenses %>% 
  filter(docket_id == 117190) %>% 
  arrange(statute_description, period) %>% 
  select(description, statute_description, sequence_number, period) %>% 
  drop_na() %>% 
  print(n = Inf)


