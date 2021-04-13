library(tidyverse)

url <- "https://storage.googleapis.com/jat-rladies-2021-datathon/"

raw_offenses <-
  read_csv(paste0(url, "offenses_dispositions.csv"))

raw_dockets <-
  read_csv(paste0(url, "defendant_docket_details.csv"))

saveRDS(raw_offenses, "reports/data/raw/offenses_dispositions.Rds")
saveRDS(raw_dockets, "reports/data/raw/defendant_docket_details.Rds")
