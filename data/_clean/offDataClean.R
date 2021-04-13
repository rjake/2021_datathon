# script to clean offense data
#setwd("/Users/patelk26/Desktop/projects/Datathon2021/offenses_disposition")

library(tidyverse)
library(lubridate)

# reading data -----
sh <- read.csv('statute_hierarchy.csv')
df <- read.csv('offenses_dispositions_v3.csv')

# remove duplicate rows -------
df <- df[!duplicated(df),]

# convert "" to NAs -------
df <- df %>% 
  mutate_if(is.character, list(~na_if(.,""))) 

# remove rows with disposition == NA -------
df <- df[!is.na(df$disposition),]
# 450532/1475882 rows left
# 190580 unique docket_ids with dispositions

# fix period columns -------
#' @example
#' extract_days(x = "2 weeks and 1 day")
extract_days <- function(x) {
  tolower(x) %>% 
    str_remove("and ") %>% 
    duration() %>% 
    seconds_to_period() %>% 
    day()
}

df$min_period_clean_days <- extract_days(df$min_period)
df$max_period_clean_days <- extract_days(df$max_period)
df$period_clean_days <- ifelse(is.na(df$min_period_clean_days), df$max_period_clean_days,
                               ifelse(is.na(df$max_period_clean_days), df$min_period_clean_days, paste0(df$min_period_clean_days,'-',df$max_period_clean_days)))


# number of offenses per docket_id -------
df %>%
  group_by(docket_id) %>%
  tally()

# cleaning statue names -----
df$statute_name2 <- df$statute_name %>%
  # head(100) %>%
  str_remove_all("§§.*") %>% 
  str_replace_all("[^[A-Z0-9\\-\\.]]+", "_") %>% 
  str_remove("_$") %>% 
  print()

# subsetting columns -----
# ___merging disposing authority name ------
df$disposing_authority_fullname <- paste0(df$disposing_authority__first_name,' ',df$disposing_authority__middle_name,' ', df$disposing_authority__last_name)

# ___subset -----
df <- df[ , -which(names(df) %in% c("description","statute_description","statute_name","min_period","max_period","period", "disposing_authority__first_name","disposing_authority__middle_name",
                                                   "disposing_authority__last_name"))]


# ___reorder columns ------
df <- df[,c("docket_id","statute_name2", "sequence_number","grade","disposition","disposing_authority__title","disposing_authority_fullname","disposing_authority__document_name",
                      "disposition_method","sentence_type","min_period_clean_days","max_period_clean_days","period_clean_days")]

# merging rows for multiple records for same docket_id -------
df.clean <- df %>%
  group_by(docket_id) %>%
  summarise_all(funs(trimws(paste(., collapse = ',')))) %>%
  print()

# writing data out ------
write.table(df.clean, file = 'offenses_dispositions_v3_clean.csv', row.names = F, col.names = T, sep = '\t', quote = F)



