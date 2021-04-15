#### load packages ####

library(tidyverse)

#### load data ####
off_16_17 <- read_csv("../../data/offenses_dispositions_v2_2016_2017.csv")
off_18_19 <- read_csv("../../data/offenses_dispositions_v2_2018_2019.csv")
table(off_18_19$grade)



#### Q: Changes in disposition, min/max period, sentence type? ####


standarize_min_period <- function(dataset) {
  
  #dataset <- off_18_19
  dataset.filt.split <- dataset %>%
    drop_na(min_period) %>%
    separate(min_period,c("min_period_num","min_period_unit"),sep = " ")
  dataset.filt.split$min_period_num <- as.numeric(dataset.filt.split$min_period_num)
  
  #table(dataset.filt.split$min_period_unit)
  
  dataset.filt.split$min_period_in_month <- ifelse(
    dataset.filt.split$min_period_unit %in% c("Month","Months"), dataset.filt.split$min_period_num,
    ifelse(dataset.filt.split$min_period_unit %in% c("Day","Days"), dataset.filt.split$min_period_num/30.436875,
           ifelse(dataset.filt.split$min_period_unit %in% c("Hour","Hours"), dataset.filt.split$min_period_num/730.485,
                  ifelse(dataset.filt.split$min_period_unit %in% c("Year","Years"),  dataset.filt.split$min_period_num * 12, 0)
           )))
  
  return(dataset.filt.split)
  
}

off_16_17 <- standarize_min_period(off_16_17)
off_18_19 <- standarize_min_period(off_18_19)

slimmer_min_period <- function(dataset,before_after) {
  
  dataset_slim <- dataset %>% 
    dplyr::select(min_period_in_month) %>%
    dplyr::mutate(before_after_LK=before_after)
  return(dataset_slim)
  
}

off_16_17_slim <- slimmer_min_period(off_16_17,"before")
off_18_19_slim <- slimmer_min_period(off_18_19,"after")

off_min_period <- rbind(off_16_17_slim,off_18_19_slim)

ggplot(off_min_period,aes(x=before_after_LK,y=min_period_in_month)) + 
  geom_violin()














#### scraps ####

# comparing grade from 16/17 to 18/19; different judges
summary(off_16_17$grade)
summary(off_18_19$grade)

ggplot(off_16_17, aes(factor(grade))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

unique(off_16_17$disposing_authority__last_name)
