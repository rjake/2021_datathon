library(tidyverse)
library(lubridate)
library(vroom)
library(rms)

bail <- vroom::vroom("csv/bail.csv")

larry_date <- as.Date("2018-01-02")
start_period_1 <- larry_date - period(2, "years")
end_period_2 <- larry_date + period(2, "years")

offenses_dispositions <- vroom::vroom("csv/offenses_dispositions_v3.csv")
defendant_docket_details <- vroom::vroom("csv/defendant_docket_details.csv") %>%
  replace_na(list(race = "Unknown/Unreported"))

clean_bail <-
  bail %>%
  mutate(action_type_name = action_type_name %>%
    as.factor() %>%
    relevel(ref = "Set")) %>%
  filter(
    participant_name__title == "Judge",
    !is.na(participant_name__first_name)
  ) %>%
  mutate(action_date = ymd(action_date))

bail_grouped <- bail %>%
  group_by(docket_id) %>%
  summarise(
    date_first_bail = first(action_date),
    ROR_ever = ifelse(any(type_name == "ROR"), 1, 0)
    # did the case ever receive ROR
  )

def_subset <- defendant_docket_details %>% select(docket_id, race)

offense_grouped <-
  offenses_dispositions %>%
  filter(!is.na(grade)) %>%
  group_by(docket_id) %>%
  summarise(most_ser_off = max(unique(grade)))

joined <- bail_grouped %>%
  left_join(def_subset, by = "docket_id") %>%
  left_join(offense_grouped, by = "docket_id") %>%
  filter(!is.na(ROR_ever)) %>%
  mutate(
    period = case_when(
      between(date_first_bail, start_period_1, larry_date) ~ "before",
      between(date_first_bail, larry_date, end_period_2) ~ "after",
      TRUE ~ NA_character_
    ) %>%
      as.factor() %>%
      relevel(ref = "before"),
    grade = case_when(
      grepl("F", most_ser_off) ~ "felony",
      grepl("M", most_ser_off) ~ "misdemeanor",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(period, grade) %>%
  mutate(
    severity = case_when(
      most_ser_off %in% c("M3", "M") ~ "M3/M",
      most_ser_off %in% c("F3", "F") ~ "F3/F",
      TRUE ~ most_ser_off
    ) %>% factor(levels = c("M3/M", "M2", "M1", "F3/F", "F2", "F1")),
    race_cat =
      case_when(
        grepl("Asian", race) ~ "Asian",
        race %in% c("Black", "White") ~ race,
        TRUE ~ "Other/Missing"
      ) %>%
        fct_relevel("White", after = Inf)
  )

model1 <- lrm(ROR_ever ~ (race_cat + severity + period)^2, data = joined)
model1p <- update(model1, penalty = 1.2, data = joined)

dd <- datadist(joined)
options(datadist = "dd")

predictions <- Predict(model1p, period, race_cat, severity) %>%
  mutate(
    est_rate = plogis(yhat),
    lower_ci = plogis(lower),
    upper_ci = plogis(upper)
  ) %>%
  as_tibble()

predictions %>% ggplot(aes(x = race_cat, color = period, y = est_rate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  facet_wrap(~severity) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = "bottom"
  )

predictions %>%
  filter(race_cat %in% c("White", "Black")) %>%
  ggplot(aes(x = race_cat, color = period, y = est_rate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  facet_wrap(~severity) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position = "bottom")

joined$black <- joined$race == "Black"

with(
  filter(joined, period == "before"),
  mantelhaen.test(black, ROR_ever, severity, exact = TRUE)
)
with(
  filter(joined, period == "after"),
  mantelhaen.test(black, ROR_ever, severity, exact = TRUE)
)

ff3 <- joined %>%
  filter(severity == "F3/F")

ff3 %>%
  filter(race == "Black") %>%
  pull(ROR_ever) %>%
  mean()
ff3 %>%
  filter(race == "White") %>%
  pull(ROR_ever) %>%
  mean()

# trang's suggestion

ex1 <- predictions %>%
  filter(
    race_cat %in% c("White", "Black"),
    severity == "M1"
  ) %>%
  ggplot(aes(x = period, linetype = race_cat, y = est_rate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  # facet_wrap(~severity) +
  labs(y = "ROR rate", x = NULL) +
  coord_cartesian(ylim = c(0, NA)) +
  # rcartocolor::scale_color_carto_d(palette = 2) +
  theme_classic() +
  theme(legend.position = "None") +
  directlabels::geom_dl(aes(label = race_cat),
    method = list(directlabels::dl.trans(x = x - .6), "first.bumpup")
  )

ex2 <- predictions %>%
  filter(severity == "M1") %>%
  select(period, race_cat, severity, est_rate) %>%
  pivot_wider(names_from = race_cat, values_from = est_rate) %>%
  mutate(diff = White - Black) %>%
  ggplot(aes(group = severity, x = period, y = diff)) +
  geom_line(color = "#F89C74") +
  geom_point(color = "#F89C74") +
  labs(y = expression(ROR[white] ~ -~ ROR[black]), x = NULL) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(ylim = c(-0.07, 0.13)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "None"
  ) +
  directlabels::geom_dl(aes(label = severity),
    color = "#F89C74",
    method = list(directlabels::dl.trans(x = x - .3), "first.bumpup")
  )

diff_plot <- predictions %>%
  select(period, race_cat, severity, est_rate) %>%
  pivot_wider(names_from = race_cat, values_from = est_rate) %>%
  mutate(diff = White - Black) %>%
  ggplot(aes(group = severity, x = period, y = diff, color = severity)) +
  geom_line() +
  geom_point() +
  labs(y = expression(ROR[white] ~ -~ ROR[black]), x = NULL) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(ylim = c(-0.07, 0.13)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "None"
  ) +
  rcartocolor::scale_color_carto_d(palette = "Pastel") +
  directlabels::geom_dl(aes(label = severity),
    method = list(directlabels::dl.trans(x = x - .3), "first.bumpup")
  )

ggsave("reports/figs/ror-1.png", ex1, height = 4.5, width = 3.5)
ggsave("reports/figs/ror-2.png", ex2, height = 4.5, width = 3.5)
ggsave("reports/figs/ror-3.png", diff_plot, height = 4.5, width = 3.5)
