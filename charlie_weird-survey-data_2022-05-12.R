library(tidyverse)
library(readxl)

data_raw <- read_excel("data/weird-survey-data_2022-05-12.xlsx",
                       col_names = c("info", "response_label", "response_count"))

all_responses <- unique(data_raw$response_label)


data_col_arranged <- data_raw %>% 
  mutate(survey_group = case_when(
    str_starts(info, "[0-9]{4} [A-z]") ~ info,
    TRUE ~ NA_character_
  )) %>% 
  mutate(survey_question = case_when(
    str_starts(info, "[0-9]{4} [A-z]") ~ NA_character_,
    TRUE ~ info
  )) %>% 
  select(survey_group, survey_question, contains("response"))

data_col_filled <- data_col_arranged %>% 
  mutate(survey_group_lag = ifelse(is.na(survey_group), 0, 1),
         survey_group_lag = cumsum(survey_group_lag)) %>% 
  group_by(survey_group_lag) %>% 
  mutate(survey_group = first(survey_group)) %>% 
  ungroup() %>% 
  mutate(survey_question_lag = ifelse(is.na(survey_question), 0, 1),
         survey_question_lag = cumsum(survey_question_lag)) %>% 
  group_by(survey_question_lag) %>% 
  mutate(survey_question = first(survey_question)) %>% 
  ungroup() %>% 
  select(survey_group, survey_question, contains("response"))

data_tidy <- data_col_filled %>% 
  filter(!response_label %in% c("Answer")) %>% 
  mutate(response_count = parse_number(response_count))

gg_charlie_chart <- data_tidy %>%
  ggplot(aes(x = response_count,
             y = str_wrap(survey_question, 20),
             fill = response_label)) +
  geom_col() +
  facet_wrap(~ survey_group)


gg_charlie_chart %>% 
  ggsave("target-chart_weird-survey-data_2022-05-12.png",
         .,
         width = 7,
         heigh = 6)

         