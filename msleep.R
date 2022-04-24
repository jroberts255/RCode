suppressPackageStartupMessages(library(tidyverse))

Q1 <- msleep %>%
  filter(vore == 'carni' & conservation == 'lc') %>%
  summarise(var = round(var(sleep_total), 2)) %>%
  as.data.frame()

Q2 <- msleep %>%
  group_by(name) %>%
  filter(!is.na(sleep_total)) %>%
  filter(!is.na(sleep_rem)) %>%
  filter(order == "Rodentia") %>%
  summarize(ratio = round(sleep_total/sleep_rem, 2)) %>%
  arrange(desc(ratio)) %>%
  select(name) %>%
  head(1)


Q3b <- msleep %>%
  group_by(name) %>%
  filter(order == "Primates") %>%
  filter(!is.na(brainwt)) %>%
  filter(!is.na(bodywt)) %>%
  summarize(ratio = round(bodywt/brainwt, 0)) %>%
  filter(ratio > 100) %>%
  as.data.frame()

Q3 <- nrow(Q3b)

Q4 <- msleep %>%
  filter(!is.na(sleep_total)) %>%
  filter(!is.na(conservation)) %>%
  group_by(conservation) %>%
  mutate(mean_sleep = round(mean(sleep_total),2)) %>%
  mutate(var_sleep = round(var(sleep_total), 2)) %>%
  arrange(conservation) %>%
  select(conservation, mean_sleep, var_sleep) %>%
  distinct(conservation, mean_sleep, var_sleep) %>%
  as.data.frame()

Q5 <- msleep %>%
  filter(sleep_total > 12 & conservation == 'domesticated' & vore == "herbi") %>%
  select(name) 
