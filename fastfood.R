suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))

fastfood <- openintro::fastfood
view(fastfood)

Q1 <- fastfood %>%
  filter(restaurant == "Burger King" | restaurant == "Chick Fil-A") %>%
  arrange(desc(calories)) %>%
  select(item) %>%
  head(1)
  
Q2 <- fastfood %>%
  filter(restaurant == "Subway") %>%
  filter(!is.na(sugar)) %>%
  summarise(mean_sugar = round(mean(sugar),2)) %>%
  as.data.frame()
  #summarise(meansugar == mean(sugar))


Q3 <- fastfood %>%
  filter(restaurant == 'Taco Bell') %>%
  filter(!is.na(calories)) %>%
  summarize(mean_calories = round(mean(calories), 2)) %>%
  as.data.frame 

Q4 <- fastfood %>%
  group_by(restaurant, item) %>%
  filter(!is.na(total_fat)) %>%
  filter(!is.na(sugar)) %>%
  summarise(fatXsugar = round(total_fat * sugar, 2)) %>%
  arrange(desc(fatXsugar)) %>%
  select(restaurant, item, fatXsugar) %>%
  head(3)

Q5b <- fastfood %>%
  filter(!is.na(sat_fat)) %>%
  group_by(restaurant) %>%
  summarise(avgsatfat = mean(sat_fat))%>%
  filter(avgsatfat > 10)

Q5 <- nrow(Q5b)
