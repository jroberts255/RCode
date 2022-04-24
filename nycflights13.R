
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))

Q1 <- flights %>%
  filter(carrier == "AA" | carrier == "EV" | carrier == "FL") %>%
  group_by(carrier) %>%
  summarise(mean = round(mean(distance),2)) %>%
  as.data.frame()

Q2 <- flights %>%
  group_by(month)%>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1) %>%
  as.data.frame()

Q3 <- flights %>%
  group_by(origin, dest, distance) %>%
  summarise(min_dist = min(distance)) %>%
  arrange(distance) %>%
  head(5) %>%
  select(origin, dest, min_dist) %>%
  as.data.frame()

Q4 <- flights %>%
  group_by(month, day, origin) %>%
  summarise(mean_distance = round(mean(distance),2))%>%
  arrange(mean_distance) %>%
  filter(origin == "JFK") %>%
  arrange(desc(mean_distance)) %>%
  select(month, day, mean_distance) %>%
  head(5) %>%
  as.data.frame()

Q5 <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(max_array_delay = max(arr_delay)) %>%
  arrange(desc(max_array_delay)) %>%
  filter(dest == "BOS" | dest == "ATL")


  
  
  
  
  
  


  
