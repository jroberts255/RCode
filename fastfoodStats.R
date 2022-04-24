suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))

fastfood <- openintro::fastfood

Q1b <- fastfood %>%
  na.omit(calories, total_fat, sugar, calcium) %>%
  filter(restaurant == 'Sonic' | restaurant == "Subway" | restaurant == "Taco Bell") %>%
  select(calories, total_fat, sugar, calcium)

Q1 <- round(cor(Q1b, method = "pearson"),2)

Q2b <- fastfood %>%
  filter(restaurant == "Mcdonalds" | restaurant == "Subway") %>%
  mutate(restaurantbin = ifelse(restaurant == 'Subway', 0, 1))

Q2a <- glm(restaurantbin~calories + sodium + protein, data = Q2b, family = "binomial")

Q2 <- round(coef(Q2a),2)

Q3a <- glm(restaurantbin~calories + protein, data = Q2b, family = "binomial")

Q3 <- round(AIC(Q2a),2)

Q4b <- lm(calories~sat_fat+fiber+sugar, data = fastfood)
Q4 <- round(coef(Q4b)[2:2],2)

Q5b <- fastfood %>%
  group_by(restaurant) %>%
  filter(n() >= 50 & n() <=60)

Q5a <- lm(total_fat ~ cholesterol + total_carb + restaurant, data = Q5b)
coefs <- round(coef(lm.beta::lm.beta(Q5a)), 2)
Q5 <- coefs[which.max(coefs)]

