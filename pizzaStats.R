suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))

pizza <- read_csv('pizza.csv')

Q1b <- pizza %>%
  select(temperature, bill, pizzas, got_wine)
Q1 <- round(cor(Q1b),2)

Q2b <- pizza %>%
  filter(operator == 'Laura' & branch == "East") %>%
  select(time, temperature, bill, pizzas)

Q2 <- round(cor(Q2b),2)

Q3b <- glm(got_wine ~ temperature + bill + pizzas, data = pizza, family = 'binomial')
Q3 <- round(summary(Q3b)$coefficients, 2)

Q4b <- lm(bill ~ temperature + pizzas + got_wine, data = pizza)
Q4a <- summary(Q4b)
Q4 <- lm.beta(Q4b)

Q5b <- lm(bill ~ temperature + pizzas + got_wine + operator, data = pizza)
Q5 <- round(AIC(Q4b),2)

