suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(lm.beta))

flights_upper <- quantile(flights$dep_delay, .997, na.rm = TRUE)
flights_lower <- quantile(flights$dep_delay, .003, na.rm = TRUE)

flights_out <- which(flights$dep_delay > flights_upper | flights$dep_delay < flights_lower)

flights_noout <- flights[-flights_out,]

Q1 <- round((nrow(flights) - length(flights_out))/nrow(flights)*100,2)

Q2 <- cor.test(flights_noout$dep_delay, flights_noout$distance)

Q3b <- lm(dep_delay ~ distance, data = flights_noout)
Q3 <- summary(Q3b)

Q4 <- lm.beta(Q3b)

Q5b <- lm(dep_delay ~ distance + carrier, data = flights_noout) 
Q5 <- summary(Q5b)

