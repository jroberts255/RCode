suppressPackageStartupMessages(library(tidyverse))

msleep
Q1 <- cor.test(msleep$sleep_total, msleep$bodywt)

Q2b <- msleep %>%
  filter(!is.na(sleep_total)) %>%
  filter(!is.na(sleep_rem)) %>%
  filter(!is.na(brainwt)) %>%
  filter(!is.na(bodywt)) %>%
  select(sleep_total, sleep_rem, brainwt, bodywt) 

Q2 <- round(cor(Q2b),2)

Q3b <- lm(bodywt ~ vore, data= msleep)
Q3 <- round(coef(Q3b),2)

Q4b <- lm(msleep$bodywt ~ msleep$vore + msleep$sleep_rem, data = msleep)

Q4 <- AIC(Q4b)

Q5b <- msleep %>%
  filter(vore != "omni" & vore != "insecti") %>%
  mutate(vorebin = ifelse(vore == 'carni', 0, 1))

Q5 <- glm(vorebin ~ sleep_total, data = Q5b, family = "binomial")
