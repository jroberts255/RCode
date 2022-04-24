suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))

BRFSS2015 <- read_csv("BRFSS2015.csv")

Q1 <- BRFSS2015 %>% 
  filter(!(is.na(HLTHPLN1))) %>%
  tally() 

Q2b <- BRFSS2015 %>%
  mutate(ifelse(is.null(MENTHLTH), 0, MENTHLTH))

Q2 <- Q2b %>%
  summarise(avgMENTHLTH = round(mean(MENTHLTH),2))

Q3b <- BRFSS2015 %>%
  filter(HAVARTH3 != 7) %>%
  filter(HAVARTH3 != 9) %>%
  filter(!is.na(HAVARTH3)) %>%
  filter(!is.na(WTKG3)) %>%
  mutate(Weight = round((WTKG3*2.20462)/100,2))

Q3a <- Q3b %>%
  select(HAVARTH3, Weight)

Q3 <- Q3a %>%
  group_by(HAVARTH3) %>%
  summarise(mean_weight = mean(Weight), sd_weight = sd(Weight)) 

names(BRFSS2015) [names(BRFSS2015) == '_MINAC11'] <- 'MINAC11'

phys_upper <- quantile(BRFSS2015$MINAC11, .997, na.rm = TRUE)
phys_upper

phys_lower <- quantile(BRFSS2015$MINAC11, .003, na.rm = TRUE)
phys_lower

phys_out <- which(BRFSS2015$MINAC11 > phys_upper | BRFSS2015$MINAC11 < phys_lower)


Q4 <- round((nrow(BRFSS2015) - length(phys_out))/nrow(BRFSS2015)*100,2)

phys_noout <- BRFSS2015[-phys_out,]

Q5 <- phys_noout %>%
  group_by(MARITAL) %>%
  summarize(meanphys = mean(PHYSHLTH), sdphys = sd(PHYSHLTH), minphys = min(PHYSHLTH), maxphys = max(PHYSHLTH)) 

Q6 <-ggplot(data = phys_noout, aes(group = MARITAL, MARITAL, PHYSHLTH)) +
  geom_boxplot() 

Q7b <- lm(MINAC11 ~ as.factor(MARITAL), data = phys_noout)
Q7 <- summary(Q7b)
Q8b <- aov(Q7b)
Q8 <- TukeyHSD(Q8b)

Q9b <- lm(PHYSHLTH ~ MARITAL + FRUTDA1_, data = BRFSS2015clean)
Q9 <- AIC(Q9b)

#Question 10 Code

Q10b <- BRFSS2015 %>%
  filter(MAXDRNKS != 77) %>%
  filter(MAXDRNKS != 99) %>%
  filter(!is.na(MAXDRNKS))

DRNKS_upper <- quantile(Q10b$MAXDRNKS, .997, na.rm = TRUE)

DRNKS_lower <- quantile(Q10b$MAXDRNKS, .003, na.rm = TRUE)

DRNKS_out <- which(Q10b$MAXDRNKS > DRNKS_upper | Q10b$MAXDRNKS < DRNKS_lower)

DRNKS_noout <- Q10b[-DRNKS_out,]

Q10 <- DRNKS_noout
#I want to see if exercise and overall health has an impact on the amount of alcohol consumed by an individual
#I took values out that were not within 3 standard deviations of the mean, 
#This took out people had drinks per month in the range of over 60, there were around 26 observations within this criteria that would have been outliers in our study
#Furthermore, 3 standard deviations from the mean gets to 24 drinks, which allows us to eliminate outliers


Q11 <- Q10 %>%
  filter(!is.na(INSULIN)) %>%
  filter(!is.na(EXERANY2)) %>%
  filter(!is.na(SMOKE100)) %>%
  filter(!is.na(MAXDRNKS)) %>%
  filter(!is.na(GENHLTH)) %>%
  filter(INSULIN != 9) %>%
  filter(EXERANY2 != 7) %>%
  filter(EXERANY2 != 9) %>%
  filter(SMOKE100 != 7) %>%
  filter(SMOKE100 != 9) %>%
  filter(MAXDRNKS != 99) %>%
  filter(MAXDRNKS != 77) %>%
  filter(GENHLTH != 7) %>%
  filter(GENHLTH != 9) %>%
  select(GENHLTH, INSULIN, EXERANY2, SMOKE100, MAXDRNKS)

#removed all 7, 9, 99, 77, and blanks. The numbers listed are because the respondent did not provide a response or the answer was not asked or not provided by interviewee so these were removed the !is.na statements

Q12 <- pairs(Q11)

Q13 <- summary(Q11)

Q14b <- lm(MAXDRNKS ~ INSULIN + EXERANY2 + SMOKE100 + GENHLTH, data = Q11)
summary(Q14b)
#this shows statistical significance for Insulin, SMOKE100, and GENHLTH, EXERANY2 omitted because statistically significant

Q14 <- lm(MAXDRNKS ~ INSULIN + SMOKE100 + GENHLTH, data = Q11)
summary(Q14)  
  
  
  
  
  


