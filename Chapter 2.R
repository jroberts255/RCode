library(Lahman)
library(tidyverse)
library(plyr)

Batting
Batting.60 <- subset(Batting, yearID >= 1960 & yearID <= 1969)
compute.hr <- function(pid) {
  d <- subset(Batting.60, playerID == pid)
  sum(d$HR)
}

players <- unique(Batting.60$playerID)
S <- sapply(players, compute.hr)

R <- data.frame(Player=players, HR=S)
R <- R[order(R$HR, decreasing = TRUE), ]

head(R)

dataframe.AB <- ddply(Batting, .(playerID), summarize, Career.AB=sum(AB, na.rm=TRUE))
Batting <- merge(Batting, dataframe.AB, by = "playerID")
Batting.5000 <- subset(Batting, Career.AB >= 5000)
ab.hr.so <- function(d) {
  c.AB <- sum(d$AB, na.rm = TRUE)
  c.HR <- sum(d$HR, na.rm = TRUE)
  c.SO <- sum(d$SO, na.rm = TRUE)
  data.frame(AB=c.AB, HR=c.HR, SO =c.SO)
}
aaron <- subset(Batting.5000, playerID == "aaronha01")
ab.hr.so(aaron)
d.5000 <- ddply(Batting.5000, .(playerID), ab.hr.so)

with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))












