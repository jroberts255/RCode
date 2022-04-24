Batting <- read.csv("Batting.csv")
Master <- read.csv("Master.csv")

mantle.info <- subset(Master, nameFirst == "Mickey" & nameLast == 'Mantle')
mantle.id <- as.character(mantle.info$playerID)


library(car)
Batting$SF <- recode(Batting$SF, "NA = 0")
Batting$HBP <- recode(Batting$HBP, "NA = 0")

get.birthyear <- function(player.id) {
  playerline <- subset(Master, playerID == player.id)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  ifelse(birthmonth >= 7, birthyear + 1, birthyear)
}

get.birthyear(mantle.id)
get.stats <- function(player.id) {
  d <- subset(Batting, playerID == player.id)
  byear <- get.birthyear(player.id)
  d$Age <- d$yearID - byear
  d$SLG <- with(d, (H- X2B - X3B - HR +
                      2 * X2B + 3 * X3B + 4 * HR) / AB)
  d$OBP <- with(d, (H + BB) / (H + AB + BB + SF))
  d$OPS <- with(d, SLG + OBP)
  d
}
Mantle <- get.stats(mantle.id)

with(Mantle, plot(Age, OPS, cex=1.5, pch=19))

fit.model <- function(d){
  fit <- lm(OPS ~ I(Age -30) + I((Age-30)^2), data = d)
  b <- coef(fit)
  Age.max <- 30 - b[2] / b[3] / 2
  Max <- b[1] - b[2] ^ 2 / b[3] / 4
  list(fit=fit,
       Age.max=Age.max, Max=Max)
}

F2 <- fit.model(Mantle)

lines(Mantle$Age, predict(F2$fit, Age = Mantle$Age), lwd=3)
abline(v=F2$Age.max, lwd=3, lty=2, col='grey')
abline(h=F2$Max, lwd=3, lty=2, col='grey')
text(29, .72, "Peak.age", cex=2)
text(20, 1, "Max", cex =2)

Fielding <- read.csv("Fielding.csv")

library(plyr)
AB.totals <- ddply(Batting, .(playerID),
                   summarize, 
                   Career.AB = sum(AB, na.rn = TRUE))
Batting <- merge(Batting, AB.totals)
Batting.2000 <- subset(Batting, Career.AB >= 2000)

find.position <- function(p){
  positions <- c("OF", "1B", "2B", "SS", "3B", "C", "P", "DH")
  d <- subset(Fielding, playerID == p)
  count.games <- function(po)
    sum(subset(d, POS == po)$G)
  FLD <- sapply(positions, count.games)
  positions[FLD == max(FLD)][1]
}

PLAYER <- as.character(unique(Batting.2000$playerID))
POSITIONS <- sapply(PLAYER, find.position)
Fielding.2000 <- data.frame(playerID = names(POSITIONS),
                            POS=POSITIONS)
Batting.2000 <- merge(Batting.2000, Fielding.2000)
library(plyr)
Batting.2000

C.totals <- ddply(Batting.2000, .(playerID),
                  summarize,
                  C.G=sum(G, na.rm=TRUE),
                  C.AB = sum(AB, na.rm=TRUE),
                  C.R = sum(R, na.rm=TRUE),
                  C.H = sum(H, na.rm=TRUE),
                  C.2B = sum(X2B, na.rm=TRUE),
                  C.3B = sum(X3B, na.rm=TRUE),
                  C.HR = sum(HR, na.rm=TRUE),
                  C.RBI = sum(RBI, na.rm=TRUE),
                  C.BB = sum(BB, na.rm=TRUE),
                  C.SO = sum(SO, na.rm=TRUE),
                  C.SB = sum(SB, na.rm=TRUE))

C.totals$C.AVG <- with(C.totals, C.H / C.AB)
C.totals$C.SLG <- with(C.totals,
                       (C.H - C.2B - C.3B - C.H + 2 * C.2B + 
                          3 * C.3B + 4 * C.HR) / C.AB)
C.totals <- merge(C.totals, Fielding.2000)
C.totals$Value.POS <- with(C.totals,
                           ifelse(POS == "C", 240,
                           ifelse(POS == "SS", 168,
                           ifelse(POS == "2B", 132,
                           ifelse(POS == "3B", 84,
                           ifelse(POS == "OF", 48,
                           ifelse(POS == "1B", 12, 0)))))))

similar <- function(p, number=10){
  P <- subset(C.totals, playerID == p)
  C.totals$SS <- with(C.totals,
                      1000 -
                        floor(abs(C.G - P$C.G) / 20) -
                        floor(abs(C.AB - P$C.AB) / 75) -
                        floor(abs(C.R - P$C.R) / 10) -
                        floor(abs(C.H - P$C.H) / 15) -
                        floor(abs(C.2B - P$C.2B) / 5) -
                        floor(abs(C.3B - P$C.3B) / 4) -
                        floor(abs(C.HR - P$C.HR) / 2) -
                        floor(abs(C.RBI - P$C.RBI) / 10) -
                        floor(abs(C.BB - P$C.BB) / 25) -
                        floor(abs(C.SO - P$C.SO) / 150) -
                        floor(abs(C.SB - P$C.SB) / 20) - 
                        floor(abs(C.AVG - P$C.AVG) / 0.001) - 
                        floor(abs(C.SLG - P$C.SLG) / 0.002) -
                        abs(Value.POS - P$Value.POS))
  C.totals <- C.totals[order(C.totals$SS, decreasing = TRUE), ]  
  C.totals[1:number, ]
}

similar(mantle.id, 6)

collapse.stint <- function(d){
  G <- sum(d$G); AB <- sum(d$AB); R <- sum(d$R)
  H <- sum(d$H); X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <- sum(d$HR); RBI <- sum(d$RBI); SB <- sum(d$SB)
  CS <- sum(d$CS); BB <- sum(d$BB); SH <- sum(d$SH)
  SF <- sum(d$SF); HBP <- sum(d$HBP)
  SLG <- (H - X2B - X3B - HR + 2 * X2B +
            3 * X3B + 4 * HR) / AB
  OBP <- (H + BB + HBP) / (AB + BB + HBP + SF)
  OPS <- SLG + OBP
  data.frame(G = G, AB = AB, R = R, H = H, X2B = X2B,
             X3B = X3B, HR = HR, RBI = RBI, SB = SB,
             CS = CS, BB = BB, HBP = HBP, SH = SH, SF = SF, 
             SLG = SLG, OBP = OBP, OPS = OPS,
             Career.AB = d$Career.AB[1], POS = d$POS[1])
}

Batting.2000 <- ddply(Batting.2000, 
                      .(playerID, yearID), collapse.stint)

player.list <- as.character(unique(Batting.2000$playerID))
#birthyears <- sapply(player.list, get.birthyear)
#Batting.2000 <- merge(Batting.2000,
                      #data.frame(playerID=player.list,
                                 #Birthyear=birthyears))
#Batting.2000$Age <- with(Batting.2000, yearID - Birthyear)














