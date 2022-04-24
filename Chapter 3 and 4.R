library(Lahman)
library(tidyverse)
library(plyr)

hof <- read.csv("hofbatting.csv")

hof$MidCareer <- with(hof, (From + To)/2)
hof$Era <- cut(hof$MidCareer, 
               breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
               labels=c("19th Century", "Lively Ball", "Dead Ball", 
                        "Integration", "Expansion", "Free Agency",
                        "Long Ball"))
T.Era <- table(hof$Era)
T.Era
barplot(T.Era)
barplot(table(hof$Era), xlab = "Era", ylab="Frequency", main = 
          "Era of the Nonpitching Hall of Famers")

plot(table(hof$Era))
pie(table(hof$Era))

pdf("graphs.pdf")
barplot(table(hof$Era))
plot(table(hof$Era))
dev.off()
RStudioGD

T.Era <- table(hof$Era)
dotchart(as.numeric(T.Era), labeals=names(T.Era), xlab = "Frequency")

hof.500 <- subset(hof, HR >= 500)
hof.500 <- hof.500[order(hof.500$OPS), ]
dotchart(hof.500$OPS, labels=hof.500$X, xlab="OPS")

windows(width = 7, height = 3.5)
stripchart(hof$MidCareer, method = "jitter", pch = 1, xlab = "Mid Career")
hist(hof$MidCareer, xlab = "Mid Career", main = "")

with(hof, plot(MidCareer, OPS))
with(hof, lines(lowess(MidCareer, OPS, f =.3)))
with(hof, identify(MidCareer, OPS, X, n=4))

with(hof, plot(OBP, SLG))

#OPS and SLG Graph
with(hof, plot(OBP, SLG, xlim=c(.25, .50),
               ylim = c(0.28, 0.75), pch=19,
               xlab="On-Base Percentage", 
               ylab = "Slugging Percentage"))
curve(.7- x, add=TRUE)
curve(.8- x, add=TRUE)
curve(.9- x, add=TRUE)
curve(1.0- x, add=TRUE)
text(.27, .42, "OPS = 0.7")
text(.27, .52, "OPS = 0.8")
text(.27, .62, "OPS = 0.9")
text(.27, .72, "OPS = 1.0")
with(hof, identify(OBP, SLG, X, n=6))


hof$HR.Rate <- with(hof, HR/AB)

stripchart(HR.Rate ~ Era, data=hof)
par(plt=c(.2, .94, .145, .883))
stripchart(HR.Rate ~ Era, data=hof, method="jitter", pch=1, las=2)

par(plt=c(.2, .94, .145, .883))
boxplot(HR.Rate ~ Era, data = hof, las=2,
        horizontal = TRUE, xlab = "HR Rate")

master <- read.csv("Master.csv")

getinfo <- function(firstname, lastname) {
  playerline <- subset(master, nameFirst==firstname & nameLast==lastname)
  name.code <- as.character(playerline$playerID)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  birthday <- playerline$birthDay
  byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
  list(name.code=name.code, byear=byear)
}

ruth.info <- getinfo("Babe", "Ruth")
aaron.info <- getinfo("Hank", "Aaron")
bonds.info <- getinfo("Barry", 'Bonds')
arod.info <- getinfo("Alex", "Rodriguez")

batting <- read.csv("Batting.csv")
ruth.data <- subset(batting, playerID==ruth.info$name.code)
ruth.data$Age <- ruth.data$yearID - ruth.info$byear

aaron.data <- subset(batting, playerID == aaron.info$name.code)
aaron.data$Age <- aaron.data$yearID - aaron.info$byear

bonds.data <- subset(batting, playerID == bonds.info$name.code)
bonds.data$Age <- bonds.data$yearID - bonds.info$byear

arod.data <- subset(batting, playerID == arod.info$name.code)
arod.data$Age <- arod.data$yearID - arod.info$byear

with(ruth.data, plot(Age, cumsum(HR), lty = 3, lwd=2, 
                    xlab="Age", ylab = "Career Home Runs", 
                    xlim=c(18,45), ylim=c(0,800)))
with(aaron.data, lines(Age, cumsum(HR), lty = 2, lwd = 2))
with(bonds.data, lines(Age, cumsum(HR), lty=1, lwd=2))
with(arod.data, lines(Age, cumsum(HR), lty=4, lwd=2))
legend(20,700, legend = c("Bonds", "Aaron", "Ruth", "ARod"),
       lty=1 : 4, lwd=2)

teams <- Teams
tail(teams)

myteams <- subset(teams, yearID > 2000) [, c("teamID", "yearID", "lgID", 'G', "W", "L", "R", "RA")]
myteams$RD <- with(myteams, R-RA)
myteams$Wpct <- with(myteams, W / (W + L))

linfit <- lm(Wpct ~ RD, data=myteams)
linfit
myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)
plot(myteams$RD, myteams$linResiduals, xlab = 'run differential', ylab = 'residual')
abline(h=0, lty = 3)
point(c(68,88), c(.0749, -.0733), pch=19)
text(68, .0749, "LAA '08", pos = 4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

myteams$pytWpct <- with(myteams, R^2 / (R^2 + RA^2))
myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
sqrt(mean(myteams$pytResiduals ^ 2))

myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytfit <- lm(logWratio ~ 0 + logRratio, data=myteams)



















