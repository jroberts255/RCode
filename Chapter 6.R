library(tidyverse)
library(Lahman)
library(ggplot2)
library(lattice)

load("balls_strikes_count.Rdata")

sampleRows <- sample(1:nrow(verlander), 20)
verlander[sampleRows,]

histogram(~ speed, data=verlander)

densityplot(~ speed, data=verlander, plot.points=FALSE)

densityplot(~ speed | pitch_type, data=verlander, layout=c(1,5), plot.points=FALSE)

densityplot(~ speed, data=verlander, groups=pitch_type, plot.points = FALSE, auto.key = TRUE)

F4ver1 <- subset(verlander, pitch_type == "FF")
F4ver1$gameDay <- as.integer(format(F4ver1$gamedate, format="%j"))

dailySpeed <- aggregate(speed ~ gameDay + season, data = F4ver1, FUN=mean)

xyplot(speed ~ gameDay | factor(season),
       data=dailySpeed,
       xlab = "day of the year",
       ylab = "pitch speed (mph)")

speedFC <- subset(verlander, pitch_type %in% c("FF", "CH"))
avgspeedFC <- aggregate(speed ~ pitch_type + season,
                        data = speedFC, FUN=mean)
avgspeedFC <- droplevels(avgspeedFC)
avgspeedFC

dotplot(factor(season) ~ speed, groups = pitch_type,
        data=avgspeedFC,
        pch = c("C", "F"), cex = 2)

avgSpeed <- aggregate(speed ~ pitches + season, data=F4ver1, FUN=mean)
xyplot(speed ~ pitches | factor(season),
       data=avgSpeed)
avgSpeedComb <- mean(F4ver1$speed)

NoHit <- subset(verlander, gamedate == "2011-05-07")

xyplot(pz ~ px | batter_hand, data=NoHit, groups = pitch_type, auto.key=TRUE)
xyplot(pz ~ px | batter_hand, data=NoHit, groups = pitch_type, auto.key = TRUE,
       aspect="iso")

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key=TRUE,
       aspect="iso",
       xlim=c(-2.2, 2.2),
       ylim=c(0,5),
       xlab="Horizontal Location\n(ft. from middle of plate)",
       ylab = "Vertical Location\n(ft. from ground)")

pitchnames <- c("change-up", "curveball", "4s-fastball", "2S-fastball", "slider")
myKey <- list(space="right",
              border=TRUE,
              cex.title=.8,
              title="pitch type",
              text=pitchnames,
              padding.text=4)

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type,
       auto.key = myKey,
       aspect="iso",
       xlim=c(-2.2, 2.2),
       ylim = c(0,5),
       xlab="horizontal location\n(ft. from middle of plate)",
       ylab = "vertical location\n(ft. from ground)",
       panel=function(...) {
         panel.xyplot(...)
         panel.rect(inKzone, botKzone, outKzone, topKzone, border="black", lty =3)
       }
)

sampleRows <- sample(1:nrow(cabrera), 20)
cabrera[sampleRows,]

p0 <- ggplot(data = cabrera, aes(x=hitx, y=hity))
p1 <- p0 + geom_point()
p1

p0 <- ggplot(data=cabrera, aes(hitx, hity))
p1 <- p0 + geom_point(aes(color=hit_outcome))
p2 <- p1 + coord_equal()
p2

p3 <- p2 + facet_wrap(~ season)
p3

bases <- data.frame(x=c(0, 90/sqrt(2), 0, -90/sqrt(2), 0),
                    y=c(0, 90/sqrt(2), 2 * 90/sqrt(2), 90/sqrt(2), 0))

p4 <- p3 + geom_path(aes(x=x, y=y), data=bases)
p4 + geom_segment(x=0, xend=300, y=0, yend=300) +
  geom_segment(x=0, xend=-300, y=0, yend=300)

cabreraStretch <- subset(cabrera, gamedate > "2012-08-31")
p0 <- ggplot(data = cabreraStretch, aes(hitx, hity))
p1 <- p0 + geom_point(aes(shape=hit_outcome, colour=pitch_type,
                          size=speed))

p2 <- p1 + coord_equal()
p3 <- p2 + geom_path(aes(x=x, y=y), data=bases)
p4 <- p3 + guides(col=guide_legend(ncol=2))
p4 + 
  geom_segment(x=0, xend=300, y = 0, yend=300) + 
  geom_segment(x=0, xend=300, y=0, yend=300)

#ggplot(F4ver1, aes(pitches, speed)) + 
  #facet_wrap(~ season) + 
  #geom_line(stat="hline", yintercept="mean", lty = 3) +
  #geom_point(aes(pitches, speed),
             #data=F4ver1[sample(1:nrow(F4ver1), 1000), ]) +
  #geom_smooth(col="black") + 
  #geom_vline(aes(xintercept=100), col = "black", lty=2)

kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

ggplot(Fver1, aes(px, pz)) +
  geom_point() + 
  facet_wrap(~ batter_hand) +
  coord_equal() +
  geom_path(aes(x,y), data=kZone, lwd=2, col="white")

Fver1  






