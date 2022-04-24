data2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2011) <- fields[, "Header"]

data2011$HALF.INNING <- with(data2011, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
                               (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

get.state <- function(runner1, runner2, runner3, outs) {
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}                               

RUNNER1 <- ifelse(as.character(data2011[, "BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[, "BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[, "BASE3_RUN_ID"])=="", 0, 1)
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

