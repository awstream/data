library(zoo)

## Latency and BW

js <- read.csv("~/Downloads/latency.bw.csv")
head(js)

bw <- rollapply(js$bytes, width=5, by=5, FUN=mean, align="left")
latency <- rollapply(js$latency, width=5, by=5, FUN=mean, align="left")

jet <- as.data.frame(cbind(bw / 1000 * 8 / 3, latency))
write.csv(jet, "jetstream.csv", row.names=F)

## Accuracy

js <- read.csv("~/Downloads/acc.csv")
head(js)

# accuracies <- c(0.733, 0.763, 0.765, 0.781, 0.782, 0.792, 0.805, 0.825, 0.846,
#                 0.865, 0.881, 0.898)

accuracies <- c()

accuracy.raw <- accuracies[js$level]
accuracy <- rollapply(accuracy.raw, width=150, by=150, FUN=mean, align="left")

accuracy.raw <- accuracies[js$level2]
accuracy <- rollapply(accuracy.raw, width=150, by=150, FUN=mean, align="left")
accuracy

accuracy.raw <- accuracies[js$level3]
accuracy <- rollapply(accuracy.raw, width=150, by=150, FUN=mean, align="left")
accuracy

write.csv(accuracy, "js.accuracy.csv", row.names=F)
