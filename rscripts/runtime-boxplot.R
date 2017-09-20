#!/usr/bin/env Rscript

source('prelude.R')

f <- path("runtime.mot.csv")
data <- read.csv(f)

variable <- c("Time", "JetStream++", "JetStream", "HLS",
              "Streaming over TCP", "Streaming over UDP",
              "AWStream")
levels <- c("AWStream", "JetStream++", "JetStream", "HLS",
            "Streaming over TCP", "Streaming over UDP")
levels <- factor(levels, levels=levels)

latency.columns <- c("time", "jet.latency", "js.latency", "hls.latency",
                     "tcp.latency", "udp.latency", "aws.latency")
accuracy.columns <- c("time", "jet.accuracy", "js.accuracy", "hls.accuracy",
                      "tcp.accuracy", "udp.accuracy", "aws.accuracy")

##################################
##
## Latency
##
##################################
latency <- data[data$time > 205 & data$time < 440, latency.columns]
latency$udp.latency <- jitter(latency$udp.latency, factor=5)
names(latency) <- variable
latency.data <- melt(latency, id="Time")
latency.data$value <- log10(latency.data$value / 1000)

##################################
##
## Accuracy
##
##################################
accuracy <- data[data$time > 205 & data$time < 440, accuracy.columns]
accuracy$udp.accuracy <- jitter(accuracy$udp.accuracy, factor=5)
names(accuracy) <- variable
accuracy.data <- melt(accuracy, id="Time")
accuracy.data$value <- accuracy.data$value + 10

##################################
##
## Combine
##
##################################
labels <- c("Latency (ms)", "Accuracy (F1 score)")
labels <- factor(labels, levels=labels)
latency.data$label <- labels[1]
accuracy.data$label <- labels[2]
combined <- rbind(latency.data, accuracy.data)

plot <- ggplot(combined, aes(x=factor(variable), y=value)) +
    geom_boxplot(outlier.size=.5, outlier.alpha = 0.5) +
    facet_grid(. ~ label, scales="free") +
    xlab(NULL) +
    ylab(NULL) +
    theme(strip.background=element_blank()) +
    theme(strip.text.x=element_text(size = 20)) +
    scale_x_discrete(limits=rev(levels)) +
    scale_y_continuous(breaks=c(-1, 0, 1, 10.0, 10.2, 10.4, 10.6, 10.8, 11),
                       labels=c(100, 1000, 10000, 0.0, 0.2, 0.4, 0.6, 0.8, 1)) +
    coord_flip()
plot

ggsave(plot, file="runtime_mot-boxplot.pdf", width=8, height=3)

