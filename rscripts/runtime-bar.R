#!/usr/bin/env Rscript

source('prelude.R')

f <- path("mot.runtime.csv")
data <- read.csv(f)

variable <- c("Time", "JetStream++", "JetStream", "Streaming w/ TCP",
              "AWStream")
levels <- c("AWStream", "JetStream++", "JetStream",
            "Streaming w/ TCP")
levels <- factor(levels, levels=levels)

latency.columns <- c("time", "jet.latency", "js.latency",
                     "tcp.latency", "aws.latency")
accuracy.columns <- c("time", "jet.accuracy", "js.accuracy",
                      "tcp.accuracy", "aws.accuracy")

##################################
##
## Latency
##
##################################
latency <- data[data$time > 205 & data$time < 380, latency.columns]
names(latency) <- variable
latency.data <- melt(latency, id="Time")
latency.data$value <- log10(latency.data$value / 1000)

##################################
##
## Accuracy
##
##################################
accuracy <- data[data$time > 205 & data$time < 380, accuracy.columns]
names(accuracy) <- variable
accuracy.data <- melt(accuracy, id="Time")

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
    ylab("") +
    theme(strip.background=element_blank()) +
    theme(strip.text.x=element_text(size = 20)) +
    scale_x_discrete(limits=rev(levels)) +
    scale_y_continuous(breaks=c(-1, 0, 1), labels=c(100, 1000, 10000)) +
    coord_flip()
plot

plot2 <- ggplot(combined, aes(x=factor(variable), y=value)) +
    geom_boxplot(outlier.size=.5, outlier.alpha = 0.5) +
    facet_grid(. ~ label, scales="free") +
    xlab(NULL) +
    ylab("") +
    theme(strip.background=element_blank()) +
    theme(strip.text.x=element_text(size = 20)) +
    scale_x_discrete(limits=rev(levels)) +
    scale_y_continuous(limits=c(0.7, 1.0),
                       breaks=c(0.7, 0.8, 0.9, 1.0),
                       labels=c(0.7, 0.8, 0.9, 1.0)) +
    coord_flip()
plot2

g1 <- ggplotGrob(plot)
g2 <- ggplotGrob(plot2)
g1[["grobs"]][[7]] <- g2[["grobs"]][[6]]

library(grid)
pdf("runtime_mot-bar.pdf", width=8, height=4)
grid.draw(g1)
dev.off()


