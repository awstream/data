#!/usr/bin/env Rscript

source('prelude.R')
library(zoo)

f <- path("runtime.topk.csv")
runtime.data <- read.csv(f)

legends <- c("time",
             "AWStream",
             "Streaming over TCP",
             "Streaming over UDP")

## Create a test plot with proper size
dev.new(width=7, height=2.3)

##
## Bandwidth (throughput)
##
bw <- runtime.data[,c("time",
                      "aws.throughput",
                      "tcp.throughput",
                      "udp.throughput")]
names(bw) <- legends
bw.data <- melt(bw, id.vars="time")
bw.data$value <- bw.data$value / 1000

bw.plot <- ggplot(bw.data, aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point(size=2) +
    geom_line(linetype=2, size=1) +
    xlab("") +
    ylab("Throughput\n(mbps)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 3), labels=function(x) format(x, nsmall=1)) +
    theme(legend.title=element_blank(),
          legend.spacing.x=unit(4, "lines"),
          legend.key.width=unit(4, "line")) +
    guides(colour=guide_legend(nrow=2, byrow=TRUE)) +
    scale_color_jco()
bw.plot

##
## latency plot
##
latency <- runtime.data[,c("time", "aws.latency", "tcp.latency", "udp.latency")]
names(latency) <- legends
latency <- melt(latency, id.vars="time")
latency$value <- latency$value / 1000

latency_label <- function(x) { format(x, nsmall=1) }

latency.plot <- ggplot(latency,
                       aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point(size=2) +
    geom_line(linetype=2, size=1) +
    scale_y_log10(breaks=c(0.1, 1, 10, 100), labels=Vectorize(latency_label)) +
    xlab("") +
    ylab("Latency\n(seconds)") +
    xlim(30, 650) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_color_jco()
latency.plot

##
## accuracy plot
##
accuracy <- runtime.data[,c("time", "aws.accuracy",
                            "tcp.accuracy", "udp.accuracy")]
accuracy <- melt(accuracy, id.vars="time")

accuracy.plot <- ggplot(accuracy, aes(x=time, y=value,
                                      colour=variable, shape=variable)) +
    geom_point(size=2) +
    geom_line(linetype=2, size=1) +
    xlab("Time (seconds)") +
    ylab("Accuracy\n(F1 Score)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 1.0),
                       labels=function(x) format(x, nsmall=1)) +
    theme(legend.position="none") +
    scale_color_jco()
accuracy.plot

##
## Final plot uses cowplot to arrange everything
##
pcol <- plot_grid(bw.plot + theme(legend.position="none"),
                  latency.plot + theme(legend.position="none"),
                  accuracy.plot + theme(legend.position="none"),
                  align = 'vh', ncol = 1)

bw.plot + theme(legend.position="top",
                legend.title = element_blank())

legend <- get_legend(bw.plot +
                     theme(legend.position="top",
                           legend.title = element_blank()))
##                   guides(colour=guide_legend(nrow=2,byrow=TRUE))


p <- plot_grid(legend, pcol, ncol = 1, rel_heights = c(.2, 1))
p

pdf("runtime_tk-timeseries.pdf", width=8, height=8)
p
dev.off()
