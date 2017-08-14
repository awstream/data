#!/usr/bin/env Rscript

library(reshape2)
library(ggsci)

f <- "mot.runtime.csv"  ## darknet.runtime.csv
runtime.data <- read.csv(f)
legends <- c("time", "AWStream", "Streaming over TCP", "Streaming over UDP")

## Create a test plot with proper size
dev.new(width=3.2, height=2.4)

##
## Bandwidth (throughput)
##
bw <- runtime.data[,c("time", "ads.throughput", "tcp.throughput", "udp.throughput")]
names(bw) <- legends
bw <- melt(bw, id.vars="time")
bw$value <- bw$value / 1000

bw.plot <- ggplot(bw, aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point() +
    geom_line(linetype=2) +
    xlab("") +
    ylab("Throughput (mbps)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 25), labels=function(x) format(x, nsmall=1)) +
    academic_paper_theme() +
    theme(legend.position="none") +
    scale_color_jco()
bw.plot

##
## latency plot
##
latency <- runtime.data[,c("time", "ads.latency", "tcp.latency", "udp.latency")]
names(latency) <- legends
latency <- melt(latency, id.vars="time")
latency$value <- latency$value / 1000

## Now we break it into two parts
l <- latency
split <- 1

l$value[which(l$value > split)[1]] <- 0.99
l$value[which(l$value > split)[1]] <- 1.01
l$value[rev(which(l$value > split))[1]] <- 0.99
l$value[rev(which(l$value > split))[1]] <- 1.01
l$value[l$time == 400 & l$variable == "Streaming over TCP"] <- NA

l$mask <- 1
l$mask[l$value > split] <- 0

transform <- function(i) log10(i) + split + 1
inverse <- function(i) 10 ^ (i - split - 1)
l$value[l$mask == 0] <- transform(l$value[l$mask == 0])

breaks <- c(0, 0.5, 1.0, transform(1.0), transform(10.0), transform(100))
labels <- breaks
labels[labels > split] <- inverse(labels[labels > split])
labels <- sapply(labels, function(l) if (l == 100) {
                                         "100"
                                     } else {
                                         sprintf("%2.1f", l)
                                     })

latency.plot <- ggplot(l, aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point() +
    geom_line(linetype=2) +
    facet_grid(mask ~ ., scales="free") +
    scale_y_continuous(breaks=breaks, labels=labels, expand=c(0.075,0)) +
    xlab("") +
    ylab("Latency (seconds)") +
    xlim(30, 650) +
    academic_paper_theme() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_color_jco()
latency.plot

##
## accuracy plot
##
accuracy <- runtime.data[,c("time", "ads.accuracy", "tcp.accuracy", "udp.accuracy")]
names(accuracy) <- legends
accuracy <- melt(accuracy, id.vars="time")
accuracy.plot <- ggplot(accuracy, aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point() +
    geom_line(linetype=2) +
    xlab("Time (seconds)") +
    ylab("Accuracy (F1 Score)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 1), labels=function(x) format(x, nsmall=1)) +
    academic_paper_theme() +
    theme(legend.position="none") +
    scale_color_jco()
accuracy.plot

##
## Final plot
##
pdf("runtime-mot-verticle.pdf", width=4, height=6.9)
multiplot(bw.plot + theme(legend.position="none"),
          latency.plot + theme(legend.position="none"),
          accuracy.plot + theme(legend.position="none"),
          cols=1)
dev.off()

pdf("runtime-legend.pdf", width=10, height=2)
bw.plot + ylab("") + theme(legend.position="top",
                           legend.key = element_rect(size = unit(30, 'lines')),
                           legend.key.size = unit(4, 'lines'),
                           legend.title = element_blank())
dev.off()
