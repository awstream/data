#!/usr/bin/env Rscript

source('prelude.R')

f <- path("runtime/darknet.runtime.csv")
runtime.data <- read.csv(f)
legends <- c("time",
             "AwStream",
             "JetStream",
             "JetStream++",
             "Streaming over TCP",
             "Streaming over UDP")

## Create a test plot with proper size
dev.new(width=7, height=2.3)

##
## Bandwidth (throughput)
##
bw <- runtime.data[,c("time", "ads.throughput",
                      "jet.throughput",
                      "tcp.throughput",
                      "udp.throughput")]
names(bw) <- legends
bw <- melt(bw, id.vars="time")
bw$value <- bw$value / 1000

bw.plot <- ggplot(bw, aes(x=time, y=value, colour=variable, shape=variable)) +
    geom_point() +
    geom_line(linetype=2) +
    xlab("") +
    ylab("Throughput\n(mbps)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 25), labels=function(x) format(x, nsmall=1)) +
    academic_paper_theme() +
    scale_color_jco()
bw.plot

##
## latency plot
##
latency <- runtime.data[,c("time", "ads.latency", "jet.latency",
                           "tcp.latency",
                           "udp.latency")]
names(latency) <- legends
latency <- melt(latency, id.vars="time")
latency$value <- latency$value / 1000

## Now we break it into two parts
l <- latency
split <- 2

l$value[which(l$value > split)[1]] <- split - 0.01
l$value[which(l$value > split)[1]] <- split + 0.01
l$value[rev(which(l$value > split))[1]] <- split - 0.01
l$value[rev(which(l$value > split))[1]] <- split + 0.01
l$value[l$time == 435 & l$variable == "Streaming over TCP"] <- NA

l$mask <- 1
l$mask[l$value > split] <- 0

transform <- function(i) log10(i) + split + 1
inverse <- function(i) 10 ^ (i - split - 1)
l$value[l$mask == 0] <- transform(l$value[l$mask == 0])

breaks <- c(0, split / 2, split, transform(split), transform(10.0), transform(100))
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
    ylab("Latency\n(seconds)") +
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
accuracy <- runtime.data[,c("time", "ads.accuracy",
                            "jet.accuracy",
                            "tcp.accuracy",
                            "udp.accuracy" )]
names(accuracy) <- legends
accuracy <- melt(accuracy, id.vars="time")

accuracy.plot <- ggplot(accuracy, aes(x=time, y=value,
                                      colour=variable, shape=variable)) +
    geom_point() +
    geom_line(linetype=2) +
    xlab("Time (seconds)") +
    ylab("Accuracy\n(F1 Score)") +
    xlim(30, 650) +
    scale_y_continuous(limits = c(0, 1), labels=function(x) format(x, nsmall=1)) +
    academic_paper_theme() +
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
#                     guides(colour=guide_legend(nrow=2,byrow=TRUE))


p <- plot_grid(legend, pcol, ncol = 1, rel_heights = c(.14, 1))
p

pdf("runtime-darknet-verticle.pdf", width=7, height=6)
p
dev.off()
