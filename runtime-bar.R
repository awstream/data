#!/usr/bin/env Rscript

library(cowplot)
library(reshape2)
library(ggsci)

# data <- read.csv("runtime/darknet.runtime.csv")
data <- read.csv("runtime/mot.runtime.csv")

## Start preparing each ellipse
latency <- data[data$time > 205 & data$time < 435,
                c("time", "ads.latency", "jet.latency")]

variable <- c("AwStream", "JetStream++")

l <- c(mean(latency$ads.latency), mean(latency$jet.latency))
df1 <- data.frame(variable, l)
df1$label <- "mean"

l <- c(quantile(latency$ads.latency, c(0.99))[[1]],
             quantile(latency$jet.latency, c(0.99))[[1]])
df2 <- data.frame(variable, l)
df2$label <- "p99"

latency.combined <- rbind(df1, df2)

latency.plot <- ggplot(latency.combined, aes(x=variable, y=l, fill=label)) +
    geom_bar(width = 0.7, position=position_dodge(.7), stat="identity") +
    geom_text(aes(label=sprintf("%0.0f", round(l, digits = 2))),
              vjust=-.2, size=4, position=position_dodge(.7)) +
    ylab("Latency\n(ms)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1500)) +
    xlab("") +
    academic_paper_theme() +
    theme(legend.position="none") +
    scale_fill_jco()

accuracy <- data[data$time > 205 & data$time < 435,
                 c("time", "ads.accuracy", "jet.accuracy")]
l <- c(mean(accuracy$ads.accuracy), mean(accuracy$jet.accuracy))
df1 <- data.frame(variable, l)
df1$label <- "mean"

l <- c(quantile(accuracy$ads.accuracy, c(0.01))[[1]],
             quantile(accuracy$jet.accuracy, c(0.01))[[1]])
df2 <- data.frame(variable, l)
df2$label <- "p99"

accuracy.combined <- rbind(df1, df2)

accuracy.plot <- ggplot(accuracy.combined, aes(x=variable, y=l, fill=label)) +
    geom_bar(width = 0.7, position=position_dodge(.7), stat="identity") +
    geom_text(aes(label=sprintf("%0.2f", round(l, digits = 2))),
              vjust=-.2, size=4, position=position_dodge(.7)) +
    ylab("Accuracy\n(F1 Score)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
    xlab("") +
    academic_paper_theme() +
    theme(legend.position="none") +
    scale_fill_jco()

## Combine for the final plot
pcol <- plot_grid(latency.plot + theme(legend.position="none"),
                  accuracy.plot + theme(legend.position="none"),
                  align = 'vh', ncol = 2, scale = 0.9)
pcol

legend <- get_legend(latency.plot + theme(legend.position="top",
                                          legend.title = element_blank()))

p <- plot_grid(legend, pcol, ncol = 1, rel_heights = c(.12, 1))
p

pdf("mot-runtime-bar.pdf", width=7, height=2.5)
p
dev.off()
