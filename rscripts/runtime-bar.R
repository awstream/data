#!/usr/bin/env Rscript

source('prelude.R')

f <- path("runtime/darknet.runtime.csv")
data <- read.csv(f)

variable <- c("Time", "JetStream++", "JetStream")
levels <- c("AwStream", "JetStream++", "JetStream")

latency.columns <- c("time", "jet.latency", "js.latency")
accuracy.columns <- c("time", "jet.accuracy", "js.accuracy")

################################
##
## Draw Latency
##
################################
latency <- data[data$time > 205 & data$time < 435, latency.columns]
names(latency) <- variable
latency.data <- melt(latency, id="Time")

latency.plot <- ggplot(latency.data, aes(x=value, colour=variable)) +
    stat_ecdf(size=1) +
    scale_x_log10() +
    xlab("Latency (ms)") +
    ylab("CDF") +
    theme(legend.position="top") +
    scale_color_jco() +
    theme_bw(base_size = 20) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
latency.plot

x
################################
##
## Draw Accuracy
##
################################
accuracy <- data[data$time > 205 & data$time < 435, accuracy.columns]
names(accuracy) <- variable
accuracy.data <- melt(accuracy, id="Time")

accuracy.plot <- ggplot(accuracy.data, aes(x=value, colour=variable)) +
    stat_ecdf(size=1) +
    xlab("Accuracy (F1)") +
    ylab("CDF") +
    theme(legend.position="none") +
    scale_color_jco()
accuracy.plot

################################
##
## Combine final plot
##
################################
legend <- get_legend(latency.plot)
figure <- plot_grid(latency.plot + theme(legend.position="none"),
                    NULL,
                    accuracy.plot + theme(legend.position="none"),
                    align = 'vh',
                    hjust = -1,
                    nrow = 1,
                    rel_widths = c(1, .1, 1)
                    )

plot_grid(legend, figure, ncol=1, rel_heights = c(0.2, 1))
