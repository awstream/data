#!/usr/bin/env Rscript

source('prelude.R')

f <- path("darknet.runtime.csv")
data <- read.csv(f)

levels <- c("AWStream", "JetStream++", "JetStream", "Streaming over TCP")
variable <- c("Time", levels)

latency.columns <- c("time", "aws.latency", "jet.latency", "js.latency",
                     "tcp.latency")
accuracy.columns <- c("time", "aws.accuracy", "jet.accuracy", "js.accuracy",
                      "tcp.accuracy")
throughput.columns <- c("time", "aws.throughput", "jet.throughput", "js.throughput",
                        "tcp.throughput")

throughput <- data[data$time > 205 & data$time < 435, throughput.columns]

################################
##
## Draw Latency
##
################################
latency <- data[data$time > 205 & data$time < 380, latency.columns]
names(latency) <- variable
latency.data <- melt(latency, id="Time")
latency.data$variable <- factor(latency.data$variable, levels=levels)

latency.plot <- ggplot(latency.data,
                       aes(x=value, colour=variable, linetype=variable)) +
    stat_ecdf(size=1.5) +
    scale_x_log10(breaks=c(10, 100, 1000, 10000),
                  labels=c(10, 100, 1000, 10000)) +
    xlab("Latency (ms)") +
    ylab("CDF") +
    theme(legend.title=element_blank(),
          legend.spacing.x=unit(3, "lines"),
          legend.key.width=unit(3, "line")) +
    scale_color_jco()
latency.plot

################################
##
## Draw Accuracy
##
################################
accuracy <- data[data$time > 205 & data$time < 380, accuracy.columns]
names(accuracy) <- variable
accuracy.data <- melt(accuracy, id="Time")
accuracy.data$variable <- factor(accuracy.data$variable, levels=levels)

accuracy.plot <- ggplot(accuracy.data,
                        aes(x=value, colour=variable, linetype=variable)) +
    stat_ecdf(size=1.5) +
    xlab("Accuracy (F1)") +
    xlim(0.5, 1) +
    ylab("CDF") +
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

p <- plot_grid(legend, figure, ncol=1, rel_heights = c(0.2, 1))
p

ggsave("runtime_darknet-cdf.pdf", p, width = 8, height = 4)
