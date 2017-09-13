#!/usr/bin/env Rscript

source('prelude.R')

f <- path("darknet.runtime.csv")
data <- read.csv(f)

variable <- c("Time", "JetStream++", "JetStream", "Streaming over TCP",
              "AWStream")

levels <- c("AWStream", "JetStream++", "JetStream", "Streaming over TCP")
levels <- factor(levels, levels=levels)

latency.columns <- c("time", "jet.latency", "js.latency",
                     "tcp.latency", "aws.latency")
accuracy.columns <- c("time", "jet.accuracy", "js.accuracy",
                      "tcp.accuracy", "aws.accuracy")
throughput.columns <- c("time", "jet.throughput", "js.throughput",
                        "tcp.throughput", "aws.throughput")


latency <- data[data$time > 205 & data$time < 380, latency.columns]

l <- c(mean(latency$aws.latency), mean(latency$jet.latency),
       mean(latency$js.latency), mean(latency$tcp.latency))
df1 <- data.frame(levels, l)
df1$label <- "mean"

l <- c(quantile(latency$aws.latency, c(0.99))[[1]],
       quantile(latency$jet.latency, c(0.99))[[1]],
       quantile(latency$js.latency, c(0.99))[[1]],
       quantile(latency$tcp.latency, c(0.99))[[1]])
df2 <- data.frame(levels, l)
df2$label <- "p99"

latency.combined <- rbind(df1, df2)
latency.combined$l <- latency.combined$l / 1000

require(scales)
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base),
            domain = c(base^from, Inf))
}

latency.plot <- ggplot(latency.combined, aes(x=label, y=l, fill=levels)) +
    geom_bar(width = 0.9,
             position=position_dodge(0.9), stat="identity") +
    geom_text(aes(label=sprintf("%0.2f", l)),
              vjust=-0.4, hjust=1, angle=-45, size=5,
              position=position_dodge(.9)) +
    ylab("Latency\n(seconds)") +
    scale_y_continuous(trans = mylog_trans(base=10, from=-2),
                       expand=c(0, 0), limits=c(NA, 1000),
                       breaks=c(0.1, 1, 10, 100),
                       labels=c("0.1", "1", "10", "100")) +
    xlab("") +
    theme(legend.position="top") +
    theme(legend.title=element_blank()) +
    scale_fill_jco()
latency.plot

##
## Accuracy
##

accuracy <- data[data$time > 205 & data$time < 380, accuracy.columns]

l <- c(mean(accuracy$aws.accuracy), mean(accuracy$jet.accuracy),
       mean(accuracy$js.accuracy), mean(accuracy$tcp.accuracy))
df1 <- data.frame(levels, l)
df1$label <- "mean"

l <- c(quantile(accuracy$aws.accuracy, c(0.01))[[1]],
       quantile(accuracy$jet.accuracy, c(0.01))[[1]],
       quantile(accuracy$js.accuracy, c(0.01))[[1]],
       quantile(accuracy$tcp.accuracy, c(0.01))[[1]])
df2 <- data.frame(levels, l)
df2$label <- "p99"

accuracy.combined <- rbind(df1, df2)

accuracy.plot <- ggplot(accuracy.combined, aes(x=label, y=l, fill=levels)) +
    geom_bar(width = 0.9,
             position=position_dodge(.9), stat="identity") +
    geom_text(aes(label=sprintf("%0.2f", l)),
              vjust=-0.4, hjust=1, angle=-45, size=5,
              position=position_dodge(.9)) +
    ylab("Accuracy\n(F1 score)") +
    scale_y_continuous(expand=c(0, 0), limits=c(0, 1.15),
                       breaks=c(0, 0.5, 1), labels=c("0", "0.5", "1.0")) +
    xlab("") +
    theme(legend.position="top") +
    scale_fill_jco()
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
                    rel_widths = c(1, .05, 1)
                    )

p <- plot_grid(legend, figure, ncol=1, rel_heights = c(0.2, 1))
p

ggsave("runtime_darknet-bar.pdf", p, width=8, height=4)
