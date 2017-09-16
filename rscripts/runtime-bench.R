#!/usr/bin/env Rscript

source('prelude.R')

read.log <- function(logname, suffix) {
    f <- raw.path(logname)
    raw.data <- read.table(f)

    time <- as.integer(as.POSIXct(paste(raw.data$V1, raw.data$V2, sep=' ')))
    time <- time - time[1]
    latency <- raw.data$V12
    accuracy <- raw.data$V15
    throughput <- raw.data$V9 / 1000

    df <- as.data.frame(cbind(time, latency, accuracy, throughput))
    names(df) <- c("time",
                   paste("latency", suffix, sep=''),
                   paste("accuracy", suffix, sep=''),
                   paste("throughput", suffix, sep=''))
    df[df$time > 40 & df$time < 220,]
}

data <- cbind(read.log("netem_0_0.stderr.log", "_0"),
              read.log("netem_50_5.stderr.log", "_50"),
              read.log("netem_100_10.stderr.log", "_100"),
              read.log("netem_150_15.stderr.log", "_150"),
              read.log("netem_200_20.stderr.log", "_200"),
              read.log("netem_250_25.stderr.log", "_250"))
data$time <- data$time

####################################################
##
## Accuracy
##
####################################################
latency.columns <- c("time", "latency_0",
                     "latency_50", "latency_100", "latency_150",
                     "latency_200", "latency_250")
names <- c("time", "0", "50", "100", "150", "200", "250")
levels <- c("0", "50", "100", "150", "200", "250")
latency <- data[,latency.columns]
names(latency) <- names

latency.data <- melt(latency, id="time")
latency.data$variable <- factor(latency.data$variable, levels=levels)

## The code below produces CDF
##
## latency.plot <- ggplot(latency.data,
##                        aes(x=value, colour=variable, linetype=variable)) +
##     stat_ecdf(size=1.2) +
##     scale_x_log10(breaks=c(1, 10, 100, 1000, 10000),
##                   labels=c(1, 10, 100, 1000, 10000)) +
##     xlab("Latency (ms)") +
##     ylab("CDF") +
##     theme(legend.title=element_blank(),
##           legend.spacing.x=unit(3, "lines"),
##           legend.key.width=unit(3, "line")) +
##     guides(colour=guide_legend(nrow=1, byrow=TRUE)) +
##     scale_color_jco()
## latency.plot

####################################################
##
## Accuracy
##
####################################################
accuracy.columns <- c("time", "accuracy_0",
                      "accuracy_50", "accuracy_100", "accuracy_150",
                     "accuracy_200", "accuracy_250")
accuracy <- data[,accuracy.columns]
names(accuracy) <- names
accuracy.data <- melt(accuracy, id="time")
accuracy.data$variable <- factor(accuracy.data$variable, levels=levels)

## The code below produces a CDF plot, which looks nice as well.
##
## accuracy.plot <- ggplot(accuracy.data,
##                        aes(x=value, colour=variable, linetype=variable)) +
##     stat_ecdf(size=1.2) +
##     xlab("Accuracy (ms)") +
##     ylab("CDF") +
##     xlim(0.7, 1.0) +
##     theme(legend.title=element_blank(),
##           legend.spacing.x=unit(3, "lines"),
##           legend.key.width=unit(3, "line")) +
##     scale_color_jco()
## accuracy.plot

################################
##
## Combine final plot
##
################################
figure <- plot_grid(latency.plot + theme(legend.position="none"),
                    NULL,
                    accuracy.plot + theme(legend.position="none"),
                    align = 'vh',
                    hjust = -1,
                    nrow = 1,
                    rel_widths = c(1, .0, 1))
figure

### Other attempts
labels <- c("Latency (ms)", "Accuracy (F1 score)")
labels <- factor(labels, levels=labels)
latency.data$label <- labels[1]
accuracy.data$label <- labels[2]
combined <- rbind(latency.data, accuracy.data)

plot <- ggplot(combined, aes(x=factor(variable), y=value)) +
    geom_boxplot(outlier.size=.5, outlier.alpha = 0.5) +
    facet_grid(. ~ label, scales="free") +
    xlab("Added Network\nDelay (ms)") +
    ylab(NULL) +
    theme(strip.background=element_blank()) +
    theme(strip.text.x=element_text(size = 20)) +
    scale_x_discrete(limits=rev(levels)) +
    coord_flip()
plot

ggsave("runtime_darknet-bench.pdf", plot, width=6.5, height=3)
