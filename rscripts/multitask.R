library(cowplot)
library(reshape2)
library(ggsci)

names <- c("client", "time", "bw", "latency", "acc")

create_plot <- function(prefix) {
    ## prefix <- "multitask/eq-acc-"
    ## prefix <- "multitask/eq-bw-"
    f1 <- path(paste(prefix, "darknet.log", sep=''))
    eq.acc.darknet <- read.csv(f1, header=F)
    names(eq.acc.darknet) <- names
    eq.acc.darknet$client <- NULL
    eq.acc.darknet <- eq.acc.darknet[0:40, ]

    f2 <- path(paste(prefix, "mot.log", sep=''))
    eq.acc.mot <- read.csv(f2, header=F)
    names(eq.acc.mot) <- names
    eq.acc.mot$client <- NULL
    eq.acc.mot <- eq.acc.mot[0:40, ]

    combined <- merge(eq.acc.mot, eq.acc.darknet, by="time")
    combined$time <- as.numeric(combined$time) * 5
    combined <- combined[combined$time < 240,]

    ## Test plot
    ## dev.new(width=2.8, height=2.4)

    legends <- c("time", "Pedestrian Detection", "Augmented Reality")

    bw <- combined[,c("time", "bw.x", "bw.y")]
    names(bw) <- legends
    bw <- melt(bw, id.vars="time")
    bw$value <- bw$value / 1000
    bw.plot <- ggplot(bw, aes(x=time, y=value, colour=variable, shape=variable)) +
        geom_point(size=2) +
        geom_line(linetype=2, size=1) +
        xlab(NULL) +
        ylab("Throughput \n (mbps)") +
        scale_x_continuous(breaks = c(0, 100, 200)) +
        scale_y_continuous(limits = c(0, 15),
                           labels=function(x) format(x, nsmall=1)) +
        theme(legend.position="none") +
        theme(plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm")) +
        scale_color_jco()
    bw.plot

    ##
    ## Accuracy
    ##
    acc <- combined[,c("time", "acc.x", "acc.y")]
    names(acc) <- legends
    acc <- melt(acc, id.vars="time")

    acc.plot <- ggplot(acc, aes(x=time, y=value, colour=variable,
                                shape=variable)) +
        geom_smooth(se=FALSE, linetype=2, size=1) +
        ##    geom_line(linetype=2) +
        xlab("Time (seconds)") +
        ylab("Accuracy \n (F1 Score)") +
        scale_x_continuous(breaks = c(0, 100, 200)) +
        scale_y_continuous(limits = c(0.5, 1),
                           labels=function(x) format(x, nsmall=2)) +
        theme(legend.position = "none") +
        theme(plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm")) +
        scale_color_jco()
    acc.plot

    list(bw.plot=bw.plot, acc.plot=acc.plot)
}

eq.acc.plots <- create_plot("multitask/eq-acc-")
eq.bw.plots <- create_plot("multitask/eq-bw-")

## Produce final plots

pcol <- plot_grid(
    eq.bw.plots[[1]] + theme(legend.position="none"),
    eq.bw.plots[[2]] + theme(legend.position="none"),
    align='vh',
    ncol=1)

pdf("multitask-left.pdf", width=4, height=4)
pcol
dev.off()

pcol <- plot_grid(
    eq.acc.plots[[1]] + theme(legend.position="none"),
    eq.acc.plots[[2]] + theme(legend.position="none"),
    align='vh',
    ncol=1)

pdf("multitask-right.pdf", width=4, height=4)
pcol
dev.off()

legend <- get_legend(eq.acc.plots[[1]] + theme(legend.position="top",
                                               legend.title = element_blank()))

p <- plot_grid(legend, pcol, ncol = 1, rel_heights = c(.18, 1))
p


pdf("multitask-legend.pdf", width=7, height=2)
legend <- get_legend(eq.acc.plots[[1]] +
                     theme(legend.position="top",
                           legend.key = element_rect(size = unit(24, 'lines')),
                           legend.key.size = unit(4, 'lines'),
                           legend.title = element_blank()))
grid.draw(legend)
dev.off()
