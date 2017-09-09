#!/usr/bin/env Rscript

library(reshape2)

online <- read.csv("online.csv", header=T)
names <- c("time", "offline", "online", "online (1/10)", "online (trigger)")
online$time <- online$time * 10

bw.ref <- 11

offline.data <- online[,c("time", "OfflineBW")]
names(offline.data) <- c("time", "value")
plot.offline <- ggplot(offline.data, aes(x=time, y=value)) +
    geom_hline(yintercept=bw.ref, linetype="dotdash") +
    geom_point() +
    geom_line(linetype = 2) +
    ylim(0, 25) +
    xlab("time (seconds)") +
    ylab("Bandwidth\n(mbps)") +
    academic_paper_theme()
plot.offline
pdf("online1.pdf", width=2.8, height=1.6)
plot.offline
dev.off()

online.data <- online[,c("time", "OnlineBW")]
names(online.data) <- c("time", "value")
plot.online <- plot.offline %+% online.data
pdf("online2.pdf", width=2.8, height=1.6)
plot.online
dev.off()

online.partial.data <- online[,c("time", "Online_1_10BW")]
names(online.partial.data) <- c("time", "value")
plot.online.partial <- plot.offline %+% online.partial.data
pdf("online3.pdf", width=2.8, height=1.6)
plot.online.partial
dev.off()

online.trigger.data <- online[,c("time", "Online_TriggerBW")]
names(online.trigger.data) <- c("time", "value")
plot.online.trigger <- plot.offline %+% online.trigger.data
pdf("online4.pdf", width=2.8, height=1.6)
plot.online.trigger
dev.off()

## Online Parallelism

lines <-
"GPU,Time,Variable,mask
1,51.91,Offline,0
1,51.91,Online,0
1,12,Offline,1
1,12,Online,1
10,7.62,Offline,1
10,5.2,Online,1
20,4.17,Offline,1
20,2.6,Online,1
30,4.02,Offline,1
30,1.75,Online,1"

parallel <- read.csv(text=lines)
parallel$GPU <- factor(parallel$GPU)
parallel$Time[parallel$mask == 0] <- parallel$Time[parallel$mask == 0] / 7

breaks <- c(0, 5, 10)
labels <- c(0, 5, 10)

p <- ggplot(parallel, aes(x=GPU, y=Time, fill=Variable)) +
    geom_bar(stat="identity", position=position_dodge(.9), width=.8) +
    facet_grid(mask ~ ., scales="free") +
    geom_text(aes(label=sprintf("%0.1f", round(Time, digits = 2))),
              vjust=-.5, size=4, position=position_dodge(.9)) +
    academic_paper_theme() +
    theme(legend.position=c(0.8, 0.8),
          legend.title = element_blank(),
          legend.direction="horizontal") +
    theme(strip.background = element_blank(), strip.text.y = element_blank()) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, 12),
                       breaks=breaks, labels=labels) +
    xlab("#GPUs") +
    ylab("Time\n(seconds)") +
    scale_fill_jco()
p

pdf("parallel.pdf", width=6, height=2)
p
dev.off()

## Then we post-process the figure with Adobe
