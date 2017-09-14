#!/usr/bin/env Rscript
library(ellipse)
source("prelude.R")

data <- read.csv(path("runtime.darknet.csv"))
data <- data[data$time > 205 & data$time < 380, ]

## Start preparing each ellipse

x <- log10(data$tcp.latency)
y <- data$tcp.accuracy
point <- data.frame(x=x, y=y, g="TCP")
center <- data.frame(x=median(x),y=median(y), g="TCP")
tcp.center <- c(median(x), median(y))
tcp.ell <- ellipse(cor(x, y), scale=c(sd(x),sd(y)), centre=c(mean(x),mean(y)))
tcp.contour <- cbind(as.data.frame(tcp.ell), group="TCP")

x <- log10(data$aws.latency)
y <- data$aws.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="AWStream"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="AWStream"))
aws.center <- c(median(x), median(y))
aws.ell <- ellipse(cor(x, y), scale=c(sd(x),sd(y)), centre=c(mean(x),mean(y)))
aws.contour <- cbind(as.data.frame(aws.ell), group="AWStream")

x <- jitter(log10(data$udp.latency), factor=2)
y <- jitter(data$udp.accuracy, factor=2)
point <- rbind(point, data.frame(x=x, y=y, g="UDP"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="UDP"))
udp.center <- c(median(x), median(y))
udp.ell <- ellipse(cor(x, y), scale=c(sd(x),0.01), centre=c(mean(x),mean(y)))
udp.contour <- cbind(as.data.frame(udp.ell), group="UDP")

x <- log10(data$js.latency)
y <- data$js.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="JS"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="JS"))
js.center <- c(median(x), median(y))
js.ell <- ellipse(cor(x, y),
                  scale=c(sd(x), sd(y)),
                  centre=c(mean(x),mean(y)))
js.contour <- cbind(as.data.frame(js.ell), group="JS")

df <- data.frame()
df <- rbind(df, tcp.contour, aws.contour, udp.contour, js.contour)

## dev.new(width=5, height=2.6)

p <- ggplot() +
    geom_polygon(data=df, aes(x=x, y=y, fill=group), alpha=0.5) +
    geom_point(data=center, aes(x=x, y=y),
               size=4, shape=21, colour="black", fill="white", stroke=1) +
    scale_x_reverse(breaks=c(5, 4, 3, 2, 1), labels=c(100, 10, 1, 0.1, 0.01)) +
    scale_y_continuous(breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), limits=c(-0.1, 1.1)) +
    xlab("Freshness, latency reversed (seconds)") +
    ylab("Fidelity, accuracy (%)") +
    annotate(geom = "text", x = udp.center[1] + 0.3, y = udp.center[2] + 0.2,
             label = "Streaming", size = 5) +
    annotate(geom = "text", x = udp.center[1] + 0.3, y = udp.center[2] + 0.1,
             label = "over UDP", size = 5) +
    annotate(geom = "text", x = tcp.center[1] + 0.2, y = tcp.center[2] - 0.1,
             label = "Streaming", size = 5) +
    annotate(geom = "text", x = tcp.center[1] + 0.22, y = tcp.center[2] - 0.2,
             label = "over TCP", size = 5) +
    annotate(geom = "text", x = aws.center[1] - 0.2, y = aws.center[2] + 0.1,
             label = "AWStream", size = 5) +
    annotate(geom = "text", x = js.center[1] - 0.2, y = js.center[2] - 0.12,
             label = "Manual", size = 5) +
    geom_segment(
        aes(x = 5.2, y = 0.07, xend = 4.5, yend = 0.28),
        arrow = arrow(length = unit(0.05, "npc"))
    ) +
    annotate(geom = "text", x = 4.95, y = 0.25, label = "Better", size = 6,
             angle = 30) +
    scale_fill_lancet() +
    theme(legend.position="none")
p

ggsave("figure1.pdf", p, width=5.6, height=3.3)
