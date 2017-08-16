#!/usr/bin/env Rscript

library(reshape2)
library(ggsci)
library(ellipse)

data <- read.csv("figure1.csv")

## Start preparing each ellipse

x <- log10(data$tcp.latency)
y <- data$tcp.accuracy
point <- data.frame(x=x, y=y, g="TCP")
center <- data.frame(x=median(x),y=median(y), g="TCP")
tcp.center <- c(median(x), median(y))
tcp.ell <- ellipse(cor(x, y), scale=c(sd(x),sd(y)), centre=c(mean(x),mean(y)))
tcp.contour <- cbind(as.data.frame(tcp.ell), group="TCP")

x <- log10(data$ads.latency)
y <- data$ads.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="AWStream"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="AWStream"))
ads.center <- c(median(x), median(y))
ads.ell <- ellipse(cor(x, y), scale=c(sd(x),sd(y)), centre=c(mean(x),mean(y)))
ads.contour <- cbind(as.data.frame(ads.ell), group="AWStream")

x <- log10(data$udp.latency)
y <- data$udp.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="UDP"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="UDP"))
udp.center <- c(median(x), median(y))
udp.ell <- ellipse(cor(x, y), scale=c(sd(x),0.01), centre=c(mean(x),mean(y)))
udp.contour <- cbind(as.data.frame(udp.ell), group="UDP")

x <- log10(data$app.latency)
y <- data$app.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="APP"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="APP"))
app.center <- c(median(x), median(y))
app.ell <- ellipse(cor(x, y),
                   scale=c(sd(x), sd(y)),
                   centre=c(mean(x),mean(y)))
app.contour <- cbind(as.data.frame(app.ell), group="APP")

x <- log10(data$mnl.latency)
y <- data$mnl.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="MNL"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="MNL"))
mnl.center <- c(median(x), median(y))
mnl.ell <- ellipse(cor(x, y),
                   scale=c(sd(x), sd(y)),
                   centre=c(mean(x),mean(y)))
mnl.contour <- cbind(as.data.frame(mnl.ell), group="MNL")

x <- log10(data$jet.latency)
y <- data$jet.accuracy
point <- rbind(point, data.frame(x=x, y=y, g="JET"))
center <- rbind(center, data.frame(x=median(x),y=median(y), g="JET"))
jet.center <- c(median(x), median(y))
jet.ell <- ellipse(cor(x, y),
                   scale=c(sd(x), sd(y)),
                   centre=c(mean(x),mean(y)))
jet.contour <- cbind(as.data.frame(jet.ell), group="JET")

df <- data.frame()
df <- rbind(df, tcp.contour, ads.contour, udp.contour, app.contour, mnl.contour, jet.contour)

dev.new(width=5, height=2.6)

p <- ggplot() +
    geom_polygon(data=df, aes(x=x, y=y, fill=group), alpha=0.5) +
    geom_point(data=center, aes(x=x, y=y),
               size=3, shape=21, colour="black", fill="white", stroke=1) +
    scale_x_reverse(breaks=c(5, 4, 3, 2, 1), labels=c(100, 10, 1, 0.1, 0.01)) +
    ylim(0, 1) +
    xlab("Freshness, latency reversed (seconds))") +
    ylab("Fidelity, accuracy (%)") +
    annotate(geom = "text", x = udp.center[1] + 0.3, y = udp.center[2] + 0.2,
             label = "Streaming", size = 4) +
    annotate(geom = "text", x = udp.center[1] + 0.3, y = udp.center[2] + 0.1,
             label = "over UDP", size = 4) +
    annotate(geom = "text", x = tcp.center[1] + 0.2, y = tcp.center[2] - 0.1,
             label = "Streaming", size = 4) +
    annotate(geom = "text", x = tcp.center[1] + 0.2, y = tcp.center[2] - 0.2,
             label = "over TCP", size = 4) +
    annotate(geom = "text", x = ads.center[1] - 0.2, y = ads.center[2] + 0.1,
             label = "AWStream", size = 4) +
    annotate(geom = "text", x = jet.center[1] - 0.4, y = jet.center[2] - 0.1,
             label = "JetStream", size = 4) +
    annotate(geom = "text", x = mnl.center[1] + 0.1, y = mnl.center[2] - 0.1,
             label = "Manual", size = 4) +
    annotate(geom = "text", x = app.center[1] - 0.1, y = app.center[2] - 0.12,
             label = "Application-specific", size = 4) +
    annotate(geom = "text", x = app.center[1] - 0.25, y = app.center[2] - 0.23,
             label = "(a different application)", size = 4) +
    geom_segment(
        aes(x = 5.2, y = 0.07, xend = 4.5, yend = 0.28),
        arrow = arrow(length = unit(0.05, "npc"))
    ) +
    annotate(geom = "text", x = 4.95, y = 0.25, label = "Better", size = 5,
             angle = 30) +
    academic_paper_theme() +
    scale_fill_lancet() +
    theme(legend.position="none")
p

ggsave("figure1a.pdf", p, width=5, height=2.6)
