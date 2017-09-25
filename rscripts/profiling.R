##
## get pareto
##
library(rPref)
source("prelude.R")

merged <- read.csv(path("offline.darknet.csv"))

dedup <- function(df) {
    df <- df[order(-df$bandwidth, -df$accuracy, df$width, df$skip, df$quant),]
    df[!duplicated(cbind(df$accuracyp, df$bandwidth)),]
}

final.merged <- dedup(merged)
pareto <- psel(merged, low(bandwidth) * high(accuracy))
dp <- dedup(pareto)

final.pareto <- dp[order(dp$bandwidth),
                   c("bandwidth", "width", "skip", "quant", "accuracy")]

final.pareto$bw <- ceiling(final.pareto$bandwidth / 100) * 100
final.final <- final.pareto[!duplicated(final.pareto$bw),]
final.final$bw <- NULL
# write.csv(final.final, "darknet.profile.csv", row.names=FALSE, quote=FALSE)

##
## get references (three lines)
##
no.degrade.1 <- merged[merged$width == 1920 & merged$skip == 0,]
no.degrade.1$label <- "tune quantizer"
no.degrade.2 <- merged[merged$width == 1920 & merged$quant == 0,]
no.degrade.2$label <- "tune framerate"
no.degrade.3 <- merged[merged$skip == 0 & merged$quant == 0,]
no.degrade.3$label <- "tune resolution"

pareto$label <- "Pareto boundary"
reference <- rbind(no.degrade.1, no.degrade.2, no.degrade.3, pareto)

##
## plotting
##
p <- ggplot(merged, aes(x = bandwidth, y = accuracy)) +
    geom_point(shape = 4, alpha = 0.5, size = 1, colour = "#56B4E9") +
    geom_line(data=reference, aes(x=bandwidth, y=accuracy, color=factor(label)),
              linetype="dashed") +
    geom_point(data=reference, aes(x=bandwidth, y=accuracy, color=factor(label),
                                   shape=factor(label)), size=1.8) +
    scale_x_log10() +
    ylim(0, 1) +
    xlab("Bandwidth (Mbps)") +
    ylab("Accuracy (F1 Score)") +
    theme(legend.margin=margin(0, 0, -3, -30),
          legend.text = element_text(size=12),
          legend.background = element_rect(fill = "white"),
          legend.key.height=unit(0.9, "line"),
          legend.key.size = unit(2, 'lines'),
          legend.title = element_blank(),
          axis.title.x = element_text(size=14, margin=margin(15, 0, 0, 0)),
          axis.title.y = element_text(size=14, margin=margin(0, 15, 0, 0))) +
    guides(col = guide_legend(ncol = 2)) +
    scale_color_jco()
## Test plot
## dev.new(width=3.5, height=3.3)
p

pdf("profile-darknet.pdf", width=4, height=3)
p
dev.off()
