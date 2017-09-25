## This file merges multiple profileX.csv files and produces the Pareto.
## It's used to generate the profile figure in the paper!

source("prelude.R")
library(latex2exp)
library(rPref)

##
## Merge individual files
##
load <- function(i) {
    f <- path(paste('offline.topk/profile', i, '.csv', sep=''))
    d <- read.csv(f, header=FALSE)
    names(d) <- c("bandwidth", "n", "t", "accuracy")
    d
}

merge_profile <- function(df1, df2) {
    merged <- merge(df1, df2, by = c("n", "t"), all = TRUE)
    merged$bandwidth <- rowSums(cbind(merged$bandwidth.x, merged$bandwidth.y))
    merged$accuracy <- rowSums(cbind(merged$accuracy.x, merged$accuracy.y))
    merged$bandwidth.x <- NULL
    merged$bandwidth.y <- NULL
    merged$accuracy.x <- NULL
    merged$accuracy.y <- NULL

    merged[order(-merged$bandwidth),]
}

merge_multiple <- function(...) {
    ## Make a list from the ... arguments and plotlist
    profiles <- c(list(...))
    num_profiles <- length(profiles)
    merged <- profiles[[1]]
    for (i in 2:num_profiles) {
        merged <- merge_profile(merged, profiles[[i]]);
    }
    merged$bandwidth <- merged$bandwidth / num_profiles
    merged$accuracy <- merged$accuracy / num_profiles
    merged
}

merged <- merge_multiple(load(1), load(2), load(3), load(4), load(5),
                         load(6), load(7), load(8), load(9), load(10))

##
## get pareto
##
library(rPref)

dedup <- function(df) {
    df <- df[order(-df$bandwidth, -df$accuracy, df$n, df$t),]
    df[!duplicated(cbind(df$accuracy, df$bandwidth)),]
}

final.merged <- dedup(merged)
pareto <- psel(merged, low(bandwidth) * high(accuracy))
dp <- dedup(pareto)
final.pareto <- dp[order(dp$bandwidth),
                   c("bandwidth", "n", "t", "accuracy")]

final.pareto$bw <- ceiling(final.pareto$bandwidth / 100) * 100
final.final <- final.pareto[!duplicated(final.pareto$bw),]
final.final$bw <- NULL
## write.csv(final.final, "topk-profile.csv", row.names=FALSE, quote=FALSE)

##
## get references (three lines)
##
no.degrade.1 <- merged[merged$t == 0,]
no.degrade.1$label <- "tune N"
no.degrade.2 <- merged[merged$n == 9950,]
no.degrade.2$label <- "tune T"
pareto$label <- "Pareto boundary"
reference <- rbind(no.degrade.1, no.degrade.2, pareto)

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
    ylim(0.4, 1) +
    xlab("Bandwidth (Kbps)") +
    ylab(TeX("Accuracy (Kendall's $\\tau$)")) +
    theme(legend.margin=margin(0, 0, -3, -30),
          legend.position="top",
          legend.text = element_text(size=12),
          legend.background = element_rect(fill = "white"),
          legend.key.height=unit(0.9, "line"),
          legend.key.size = unit(2, 'lines'),
          legend.title = element_blank(),
          axis.title.x = element_text(size=14, margin=margin(15, 0, 0, 0)),
          axis.title.y = element_text(size=14, margin=margin(0, 15, 0, 0))) +
    guides(col = guide_legend(ncol = 2)) +
    scale_color_jco()
p

pdf("profile-topk.pdf", width=4, height=3)
p
dev.off()
