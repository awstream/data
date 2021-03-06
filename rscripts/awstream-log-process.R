#!/usr/bin/env Rscript

source('prelude.R')
library(dplyr)

read.log.and.group.by.time <- function(logname) {
    f <- raw.path(logname)
    raw.data <- read.table(f)

    client <- raw.data$V4
    time <- as.integer(as.POSIXct(paste(raw.data$V1, raw.data$V2, sep=' ')))
    latency <- raw.data$V12
    accuracy <- raw.data$V15
    throughput <- raw.data$V9 / 1000

    data <- as.data.frame(cbind(time, client, latency, accuracy, throughput))

    per.second <- data %>%
        group_by(time) %>%
        summarise(latency=mean(latency), throughput=mean(throughput))

    per.second$accuracy <- data$accuracy[data$client == 2]

    per.second
}

others <- read.csv(path("runtime/mot.others.csv"))

### Append aws
aws <- read.log.and.group.by.time("mot.aws.log")
aws$time <- NULL
names(aws) <- c("aws.latency", "aws.throughput", "aws.accuracy")
until <- min(nrow(others), nrow(aws))
all <- cbind(others[1:until,], aws[1:until,])

### Append TCP
tcp <- read.log.and.group.by.time("mot.tcp.log")
tcp$time <- NULL
names(tcp) <- c("tcp.latency", "tcp.throughput", "tcp.accuracy")
until <- min(nrow(all), nrow(tcp))
all <- cbind(all[1:until,], tcp[1:until,])

### Append UDP
udp <- read.csv(path("runtime/mot.udp.csv"))
udp$time <- NULL
udp$udp.throughput <- udp$udp.throughput / 1000
repeated.udp <- udp[rep(seq_len(nrow(udp)), each=5),]
all <- cbind(all[1:639,], repeated.udp[1:639,])

### Append HLS
hls <- read.csv(path("runtime/mot.hls.csv"))
hls$hls.throughput <- hls$hls.throughput / 1000
all <- cbind(all[1:639,], hls[1:639,])

write.csv(all, file=path("runtime.mot.csv"), row.names=FALSE)
