#!/usr/bin/env Rscript

source('prelude.R')

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

    per.second$accuracy <- data$accuracy[data$client == 1]

    per.second
}

others <- read.csv(path("runtime/mot.others.csv"))

### Append aws
aws <- read.log.and.group.by.time("mot.aws.log")
aws$time <- NULL
names(aws) <- c("aws.latency", "aws.throughput", "aws.accuracy")
all <- cbind(others, aws)

### Append TCP
tcp <- read.log.and.group.by.time("mot.tcp.log")
tcp$time <- NULL
names(tcp) <- c("tcp.latency", "tcp.throughput", "tcp.accuracy")
all <- cbind(all, tcp)

write.csv(all, file=path("mot.runtime.csv"), row.names=FALSE)
