#!/usr/bin/env Rscript
## Prelude code for all subsequent data analysis and plotting

library(cowplot)
library(ggsci)
library(reshape2)

script.dir <- getwd()
data.dir <- paste(dirname(script.dir), "data", sep='/')

path <- function(filename) {
    paste(data.dir, filename, sep='/')
}
