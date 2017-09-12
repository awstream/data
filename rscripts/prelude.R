#!/usr/bin/env Rscript
## Prelude code for all subsequent data analysis and plotting

library(cowplot)
library(ggsci)
library(reshape2)

script.dir <- getwd()
data.dir <- paste(dirname(script.dir), "summary-data", sep='/')
raw.dir <- paste(dirname(script.dir), "raw-data", sep='/')

path <- function(filename) {
    paste(data.dir, filename, sep='/')
}

raw.path <- function(filename) {
    paste(raw.dir, filename, sep='/')
}

academic_paper_theme <-
    theme_bw(base_size = 20) +
    theme(legend.position="top") +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.border = element_rect(size = 2))
theme_set(academic_paper_theme)
