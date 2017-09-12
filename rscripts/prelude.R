#!/usr/bin/env Rscript
## Prelude code for all subsequent data analysis and plotting

library(cowplot)
library(ggsci)
library(reshape2)

script.dir <- getwd()
data.dir <- paste(dirname(script.dir), "summary-data", sep='/')

path <- function(filename) {
    paste(data.dir, filename, sep='/')
}

academic_paper_theme <- theme_bw(base_size = 20)
theme_set(academic_paper_theme)
