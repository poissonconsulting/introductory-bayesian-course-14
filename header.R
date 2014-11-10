library(dplyr)
library(ggplot2)
library(scales)
library(jaggernaut)

graphics.off()
rm(list = ls())

if (getDoParWorkers() == 1) {
  registerDoParallel(3)
  opts_jagr(parallel = TRUE)
}
