setwd("C:/Users/daniel/Dropbox/dissertation/chap3/r")
source("config.r")

#######################

options(stringsAsFactors = FALSE)
lapply(reqs, require, character.only = TRUE)

ppf <- function(...){cat(paste0(...));flush.console();}