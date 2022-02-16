rm(list = ls())
options(scipen = 999)

library(tidyr)
library(zoo)
library(utils)
library(compositions)

diag <- read.table("diagnostics.txt", header = F, colClasses = 'character')

# Day 3, Part 1
l <- paste0("V",seq_along(rep(1:nchar(diag[1,]))))
diagnostics <- diag %>% separate(V1, l, sep = "(?<=.)") %>% mutate_all(as.numeric)
ones <- colSums(diagnostics)
zeros <- nrow(diagnostics) - ones
unbinary(paste0(ifelse(ones>zeros,1,0),collapse = "")) * unbinary(paste0(ifelse(ones<zeros,1,0),collapse = ""))
# 4191876

# Day 3, Part 2
mostcommon <- function (v, iteration) {
  freq <- plyr::count(iteration[v])
  iter <- iteration %>% filter(iteration[v]==max(freq[1][freq[2]==max(freq[2])]))
}
leastcommon <- function (v, iteration) {
  freq <- plyr::count(iteration[v])
  iter <- iteration %>% filter(iteration[v]==min(freq[1][freq[2]==min(freq[2])]))
}

most <- list()
most[[1]] <- diagnostics
for (i in seq_along(diagnostics)) {
  most[[i+1]] <- mostcommon(i, most[[i]])
  most[[i]] <- most[[i+1]]
}

least <- list()
least[[1]] <- diagnostics
for (i in seq_along(diagnostics)) {
  least[[i+1]] <- leastcommon(i, least[[i]])
  least[[i]] <- least[[i+1]]
}

unbinary(paste0(most[[12]],collapse = "")) * unbinary(paste0(least[[12]],collapse = ""))
# 3414905