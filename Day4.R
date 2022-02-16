rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(zoo)
library(utils)
library(compositions)
library(stringr)

calls <- read.csv("bingo_draw.txt", header = F, sep = ",")
boards <- read.csv("bingo_boards.txt", header = F)

# Day 4, Part 1
boardlist <- boards %>% mutate(across(where(is.character), str_replace_all, "\\s+", " "), across(where(is.character),trimws)) %>%
  separate(V1, c("V1", "V2", "V3", "V4", "V5")) %>% mutate_all(as.numeric)
# Separate boardlist into list of dataframes
all_boards <- rep(list(),20)
y <- 0
for(i in seq(1,nrow(boardlist),5)) {
  y = y+1
  all_boards[[y]] <- boardlist[i:(i+4), ]
}
# Find numbers from bingo draw vector %in% each bingo board
# Identify which one has 5 sequential numbers called (vertical, horizontal, NOT diagonal) first
bingo <- rep(T, 5)
count <- 0
match <- FALSE
all_cards <- list()
all_checks <- list()
while(match!=TRUE) {
  for (j in seq_along(calls)) {
    for (i in seq_along(all_boards)) {
      count <- count + 1
      card <- as.matrix(all_boards[[i]])
      check <- matrix(card %in% calls[1:j],5,5)
      match <- ifelse(sum(check[1,])==sum(bingo)||sum(check[2,])==sum(bingo)||sum(check[3,])==sum(bingo)||
                        sum(check[4,])==sum(bingo)||sum(check[5,])==sum(bingo)||
                        sum(check[,1])==sum(bingo)||sum(check[,2])==sum(bingo)||sum(check[,3])==sum(bingo)||
                        sum(check[,4])==sum(bingo)||sum(check[,5])==sum(bingo),#||
                      #sum(c(check[1,1],check[2,2],check[3,3],check[4,4],check[5,5]))==sum(bingo)||
                      #sum(c(check[1,5],check[2,4],check[3,3],check[4,2],check[5,1]))==sum(bingo),
                      TRUE,FALSE)
      print(match)
      if (match==TRUE) print(card)
      all_cards[[i]] <- card
      all_checks[[i]] <- check
      if (match==TRUE) break
    }
    if (match==TRUE) break
  }}
first_bingo <- c(all_cards[[i]])
which_last_call <- calls[j]
sum(first_bingo[which(!(first_bingo %in% calls[1:j]))])*which_last_call
# 23177

# Day 4, Part 2 (this is a less elegant answer that I had to come back to after some time)
match <- FALSE
matches <- vector()
for (i in seq_along(all_boards)) {
  card <- as.matrix(all_boards[[i]])
  check <- matrix(card %in% calls[1:i],5,5)
  match <- ifelse(sum(check[1,])==sum(bingo)||sum(check[2,])==sum(bingo)||sum(check[3,])==sum(bingo)||
                    sum(check[4,])==sum(bingo)||sum(check[5,])==sum(bingo)||
                    sum(check[,1])==sum(bingo)||sum(check[,2])==sum(bingo)||sum(check[,3])==sum(bingo)||
                    sum(check[,4])==sum(bingo)||sum(check[,5])==sum(bingo),
                  TRUE,FALSE)
  print(match)
  matches[i] <- match
}
which_last_rev_call <- length(all_boards)-last(which(matches==F))
for (i in seq_along(all_boards)) {
  card <- as.matrix(all_boards[[i]])
  check <- matrix(card %in% calls[1:(which(calls %in% which_last_rev_call)-1)],5,5)
  match <- ifelse(sum(check[1,])==sum(bingo)||sum(check[2,])==sum(bingo)||sum(check[3,])==sum(bingo)||
                    sum(check[4,])==sum(bingo)||sum(check[5,])==sum(bingo)||
                    sum(check[,1])==sum(bingo)||sum(check[,2])==sum(bingo)||sum(check[,3])==sum(bingo)||
                    sum(check[,4])==sum(bingo)||sum(check[,5])==sum(bingo),
                  TRUE,FALSE)
  if (match==FALSE) last_bingo <- card
  if (match==FALSE) last_check <- check
}
last_bingo <- c(last_bingo)
sum(last_bingo[which(!(last_bingo %in% calls[1:which(calls%in%which_last_rev_call)]))])*which_last_rev_call
# 6804