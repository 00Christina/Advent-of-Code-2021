rm(list = ls())
#syntax <- data.frame(read.csv("syntax_test.txt",header=F))
syntax <- data.frame(read.csv("syntax_input.txt",header=F))

## Part 1
library(stringr)
library(tidyverse)
# Legend: [=A, ]=a, (=B, )=b, {=C, }=c, <=D, >=d
# Points: )/b: 3 points, ]/a: 57 points, }/c: 1197 points, >/d: 25137 points
alphas <- syntax %>% mutate_all(str_replace_all,"\\[", "A") %>% 
  mutate_all(str_replace_all,"\\]", "a") %>%
  mutate_all(str_replace_all,"\\(","B") %>% mutate_all(str_replace_all,"\\)","b") %>%
  mutate_all(str_replace_all,"\\{","C") %>% mutate_all(str_replace_all,"\\}","c") %>%
  mutate_all(str_replace_all,"\\<","D") %>% mutate_all(str_replace_all,"\\>","d")
alphas <- as.data.frame(alphas)

unpaired <- function(x) {
  x %>% mutate_all(str_remove_all,"Aa") %>%
  mutate_all(str_remove_all,"Bb") %>%
  mutate_all(str_remove_all,"Cc") %>%
  mutate_all(str_remove_all,"Dd")
  }

output <- list()
output[[1]] <- alphas
for (i in seq(nchar(alphas[which.max(nchar(alphas)),])/2)) {
  output[[i+1]] <- unpaired(output[[i]])
}

unmatched <- output[[length(output)]]
mismatch <- unmatched %>% mutate(V2 = str_match(V1, "(.)[:lower:]") %>% .[,1])
mismatch <- mismatch[2] %>% drop_na() %>% mutate_all(str_extract,"[:lower:]")
mismatch <- mismatch %>% mutate(points = ifelse(V2=="a",57,ifelse(V2=="b",3,
            ifelse(V2=="c",1197,25137))))
sum(mismatch$points)
# Test: 26397, Input: 318099

## Part 2
# New points: )/b: 1 point, ]/a: 2 points, }/c: 3 points, >/d: 4 points.
incomplete <- data.frame(unmatched[unmatched %>% mutate_all(str_detect,
              "[:lower:]")==FALSE,])
names(incomplete) <- "unmatched"

# Source code for reverse_chars function: https://www.gastonsanchez.com/r4strings/reversing.html
reverse_chars <- function(string) {
  # split string by characters
  string_split = strsplit(string, split = "")
  # reverse order
  rev_order = nchar(string):1
  # reversed characters
  reversed_chars = string_split[[1]][rev_order]
  # collapse reversed characters
  paste(tolower(reversed_chars), collapse = "")
}

incomplete$matched <- apply(incomplete, 1, reverse_chars)
points <- incomplete[2] %>% mutate_all(str_replace_all,"a", "2") %>% 
  mutate_all(str_replace_all,"b", "1") %>%
  mutate_all(str_replace_all,"c","3") %>% mutate_all(str_replace_all,"d","4")

completions <- as.list(vapply(strsplit(points$matched, ""),
               function(x) paste(x, collapse = ","), character(1L)))
completions <- lapply(completions, function(x) as.numeric(unlist(strsplit(x,","))))

output <- vector()
for (j in 1:length(completions)) {
res <- 0
v <- completions[[j]]
for(i in 1:length(v)) {
  res <- (res*5)+v[i]
}
output[j] <- res
}
median(output)
# 2389738699