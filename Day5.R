setwd("~/Advent Code")
rm(list = ls())
library(plyr)
library(tidyverse)

test <- read.csv("vent_test.txt",header=F)
test <- test %>% separate(V2, c("y1","x2")) %>% mutate_all(as.numeric)
colnames(test) <- c("xstart","ystart","xend","yend")
horvert <- test %>% subset(xstart==xend | ystart==yend)

# Switch test for input
vents <- read.csv("vents.txt", header = F)
vents <- vents %>% separate(V2, c("y1","x2")) %>% mutate_all(as.numeric)
colnames(vents) <- c("xstart","ystart","xend","yend")
hvents <- vents %>% subset(xstart==xend | ystart==yend)

# Day 1
plot(NA, xlim=c(0,max(hvents$xend)), ylim=c(max(hvents$yend),0), xlab="x", ylab="y")
axis(1, at = seq(1,9))
axis(2, at = seq(1,9))
segments(hvents$xstart, hvents$ystart, hvents$xend, hvents$yend)

findintersections <- function(vents) {
  line_coords <- list()
  for (i in seq(nrow(vents))){
    line_coords[[i]] <- data.frame(cbind(seq(vents[i,1],vents[i,3]),seq(vents[i,2],vents[i,4])))
  }
  intersections <- data.frame()
  for (i in seq(length(line_coords))) {
    if (i < length(line_coords)) {j <- i + 1
    } else { j <- length(line_coords) }
    other_coords <- line_coords[j:length(line_coords)]
    for (k in seq(length(other_coords))) {
      output <- join(line_coords[[i]],other_coords[[k]], type = "inner")
      names(output) <- c("V1","V2")
      #print(output)
      if(nrow(output)==0) { 
        intersections <- rbind(intersections,c(NA,NA))
        names(intersections) <- c("V1","V2")
      } else {
        intersections <- rbind(intersections,output)
      }
    }
  }
  sub<-nrow(intersections)-nrow(other_coords[[1]])
  intersections <- intersections[1:sub,]
  nrow(unique(intersections %>% filter(!is.na(V1))))
}
findintersections(hvents)
# 6666

# Day 2; rerun function over vents which includes diagonals
plot(NA, xlim=c(0,max(vents$xend)), ylim=c(max(vents$yend),0), xlab="x", ylab="y")
axis(1, at = seq(1,9))
axis(2, at = seq(1,9))
segments(vents$xstart, vents$ystart, vents$xend, vents$yend)

findintersections(vents)
# 19081