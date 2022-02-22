rm(list = ls())
library(stringr)
library(zoo)
library(EcotoneFinder)
library(dplyr)
library(raster)
library(rgeos)
options(scipen = 999)
#cave <- read.csv("cavetest.txt",header=F)
cave <- read.fwf("cave.txt",rep(1,100))

heightmap <- cave %>% mutate_all(as.numeric)
rownames(heightmap) <- seq(1:nrow(heightmap))

# Source code for which.peaks: https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>0,TRUE))>0)
    }else {
      which(diff(diff(x)>0)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)+1
    }
  }
}

by_row <- data.frame(matrix(data=NA,nrow=nrow(heightmap),ncol=length(heightmap)))
colorder <- colnames(heightmap)
colnames(by_row) <- colorder

column <- list()
for (i in 1:length(heightmap)) {
  column[[i]] <- replace(by_row[,i],which.peaks(-heightmap[,i]),heightmap[,i][which.peaks(-heightmap[,i])])
}
columns <- data.frame(do.call(cbind,column))
colnames(columns) <- colorder

rows <- list()
for (i in 1:nrow(heightmap)) {
rows[[i]] <- merge(by_row[i,], heightmap[i,][which.peaks(as.numeric(-heightmap[i,]))], all.y = T,
                    by=colnames(by_row)[which(colnames(by_row) %in% colnames(heightmap[i,][which.peaks(as.numeric(-heightmap[i,]))]))])
rows[[i]] <- rows[[i]][, colorder]
}

by_rows <- do.call(rbind, rows)

same <- list()
for (i in 1:length(columns)){
  common <- cbind(columns[i],by_rows[i])
  same[[i]] <- common[1][complete.cases(common),]
}
combined <- unlist(same)
combined <- combined[-which(combined==9)] # Make sure there's no remaining 9s
sum(combined+1)
# 516

## Part 2
# Source code for combining cells into polygons and calculating area: 
# https://stackoverflow.com/questions/20659186/combining-polygons-and-calculating-their-area-i-e-number-of-cells-in-r
basins <- heightmap
basins[basins==9] <-NA
basins[basins==0] <-1
map <- list()
map$x <- seq(length(basins))
map$y <- seq(length(basins))
map$z <- as.matrix(basins)
m <- raster(map)
plot(m)
mclus <- clump(m, directions = 4, gaps = TRUE)
mareas <- na.omit(transform(data.frame(freq(mclus)),area = count*prod(res(mclus))))
tail(sort(mareas$area),3)
prod(tail(sort(mareas$area),3))
# 1023660