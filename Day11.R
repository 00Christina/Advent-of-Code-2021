rm(list = ls())
library(dplyr)
library(raster)
#small <- matrix(c(1,1,1,1,1,1,9,9,9,1,1,9,1,9,1,1,9,9,9,1,1,1,1,1,1), ncol=5,nrow=5)
#dumbos <- data.frame(read.csv("octopuses_test.txt",header=F))
dumbos <- data.frame(read.csv("octopuses.txt",header=F))
dumbos <- dumbos %>% separate(V1,
          c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),sep = "(?<=.)") %>%
          mutate_all(as.numeric)

octopi <- list()
octopi$x=seq(length((dumbos)))
octopi$y=seq(length((dumbos)))
octopi$z=as.matrix(t(apply(dumbos,2,rev)))
octomap <- raster(octopi)

step <- list()
step[[1]] <- octomap
flashes <- 0
steps <- 101 #100th step including 0

synch <- function(steps) {
for (j in 2:steps) {
  step[[j]] <- step[[j-1]]+1 
  while(any(values(step[[j]])>9,na.rm=T)) {
    midstep <- adjacent(step[[j]], cell=which(values(step[[j]])>9),
                        directions=8,sorted=TRUE)
    values(step[[j]]) <- ifelse(values(step[[j]])>9,NA,values(step[[j]]))
      for(i in 1:nrow(midstep)) {
        values(step[[j]])<-ifelse(1:ncell(step[[j]])%in%midstep[i,],
                                  values(step[[j]]+1),values(step[[j]]))
      }
  }
  flashes[j] <- flashes[j-1] + length(which(is.na(values(step[[j]]))))
  step[[j]] <- step[[j]] %>% replace_na(.,0)
}
  assign("flashes", flashes, envir=globalenv())
  assign("step", step, envir=globalenv())
}
synch(steps)
plot(step[[length(step)]])
text(step[[length(step)]])
flashes[length(flashes)] #100th step
# 1686

## Part 2
diff_flashes <- (flashes-lag(flashes))[-1]

while(any(diff_flashes==100)==0) {
  steps <- steps + 10
  synch(steps)
  diff_flashes <- (flashes-lag(flashes))[-1]
}
which(diff_flashes==100) # Identifies step when all 100 octopuses have flashed
# 360