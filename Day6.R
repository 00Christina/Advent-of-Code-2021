rm(list = ls())
memory.limit(100000)
options(scipen = 999)

day0 <- c(3,4,3,1,2)
day18 <- c(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8)
length(day18)

# Part 1
day1<- day0-1
day2<- if(any(day1==0)) {replace(day1-1,day1 == 0,6)} else {day1-1}
day2<- if(any(day1==0)) {c(day2,rep(8,sum(day1==0)))}
day3<- if(any(day2==0)) {replace(day2-1,day2 == 0,6)}
day3<- if(any(day2==0)) {c(day3,rep(8,sum(day2==0)))}
day4<- if(any(day3==0)) {replace(day3-1,day3 == 0,6)}
day4<- if(any(day3==0)) {c(day4,rep(8,sum(day3==0)))}

day0 <- c(2,4,1,5,1,3,1,1,5,2,2,5,4,2,1,2,5,3,2,4,1,3,5,3,1,3,1,3,5,4,1,1,1,1,5,
          1,2,5,5,5,2,3,4,1,1,1,2,1,4,1,3,2,1,4,3,1,4,1,5,4,5,1,4,1,2,2,3,1,1,1,
          2,5,1,1,1,2,1,1,2,2,1,4,3,3,1,1,1,2,1,2,5,4,1,4,3,1,5,5,1,3,1,5,1,5,2,
          4,5,1,2,1,1,5,4,1,1,4,5,3,1,4,5,1,3,2,2,1,1,1,4,5,2,2,5,1,4,5,2,1,1,5,
          3,1,1,1,3,1,2,3,3,1,4,3,1,2,3,1,4,2,1,2,5,4,2,5,4,1,1,2,1,2,4,3,3,1,1,
          5,1,1,1,1,1,3,1,4,1,4,1,2,3,5,1,2,5,4,5,4,1,3,1,4,3,1,2,2,2,1,5,1,1,1,
          3,2,1,3,5,2,1,1,4,4,3,5,3,5,1,4,3,1,3,5,1,3,4,1,2,5,2,1,5,4,3,4,1,3,3,
          5,1,1,3,5,3,3,4,3,5,5,1,4,1,1,3,5,5,1,5,4,4,1,3,1,1,1,1,3,2,1,2,3,1,5,
          1,1,1,4,3,1,1,1,1,1,1,1,1,1,2,1,1,2,5,3)
day <- list()
day[[1]] <- day0-1
no_days <- 80
for(i in seq_along(1:no_days)) {
  day[[i+1]]<- if(any(day[[i]]==0)) {replace(day[[i]]-1,day[[i]] == 0,6)} else {day[[i]]-1}
  day[[i+1]]<- if(any(day[[i]]==0)) {c(day[[i+1]],rep(8,sum(day[[i]]==0)))} else {day[[i+1]]}
}
length(day[[80]]) # 362666

# Part 2 (inelegant brute force method, but relatively quick with chunking)
no_days <- 256/2 # Halfway for each chunk
for(i in seq_along(1:no_days)) {
  day[[i+1]]<- if(any(day[[i]]==0)) {replace(day[[i]]-1,day[[i]] == 0,6)} else {day[[i]]-1}
  day[[i+1]]<- if(any(day[[i]]==0)) {c(day[[i+1]],rep(8,sum(day[[i]]==0)))} else {day[[i+1]]}
}
length(day[[128]]) # 23704306
day128 <- c(length(which(day[[128]]==0)),length(which(day[[128]]==1)),length(which(day[[128]]==2)),
              length(which(day[[128]]==3)),length(which(day[[128]]==4)),length(which(day[[128]]==5)),
              length(which(day[[128]]==6)),length(which(day[[128]]==7)),length(which(day[[128]]==8)))

day256 <- vector()
for(j in 0:8) {
day <- list()
day[[1]] <- j
for(i in seq_along(1:no_days)) {
  day[[i+1]]<- if(any(day[[i]]==0)) {replace(day[[i]]-1,day[[i]] == 0,6)} else {day[[i]]-1}
  day[[i+1]]<- if(any(day[[i]]==0)) {c(day[[i+1]],rep(8,sum(day[[i]]==0)))} else {day[[i+1]]}
}
day256[j+1] <- length(day[[length(day)]])
}
# For each by day 256 to be multiplied by number on day 128, 0: 94508; each 1: 90763;
# each 2: 79321; each 3: 75638; each 4: 67616; each 5: 62475; each 6: 58016;
# each 7: 51564; each 8: 49380
sum(as.numeric(day128)*as.numeric(day256))
# 1640526601595