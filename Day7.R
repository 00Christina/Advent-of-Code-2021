rm(list = ls())
#crabstest <- c(16,1,2,0,4,2,7,1,2,14)
crabs <- as.numeric(read.csv("crabs.txt",header = F,sep = ",")[1,])
horizon <- seq(min(crabs),max(crabs))

# Part 1
distance <- vector()
for (i in seq(horizon)) {
  output <- sum(abs(crabs-horizon[i]))
  distance[i] <- output
}
min(distance) # 339321

# Part 2
distance <- vector()
  for (i in seq(horizon)) {
    for (k in seq(crabs)) {
    output[k] <- sum(seq_len(abs(crabs[k]-horizon[i])))
    }
    distance[i] <- sum(output)
  }
min(distance) # 95476244