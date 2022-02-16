rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(zoo)

submarine <- read.csv("sub.txt", header = F)

# Day 2, Part 1
course <- submarine %>% separate(V1, c('direction','value')) %>% mutate(value=as.numeric(value))
moves <- course %>% group_by(direction) %>% summarize(Frequency = sum(value))
moves[2,2] * (moves[1,2]-moves[3,2])
# 2150351

# Day 2, Part 2
course$change <- 0
course$change <-ifelse(course$direction=="forward",course$change,
                       ifelse(course$direction=="up",lag(course$change, default=0)-course$value,course$value+lag(course$change,default=0)))
course$aim <- cumsum(course$change)
course$depth <- ifelse(course$direction=="forward",course$value*lag(course$aim),
                       course$aim)
sum(course$depth[course$direction=="forward"], na.rm = T) * sum(course$value[course$direction=="forward"])
# 1842742223