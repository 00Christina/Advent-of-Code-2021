rm(list = ls())
options(scipen = 999)
library(tidyverse)
library(zoo)

depths <- read.csv("depth.txt",header = F)

# Day 1, Part 1
chg <- depths%>%mutate(Difference=depths-lag(depths))
length(which(chg$Difference>0))
# 1390

# Day 1, Part 2
chg_window <- depths%>%mutate(Difference=depths-lag(depths),roll_3 = rollsum(depths, k=3, fill = NA))
chg_window <- chg_window%>%mutate(diff_3 =chg_window$roll_3-lag(chg_window$roll_3))
length(which(chg_window$diff_3>0))
# 1457