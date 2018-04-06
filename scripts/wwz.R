library(magrittr)
library(dplyr)
library(decompr)
library(gvc)

setwd("~/vagrant_shared")
load("./data/wiot_r_Nov16/WIOT2014_October16_ROW.RData")

x <- wiot[1:2464,6:2469]
x <- as.matrix(x)

y <- wiot[-2465:-2472,2470:2689]
y <- as.matrix(y)
##Check code below for zero-ing out change in inventory

v <- wiot[2470,6:2469]
v <- as.numeric(as.vector(v))

o <- wiot[[2690]]
o <- o[1:2464]

k <- unique(wiot$Country)
k <- k[!k %in% "TOT"]

i <- unique(wiot[[1]])[1:56]


wwz_2014 <- decomp(x=x, y=y, k=k, i=i, o=o, v=v, method = "wwz", verbose = T)

write.csv(wwz_2014,file="./output/wwz_2014.csv")