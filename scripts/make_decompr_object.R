library(magrittr)
library(dplyr)
library(decompr)
library(gvc)

load("~/vagrant_shared/data/wiot_r_Nov16/WIOT2000_October16_ROW.RData")

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


wwz <- decomp(x=x, y=y, k=k, i=i, o=o, v=v, method = "wwz", verbose = T)

write.csv(wwz,file="./output/wwz_2000.csv")


mex2usa <- wwz %>%
  filter(.$Exporting_Country == "MEX") %>%
  filter(.$Importing_Country == "USA")
usa2mex <- wwz %>%
  filter(.$Exporting_Country == "USA") %>%
  filter(.$Importing_Country == "MEX")

nrca <- nrca(mex2usa)
attr(mex2usa, 'long') <- F
attr(wwz,'long') <- T

G <- length(k)
N <- length(i)
GN <- G * N

rownam <- as.vector(t(outer(k, i, paste, sep= ".")))

fdc <- dim(y)[2]/G

##Need Null Inventory Probably for WIOD (Since Negative Vals)
if (null_inventory == TRUE) {
  y[, fdc * (1:G)] <- 0
}

Bd <- Ad <- matrix(0, nrow = GN, ncol = GN)
Yd <- ESR <- Eint <- Efd <- Y <- matrix(0, nrow = GN, ncol = G)

A <- t(t(x) / o)
A[!is.finite(A)] <- 0
Am <- A

II <- diag(GN)
Bm <- B <- solve(II - A)

for (j in 1:G) {
  m = 1 + (j - 1) * N
  n = N + (j - 1) * N
  
  Ad[m:n, m:n] <- A[m:n, m:n]
  Bd[m:n, m:n] <- B[m:n, m:n]
  
  Bm[m:n, m:n] <- 0
  Am[m:n, m:n] <- 0
}

L <- solve(II - Ad)
Vc <- v/o
Vc[!is.finite(Vc)] <- 0

if (fdc > 1) {
  for (j in 1:G) {
    m <- 1 + (j - 1) * fdc
    n <- fdc + (j - 1) * fdc
    
    Y[, j] <- rowSums(y[, m:n])
  }
} else if(fdc == 1) {
  Y <- y
}

Ym <- Y

E <- cbind(x, y)
for (j in 1:G) {
  m <- 1 + (j - 1) * N
  n <- N + (j - 1) * N
  
  s <- GN + 1 + (j - 1) * fdc
  r <- GN + fdc + (j - 1) * fdc
  
  E[m:n, m:n] <- 0
  E[m:n, s:r] <- 0
  
  Yd[m:n, j] <- Y[m:n, j]
  Ym[m:n, j] <- 0
}

z <- E
E <- as.matrix(rowSums(E))

