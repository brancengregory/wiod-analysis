library(magrittr)
library(tidyr)
library(dplyr)
library(readr)
library(knitr)
library(xtable)

wwz <- read_csv("~/vagrant_shared/output/wwz_2000.csv") 

wwz <- list()
mex2usa <- list()
usa2mex <- list()

years = 2000:2014
for (i in 1:length(years)) {
  wwz[[i]] <- read_csv(paste("~/vagrant_shared/output/wwz_",as.character(years[i]),".csv",sep = ""))
  names(wwz)[i] <- paste("wwz_", as.character(years[i]), sep = "")

  wwz[[i]] <- wwz[[i]] %>%
    mutate(DVA = .$DVA_FIN + .$DVA_INT + .$DVA_INTrexI1 + .$DVA_INTrexF + .$DVA_INTrexI2) %>%
    mutate(RDV = .$RDV_INT + .$RDV_FIN + .$RDV_FIN2) %>%
    mutate(FVA = .$OVA_FIN + .$MVA_FIN + .$OVA_INT + .$MVA_INT) %>%
    mutate(PDC = .$DDC_FIN + .$DDC_INT + .$ODC + .$MDC)
}
  
for (i in 1:length(years)) {
  mex2usa[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "MEX") %>%
    filter(.$Importing_Country == "USA")
  names(mex2usa)[i] <- paste("mex2usa_", as.character(years[i]), sep = "")
  usa2mex[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "USA") %>%
    filter(.$Importing_Country == "MEX")
  names(usa2mex)[i] <- paste("usa2mex_", as.character(years[i]), sep = "")
}

test <- lapply(mex2usa, function(x) x[c(1,2,3,30:33)])
X <- xtable(mex2usa[[1]])
test_dva <- lapply(mex2usa, function(x) colSums(x[c(30:33)]))
write.csv(test_dva, file = "~/vagrant_shared/output/test_dva.csv")

test_grossexp <- lapply(mex2usa, function(x) colSums(x[20]))

