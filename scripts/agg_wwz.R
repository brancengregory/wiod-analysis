library(magrittr)
library(tidyr)
library(dplyr)
library(readr)

#wwz <- read_csv("~/vagrant_shared/output/wwz_2000.csv") 

wwz <- list()
mex2all <- list()
usa2all <- list()
mex2usa <- list()
usa2mex <- list()

years = 2000:2014
for (i in 1:length(years)) {
  wwz[[i]] <- read_csv(paste("~/vagrant_shared/output/wwz_",as.character(years[i]),".csv",sep = ""))
  names(wwz)[i] <- years[i]

  wwz[[i]] <- wwz[[i]] %>%
    mutate(DVA = .$DVA_FIN + .$DVA_INT + .$DVA_INTrexI1 + .$DVA_INTrexF + .$DVA_INTrexI2) %>%
    mutate(RDV = .$RDV_INT + .$RDV_FIN + .$RDV_FIN2) %>%
    mutate(FVA = .$OVA_FIN + .$MVA_FIN + .$OVA_INT + .$MVA_INT) %>%
    mutate(PDC = .$DDC_FIN + .$DDC_INT + .$ODC + .$MDC)
}
  
for (i in 1:length(years)) {
  mex2all[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "MEX")
  names(mex2all)[i] <- years[i]
  usa2all[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "USA")
  names(usa2all)[i] <- years[i]
  mex2usa[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "MEX") %>%
    filter(.$Importing_Country == "USA")
  names(mex2usa)[i] <- years[i]
  usa2mex[[i]] <- wwz[[i]] %>%
    filter(.$Exporting_Country == "USA") %>%
    filter(.$Importing_Country == "MEX")
  names(usa2mex)[i] <- years[i]
}

#mex2all_agg_bucket <- lapply(mex2all, function(x) colSums(x[c(30:33)]))
#usa2all_agg_bucket <- lapply(usa2all, function(x) colSums(x[c(30:33)]))
#mex2usa_agg_bucket <- lapply(mex2usa, function(x) colSums(x[c(30:33)]))
#usa2mex_agg_bucket <- lapply(usa2mex, function(x) colSums(x[c(30:33)]))

#write.csv(mex2all_agg_bucket, file = "~/vagrant_shared/final_output/mex2all_agg_bucket.csv")
#write.csv(usa2all_agg_bucket, file = "~/vagrant_shared/final_output/usa2all_agg_bucket.csv")
#write.csv(mex2usa_agg_bucket, file = "~/vagrant_shared/final_output/mex2usa_agg_bucket.csv")
#write.csv(usa2mex_agg_bucket, file = "~/vagrant_shared/final_output/usa2mex_agg_bucket.csv")


#mex2all_agg_component <- lapply(mex2all, function(x) colSums(x[c(4:19)]))
#usa2all_agg_component <- lapply(usa2all, function(x) colSums(x[c(4:19)]))
#mex2usa_agg_component <- lapply(mex2usa, function(x) colSums(x[c(4:19)]))
#usa2mex_agg_component <- lapply(usa2mex, function(x) colSums(x[c(4:19)]))

#write.csv(mex2all_agg_component, file = "~/vagrant_shared/final_output/mex2all_agg_component.csv")
#write.csv(usa2all_agg_component, file = "~/vagrant_shared/final_output/usa2all_agg_component.csv")
#write.csv(mex2usa_agg_component, file = "~/vagrant_shared/final_output/mex2usa_agg_component.csv")
#write.csv(usa2mex_agg_component, file = "~/vagrant_shared/final_output/usa2mex_agg_component.csv")
