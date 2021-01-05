library(tidyverse)
library(wiod.diagrammer)
library(networkD3)

W <- loadWIOD('./data/wiot_r_Nov16/WIOT2014_October16_ROW.RData')
W_flat <- flatWIOD(W)

W_flat[, domestic :=   # creating column in-place, following data.table's semantics
         ExpCountry == ImpCountry]
# Let's get rid of self-produced intermediate consumption
W_flat_noself <- W_flat[!(domestic &
                            ExpSectorNr == ImpSectorNr)]
# Let's keep only intra-EU trade
COUNTRIES_DT <- countries()
EU_COUNTRIES <-
  COUNTRIES_DT$Country[COUNTRIES_DT$isEUmember]
# Let's keep only flows >= 1 billion USD for clarity
W_flat_noself_truncated <- W_flat_noself[value >= 1000]  # original WIOD data is in million USD


TOP_CUSTOMERS <-
  findLinks(partners = 'users',
            flat_wiod = W_flat_noself_truncated,
            start_countries = 'USA', # We could add here other countries.
            start_sectors = 1, # "Manufacture of motor vehicles, trailers and semi-trailers"
            # We could add here other sectors.
            ListOfselectionFuns = # As discussed in the text above:
              list(function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 3),  # minus because we
                   function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 2),  # want to rank from
                   function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 1)), # the highest to the lowest
            by = c('domestic','ExpCountry','ExpSectorNr')) # as discussed above


nodes <- tibble(name = unique(c(paste0(TOP_CUSTOMERS$ExpCountry, TOP_CUSTOMERS$ExpSectorNr), paste0(TOP_CUSTOMERS$ImpCountry, TOP_CUSTOMERS$ImpSectorNr))))
source <- NULL
target <- NULL
for (i in 1:length(TOP_CUSTOMERS$value)) {
  source[i] <- which(nodes$name == paste0(TOP_CUSTOMERS$ExpCountry[i], TOP_CUSTOMERS$ExpSectorNr[i])) - 1
  target[i] <- which(nodes$name == paste0(TOP_CUSTOMERS$ImpCountry[i], TOP_CUSTOMERS$ImpSectorNr[i])) - 1
}
links <- tibble(source = source, target = target, value = TOP_CUSTOMERS$value)
rm(list = c("source", "target", "i"))


sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "$", fontSize = 12, nodeWidth = 30)

TOP_SUPPLIERS <-
  findLinks(partners = 'suppliers',
            flat_wiod = W_flat_noself_truncated,
            start_countries = 'USA', # We could add here other countries.
            start_sectors = 1, # "Manufacture of motor vehicles, trailers and semi-trailers"
            # We could add here other sectors.
            ListOfselectionFuns = # As discussed in the text above:
              list(function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 3),  # minus because we
                   function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 2),  # want to rank from
                   function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 1)), # the highest to the lowest
            by = c('domestic','ImpCountry','ImpSectorNr')) # as discussed above


nodes <- tibble(name = unique(c(paste0(TOP_SUPPLIERS$ExpCountry, TOP_SUPPLIERS$ExpSectorNr), paste0(TOP_SUPPLIERS$ImpCountry, TOP_SUPPLIERS$ImpSectorNr))))
source <- NULL
target <- NULL
for (i in 1:length(TOP_SUPPLIERS$value)) {
  source[i] <- which(nodes$name == paste0(TOP_SUPPLIERS$ExpCountry[i], TOP_SUPPLIERS$ExpSectorNr[i])) - 1
  target[i] <- which(nodes$name == paste0(TOP_SUPPLIERS$ImpCountry[i], TOP_SUPPLIERS$ImpSectorNr[i])) - 1
}
links <- tibble(source = source, target = target, value = TOP_SUPPLIERS$value)
rm(list = c("source", "target", "i"))

sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "$", fontSize = 12, nodeWidth = 10)

# plotLinks(top_links_dt = TOP_SUPPLIERS,
#           wiot = wiot, # this is necessary
#           specificNodeOptionsFun =  # this is optional, just to show-off:
#             function(country_sector_dt)
#               ifelse(country_sector_dt$Country=='USA',
#                      'style=filled, fillcolor=cadetblue1', ""))
