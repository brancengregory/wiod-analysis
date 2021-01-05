wiod_diagram <- function(year = 2014, flow = "purchasers", country = "USA", sector = 1) {
  require(tidyverse)
  require(networkD3)
  require(wiod.diagrammer)
  file <- paste0("./data/wiot_r_Nov16/WIOT", as.character(year), "_October16_ROW.RData")
  W <- loadWIOD(file)
  W_flat <- flatWIOD(W)
  
  W_flat[, domestic :=   # creating column in-place, following data.table's semantics
           ExpCountry == ImpCountry]
  # Let's get rid of self-produced intermediate consumption
  W_flat_noself <- W_flat[!(domestic &
                              ExpSectorNr == ImpSectorNr)]
  W_flat_noself_truncated <- W_flat_noself[value >= 1000]
  if (flow == "purchasers") {
    TOP_CUSTOMERS <-
      findLinks(partners = 'users',
                flat_wiod = W_flat_noself_truncated,
                start_countries = country, # We could add here other countries.
                start_sectors = sector, # "Manufacture of motor vehicles, trailers and semi-trailers"
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
  } else if (flow == "suppliers") {
    TOP_SUPPLIERS <-
      findLinks(partners = 'suppliers',
                flat_wiod = W_flat_noself_truncated,
                start_countries = country, # We could add here other countries.
                start_sectors = sector, # "Manufacture of motor vehicles, trailers and semi-trailers"
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
  } else {
    return()
  }
  
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                units = "$", fontSize = 12, nodeWidth = 15)
          
}
