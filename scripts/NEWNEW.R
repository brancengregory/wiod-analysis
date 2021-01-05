library(tidyverse)
library(igraph)
#library()

load("~/Documents/wiod-analysis/data/wiot_r_Nov16/WIOT2014_October16_ROW.RData")

summary(wiot$Year)

wiot %>%
  filter(.$Country=="USA") %>%
  filter(.$Year==2014) %>%
  select(num_range("USA", 1:56)) %>%
  as.matrix() %>%
  image()

net <- wiot %>%
  filter(.$Country=="USA") %>%
  filter(.$Year==2014) %>%
  select(num_range("USA", 1:56)) %>%
  as.matrix() %>%
  graph.adjacency(mode = "directed", diag = F)

plot.igraph(net)
            