library(dplyr)
library(magrittr)
library(readr)

wwz_2000 <- read_csv("~/vagrant_shared/output/wwz_2000.csv")

mex2usa_2000 <- wwz_2000 %>%
  filter(.$Exporting_Country %in% c("MEX")) %>%
  filter(.$Importing_Country %in% c("USA")) %>%
  mutate(Year=rep(2000,56)) %>%
  mutate(DVA_INTrex= DVA_INTrexI1 + DVA_INTrexF + DVA_INTrexI2) %>%
  mutate(RDV= RDV_INT + RDV_FIN + RDV_FIN2) 

usa2mex_2000 <- wwz_2000 %>%
  filter(.$Exporting_Country %in% c("USA")) %>%
  filter(.$Importing_Country %in% c("MEX")) %>%
  mutate(Year=rep(2000,56)) %>%
  mutate(DVA_INTrex= DVA_INTrexI1 + DVA_INTrexF + DVA_INTrexI2) %>%
  mutate(RDV= RDV_INT + RDV_FIN + RDV_FIN2) %>%
  .[,c(1:3,30,4:5,31:32,12:19)]

rm(wwz_2000)

wwz_agg_mex <- rbind(mex2usa_2000,mex2usa_2001,mex2usa_2002,mex2usa_2003,mex2usa_2004,mex2usa_2005,
                     mex2usa_2006,mex2usa_2007,mex2usa_2008,mex2usa_2009,mex2usa_2010,mex2usa_2011,mex2usa_2012,
                     mex2usa_2013,mex2usa_2014)

wwz_agg <- rbind(wwz_agg_mex,wwz_agg_usa)

write.csv(wwz_agg_usa, file="./output/wwz_agg_usa.csv")


