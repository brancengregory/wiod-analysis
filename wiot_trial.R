library(tidyverse)

load("~/Documents/wiod-analysis/data/wiot_r_Nov16/WIOT2014_October16_ROW.RData")

industries <- 1:56
sectors <- 1:20

usa_io <- wiot %>%
  filter(Country=="USA") %>%
  select(starts_with("USA")) %>%
  select(-c("USA57", "USA58", "USA59", "USA60", "USA61")) %>%
  as.matrix()


library()



usa_io_sectoral <- matrix(rep(0, 20*20), nrow = 20, ncol = 20)

for (i in 1:3) {
  for (j in 1:3) {
    usa_io_sectoral[1,1] <- usa_io_sectoral[1,1] + usa_io[i,j]
  }
}

for (i in 1:3) {
  for (j in 4) {
    usa_io_sectoral[1,2] <- usa_io_sectoral[1,2] + usa_io[i,j]
  }
}

for (i in 4) {
  for (j in 1:3) {
    usa_io_sectoral[2,1] <- usa_io_sectoral[2,1] + usa_io[i,j]
  }
}

usa_io_sectoral[2,2] <- usa_io[4,4]

for (i in 5:23) {
  for (j in 1:3) {
    usa_io_sectoral[3,1] <- usa_io_sectoral[3,1] + usa_io[i,j]
  }
}

for (i in 1:3) {
  for (j in 5:23) {
    usa_io_sectoral[1,3] <- usa_io_sectoral[1,3] + usa_io[i,j]
  }
}

for (i in 5:23) {
  for (j in 4) {
    usa_io_sectoral[3,2] <- usa_io_sectoral[3,2] + usa_io[i,j]
  }
}

for (i in 4) {
  for (j in 5:23) {
    usa_io_sectoral[2,3] <- usa_io_sectoral[2,3] + usa_io[i,j]
  }
}

for (i in 5:23) {
  for (j in 5:23) {
    usa_io_sectoral[3,3] <- usa_io_sectoral[3,3] + usa_io[i,j]
  }
}




