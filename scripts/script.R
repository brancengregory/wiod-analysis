#install.packages("tidyverse")

#if (!require('devtools')) install.packages('devtools')
#devtools::install_github("bquast/decompr")
#install.packages("OECD")

library(tidyverse)

load("./data/wiot_r_Nov16/WIOT2000_October16_ROW.RData")

# countries <- unique(wiot$Country)
# countries <- countries[!countries %in% "TOT"]
# years <- rep(2000,44)

##Get Single-Country IOT

# niot_USA_2000 <- wiot %>%
#   filter(.$Country=="USA" & .$Year==2000) %>%
#   select(num_range("USA",1:56)) %>%
#   as.matrix()
# 
# niot_MEX_2000 <- wiot %>%
#   filter(.$Country=="MEX" & .$Year==2000) %>%
#   select(num_range("MEX",1:56)) %>%
#   as.matrix()
# 
# ##Use of MEX BY USA
# iiot_USA_MEX_2000 <- wiot %>%
#   filter(.$Country=="USA" & .$Year==2000) %>%
#   select(num_range("MEX",1:56)) %>%
#   as.matrix()

niot <- function(cntry1,yr){
  x <- wiot %>%
    filter(.$Country==cntry1 & .$Year==yr) %>%
    select(num_range(cntry1,1:56)) %>%
    as.matrix()
  return(x)
}
iiot <- function(cntry1,cntry2,yr){
  x <- wiot %>%
    filter(.$Country==cntry1 & .$Year==yr) %>%
    select(num_range(cntry2,1:56)) %>%
    as.matrix()
  return(x)
}
bigaggWIOThoriz <- function(cntry1,countriesX,yr){
  country_list <- countriesX
  x <- list()
  for(i in 1:length(country_list)){
    x[[i]] <- wiot %>%
      filter(.$Country==cntry1 & .$Year==yr) %>%
      select(num_range(country_list[i],1:56)) %>%
      as.matrix()
  }
  y <- Reduce("+",x)
  return(y)
}
bigaggWIOTvert <- function(countriesX,yr){
  country_list <- countriesX
  x <- list()
  for(i in 1:length(country_list)){
    x[[i]] <- bigaggWIOThoriz(country_list[i],country_list,yr)
  }
  z <- Reduce("+",x)
  return(z)
}

aggWIOT <- function(cntry1,cntry2,yr){
  countries <- unique(wiot$Country)
  countries <- countries[!countries %in% "TOT"]
  countriesX <- countries[!countries %in% c(cntry1,cntry2)]
  ##Domestic IOTs
  dom1 <- niot(cntry1,yr)
  dom2 <- niot(cntry2,yr)
  ##Cntry1 Use of Cntry 2 IIOT
  imp1 <- iiot(cntry2,cntry1,yr)
  ##Cntry2 Use of Cntry 1 IIOT
  imp2 <- iiot(cntry1,cntry2,yr)
  ##Cntry1 Use of ROW IIOT
  x <- list()
  for(i in 1:length(countriesX)){
    x[[i]] <- wiot %>%
      filter(.$Country==countriesX[i] & .$Year==yr) %>%
      select(num_range(cntry1,1:56)) %>%
      as.matrix()
  }
  imp3 <- Reduce("+",x)
  ##Cntry2 Use of ROW IIOT
  x <- list()
  for(i in 1:length(countriesX)){
    x[[i]] <- wiot %>%
      filter(.$Country==countriesX[i] & .$Year==yr) %>%
      select(num_range(cntry1,1:56)) %>%
      as.matrix()
  }
  imp4 <- Reduce("+",x)
  ##ROW Use of Cntry1
  x <- list()
  for(i in 1:length(countriesX)){
    x[[i]] <- wiot %>%
      filter(.$Country==cntry1 & .$Year==yr) %>%
      select(num_range(countriesX[i],1:56)) %>%
      as.matrix()
  }
  imp5 <- Reduce("+",x)
  ##ROW Use of Cntry2
  x <- list()
  for(i in 1:length(countriesX)){
    x[[i]] <- wiot %>%
      filter(.$Country==cntry2 & .$Year==yr) %>%
      select(num_range(countriesX[i],1:56)) %>%
      as.matrix()
  }
  imp6 <- Reduce("+",x)
  ##ROW Use of ROW
  imp7 <- bigaggWIOTvert(countriesX,yr)
  
  return(list(dom1=dom1,dom2=dom2,imp1=imp1,imp2=imp2,imp3=imp3,imp4=imp4,imp5=imp5,imp6=imp6,imp7=imp7))
}


# diagonals <- list()
# for(i in 1:length(countries)){
#   diagonals[[i]] <- niot(countries[i],2000)
# }
# 
# wiot[,5:61]
# ##Cols: 5:2469
# ##Rows: 1:2464

test <- aggWIOT("USA","MEX",2000)

test_IOT_mat <- test$dom2
test_IOT_mat <- rbind(test_IOT_mat,test$imp2)
test_IOT_mat <- rbind(test_IOT_mat,test$imp4)
test_IOT_mat2 <- test$imp1
test_IOT_mat2 <- rbind(test_IOT_mat2,test$dom1)
test_IOT_mat2 <- rbind(test_IOT_mat2,test$imp3)
test_IOT_mat3 <- test$imp6
test_IOT_mat3 <- rbind(test_IOT_mat3,test$imp5)
test_IOT_mat3 <- rbind(test_IOT_mat3,test$imp7)
test_IOT <- cbind(test_IOT_mat,test_IOT_mat2,test_IOT_mat3)

write.csv(test_IOT,file = "./iot.csv",row.names = F,col.names = F)

# test2 <- wiot %>%
#   select(starts_with("MEX",1:56))
# test2[2465:2472,]

sum(test$dom2[,1],test$imp2[,1])
sum(test$imp8[,1])
