library(readxl)

WIOD_SEA_Nov16 <- read_excel("~/Downloads/WIOD_SEA_Nov16.xlsx", sheet = "DATA")
View(WIOD_SEA_Nov16)

library(tidyverse)

data <- as.tibble(WIOD_SEA_Nov16)
data <- data %>% mutate(sector = str_extract(data$code, regex("^.")))
data <- data[,c(1:4,20, 5:19)]
data

data %>% filter(country=="USA" & variable=="H_EMPE") %>% select(paste0(2000:2014)) %>% summarise_all(funs(sum))

data %>% filter(country=="USA" & variable=="H_EMPE") %>% select(paste0(2000:2014)) %>% summarise_all(funs(sum)) %>% plot(x=2000:2014, y=., type = "l")

data %>% filter(country=="USA" & variable=="H_EMPE")

data %>% filter(country=="USA" & variable=="H_EMPE") %>% select(paste0(2000:2014)) %>% t() %>% matplot(type="l")
data %>% filter(country=="USA" & variable=="H_EMPE") %>% select(paste0(2000:2014)) %>% log() %>% t() %>% matplot(type="l")


data %>% filter(country=="USA" & variable=="GO") %>% select(paste0(2000:2014)) %>% summarise_all(funs(sum))
data %>% filter(country=="USA" & variable=="GO") %>% select(paste0(2000:2014)) %>% summarise_all(funs(sum)) %>% plot(x=2000:2014, y=., type = "l")

data %>% filter(country=="USA" & variable=="GO")

data %>% filter(country=="USA" & variable=="GO") %>% select(paste0(2000:2014)) %>% t() %>% matplot(type="l")
data %>% filter(country=="USA" & variable=="GO") %>% select(paste0(2000:2014)) %>% log() %>% t() %>% matplot(type="l")



data %>% filter(country=="USA" & variable=="GO") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.)))
usa_go <- data %>% filter(country=="USA" & variable=="GO") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t()
data %>% filter(country=="USA" & variable=="GO") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t() %>% matplot(type = "l")
data %>% filter(country=="USA" & variable=="GO") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% log() %>% t() %>% matplot(type = "l")

data %>% filter(country=="USA" & variable=="EMP") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.)))
usa_emp <- data %>% filter(country=="USA" & variable=="EMP") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t()
data %>% filter(country=="USA" & variable=="EMP") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t() %>% matplot(type = "l")
data %>% filter(country=="USA" & variable=="EMP") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% log() %>% t() %>% matplot(type = "l")

data %>% filter(country=="USA" & variable=="LAB") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.)))
usa_lab <- data %>% filter(country=="USA" & variable=="LAB") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t()
data %>% filter(country=="USA" & variable=="LAB") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% t() %>% matplot(type = "l")
data %>% filter(country=="USA" & variable=="LAB") %>% group_by(sector) %>% summarise_all(funs(if(is.numeric(.)) sum(.) else first(.))) %>% select(paste0(2000:2014)) %>% log() %>% t() %>% matplot(type = "l")
