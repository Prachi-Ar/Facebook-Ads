rm(list=ls())
setwd("~/Desktop/fb_ads")

week1 <- read.csv("https://raw.githubusercontent.com/prachiarya0067/facebook_ads/main/week1.csv?token=GHSAT0AAAAAABTLGC4SW3R22C42KGRDW7HGYUFRXGA") %>%
  select(-X.1)

week2 <- read.csv("https://raw.githubusercontent.com/prachiarya0067/facebook_ads/main/week2.csv?token=GHSAT0AAAAAABTLGC4SG4Z6UTN5XNAU7CWKYUFRZFQ")%>%
  select(-X.1)

week3 <- read.csv("https://raw.githubusercontent.com/prachiarya0067/facebook_ads/main/week3.csv?token=GHSAT0AAAAAABTLGC4SXGJXQ6VLC2IOUYU6YUFR2AQ") %>%
  select(-X.1)

week4 <- read.csv("https://raw.githubusercontent.com/prachiarya0067/facebook_ads/main/week4.csv?token=GHSAT0AAAAAABTLGC4SM5ORV5O55KRGP7ASYUFR3CA") %>%
  select(-X.1)

ads <- rbind(week1, week2, week3, week4)