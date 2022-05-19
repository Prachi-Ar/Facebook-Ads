setwd("~/Desktop/fb_ads")

install.packages("repmis")

library(dplyr)

data <- read.csv("pulled_15_05.csv")

class(data$ad_delivery_start_time)

data$ad_delivery_start_time <- as.Date(data$ad_delivery_start_time, format = "%Y-%m-%d")   

class(data$ad_delivery_start_time)

week1start <- as.Date("2022-04-11", format = "%Y-%m-%d")
week1end <- as.Date("2022-04-17", format = "%Y-%m-%d")
week2start <- as.Date("2022-04-18", format = "%Y-%m-%d")
week2end <- as.Date("2022-04-24", format = "%Y-%m-%d")
week3start <- as.Date("2022-04-30", format = "%Y-%m-%d")
week3end <- as.Date("2022-05-06", format = "%Y-%m-%d")
week4start <- as.Date("2022-05-07", format = "%Y-%m-%d")
week4end <- as.Date("2022-05-13", format = "%Y-%m-%d")
week5start <- as.Date("2022-05-14", format = "%Y-%m-%d")
week5end <- as.Date("2022-05-20", format = "%Y-%m-%d")

week1 <- data[data$ad_delivery_start_time >= week1start &    # Extract data frame subset
                   data$ad_delivery_start_time <= week1end, ]

week2 <- data[data$ad_delivery_start_time >= week2start &    # Extract data frame subset
                data$ad_delivery_start_time <= week2end, ]

week3 <- data[data$ad_delivery_start_time >= week3start &    # Extract data frame subset
                data$ad_delivery_start_time <= week3end, ]

week4 <- data[data$ad_delivery_start_time >= week4start &    # Extract data frame subset
                data$ad_delivery_start_time <= week4end, ]

data_weeks <- rbind(week1, week2, week3, week4)

data$id <- as.character(data$id)
data_weeks$id <- as.character(data_weeks$id)

other_15_05 <- setdiff(data, data_weeks)



write.csv(week1, "week1.csv")
write.csv(week2, "week2.csv")
write.csv(week3, "week3.csv")
write.csv(week4, "week4.csv")
write.csv(other_15_05, "other_15_05.csv")