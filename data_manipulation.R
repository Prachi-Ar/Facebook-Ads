rm(list=ls())
setwd("~/Desktop/fb_ads")

library(tidyr)
library(dplyr)
library(fuzzyjoin)
library(stringr)

# week1 <- read.csv("week1.csv") %>%
#   select(-X.1)
# 
# week2 <- read.csv("week2.csv")%>%
#   select(-X.1)
# 
# week3 <- read.csv("week3.csv")%>%
#   select(-X.1)
# 
# week4 <- read.csv("week4.csv")%>%
#   select(-X.1)
# 
# week5 <- read.csv("week5.csv")%>%
#   select(-X.1)
# 
# week6 <- read.csv("week6.csv")%>%
#   select(-X.1)
# 
# other <- read.csv("other.csv")%>%
#   select(-X.1)
# 

# 
# ads <- rbind(week1, week2, week3, week4, week5, week6, other)

# Reading candidates data as presented on the AEC website

candidates <- read.csv("candidates_edited.csv") %>%
  select(-fullName.1) %>%
  mutate(fullName_lower = str_to_lower(fullName))

# Reading raw ads data pulled from Facebook API

ads <- read.csv("ads_raw.csv") %>% 
  select(-X) %>%
  mutate(page_name_lower = str_to_lower(page_name))


# for (i in nrow(candidates)){
#   ads$fullName <- ifelse(grepl(toString(candidates$fullName[i]), ads$page_name), candidates$fullName[i], NA )
# }

ads$page_name<- as.character(ads$page_name)
candidates$fullName <- as.character(candidates$fullName)
# ads$partyBallotName<- as.character(ads$partyBallotName)

# Adding fullName_lower row to the ads dataframe based on fullName provided in candidates based on lower case names

ads$fullName_lower = str_extract(
  ads$page_name_lower,
  pattern = regex(paste(candidates$fullName_lower, collapse = "|"), ignore_case = TRUE)
)


ads <- left_join(ads, candidates, by = "fullName_lower", all.x = TRUE)
ads$party <- ads$partyBallotName

# manual tagging of parties

ads$party[grepl("Labor", ads$page_name, fixed = TRUE) & is.na(ads$party)] <- "Labor"

ads$party[grepl("A.L.P.", ads$party, fixed = TRUE)] <- "Labor"

ads$party[grepl("Labor", ads$party, fixed = TRUE)] <- "Labor"

ads$party[grepl("Liberal National", ads$page_name, fixed = TRUE) & is.na(ads$party)] <- "Liberal National Party"

ads$party[grepl("Liberal Democrat", ads$page_name, fixed = TRUE) & is.na(ads$party)] <- "Liberal Democrats"

ads$party[grepl("Liberal", ads$page_name, fixed = TRUE) & is.na(ads$party)] <- "Liberal"


ads$party[grepl("Green", ads$party, fixed = TRUE)] <- "Greens"
ads$party[grepl("Green", ads$page_name, fixed = TRUE)] <- "Greens"

ads$party[grepl("Country Liberal Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "NT CLP"

ads$party[grepl("TNL", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "TNL"

ads$party[grepl("One Nation", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Pauline Hanson's One Nation"

ads$party[grepl("Nationals", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "The Nationals"
ads$party[grepl("National Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "The Nationals"
ads$party[grepl("nationals", ads$page_name_lower, fixed = TRUE) & is.na("ads$party")] <- "The Nationals"
ads$party[grepl("sa nationals", ads$page_name_lower, fixed = TRUE) & is.na("ads$party")] <- "The Nationals"


ads$party[grepl("Australian Federation Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Australian Federation Party"

ads$party[grepl("United Australia Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "United Australia Party"

ads$party[grepl("UAP", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "United Australia Party"
ads$party[grepl("united australia party cowper candidate - joshua fairhall", ads$page_name_lower, fixed = TRUE)] <- "United Australia Party"
ads$party[grepl("united australia party", ads$page_name_lower, fixed = TRUE)] <- "United Australia Party"

ads$party[grepl("Centre Alliance", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Centre Alliance"

ads$party[grepl("victorian socialists", ads$page_name_lower, fixed = TRUE) & is.na("ads$party")] <- "Victorian Socialists"

ads$party[grepl("australian democrats", ads$page_name_lower, fixed = TRUE)] <- "Australian Democrats"
ads$party[grepl("citizens party", ads$page_name_lower, fixed = TRUE)] <- "Citizens Party"


ads$party[grepl("western australia party", ads$page_name_lower, fixed = TRUE)] <- "WESTERN AUSTRALIA PARTY"
ads$party[grepl("western australia party for durack", ads$page_name_lower, fixed = TRUE)] <- "WESTERN AUSTRALIA PARTY"

ads$party[grepl("Animal Justice Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Animal Justice Party"
ads$party[grepl("animal justice party queensland", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"
ads$party[grepl("animal justice party ajp tasmania", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"
ads$party[grepl("animal justice party ajp western australia", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"
ads$party[grepl("animal justice party nsw", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"
ads$party[grepl("animal justice party south australia", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"
ads$party[grepl("animal justice party ajp victoria", ads$page_name_lower, fixed = TRUE)] <- "Animal Justice Party"


ads$party[grepl("Reason", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Reason Australia"

ads$party[grepl("sustainable australia", ads$page_name_lower, fixed = TRUE) & is.na("ads$party")] <- "Sustainable Australia Party - Stop Overdevelopment / Corruption"

ads$party[grepl("Liberal/The Nationals", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Liberal"

ads$party[grepl("Local Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "The Local Party"
ads$party[grepl("the local party", ads$page_name_lower, fixed = TRUE)] <- "The Local Party"


ads$party[grepl("Democratic Alliance", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Democratic Alliance"

ads$party[grepl("AustralianValues", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Australian Values Party"

ads$party[grepl("Great Australian Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "The Great Australian Party"

ads$party[grepl("FUSION: Science, Pirate, Secular, Climate Emergency", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "FUSION: Science, Pirate, Secular, Climate Emergency"
ads$party[grepl("fusion", ads$page_name_lower, fixed = TRUE) & is.na("ads$party")] <- "FUSION: Science, Pirate, Secular, Climate Emergency"
ads$party[grepl("vote 1 fusion party", ads$page_name_lower, fixed = TRUE)] <- "FUSION: Science, Pirate, Secular, Climate Emergency"
ads$party[grepl("fusion party australia", ads$page_name_lower, fixed = TRUE)] <- "FUSION: Science, Pirate, Secular, Climate Emergency"

ads$party[grepl("Socialist Alliance", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Socialist Alliance"
ads$party[grepl("socialist alliance fnq", ads$page_name_lower, fixed = TRUE)] <- "Socialist Alliance"
ads$party[grepl("socialist alliance wa", ads$page_name_lower, fixed = TRUE)] <- "Socialist Alliance"


ads$party[grepl("Katter's Australian Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Katter's Australian Party (KAP)"
ads$party[grepl("katter's australian party", ads$page_name_lower, fixed = TRUE)] <- "Katter's Australian Party (KAP)"

ads$party[grepl("ICAC", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Federal ICAC Now"

ads$party[grepl("Informed Medical Options Party", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Informed Medical Options Party"

ads$party[grepl("IMOP", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Informed Medical Options Party"

ads$party[grepl("Progressives", ads$page_name, fixed = TRUE) & is.na("ads$party")] <- "Australian Progressives"

# ------------------------------------------------------------------------------------------------------------------------------------
# page type - whether candidate or party 

ads <- ads %>% mutate(page_type = ifelse(!is.na(ads$party), ifelse(!is.na(ads$fullName), "Candidate", "Party"), "Other"))


# keep re-running this as you edit the list of party for pages and the candidates/parties that own them 

ads <- ads %>% mutate(ad_type = ifelse(!is.na(ads$party), "affiliated", "unaffiliated"))

ads_unaffiliated <- ads %>% filter(ad_type =="unaffiliated")

