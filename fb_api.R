rm(list=ls())
setwd("~/Desktop/fb_ads")

install.packages('pacman')
install.packages('BiocManager')


library(pacman)
library(BiocManager)

p_load('httr', 'remotes', 'dplyr',
       'ggplot2', 'tidyr', 'Radlibrary', 'dplyr', 'tidyr', 'DT')

library(httr)
library(remotes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# Generating environment variable for key
# usethis::edit_r_environ(scope = "user")

remotes::install_github("facebookresearch/Radlibrary")
library(Radlibrary)

fields_vector <- c("ad_data", "region_data", "demographic_data")

# Correspondingly, save all of the table types.
table_type_vector <- c("ad", "region", "demographic")

# Initiate an empty list to which we will append the extracted API data.
# The list could be initiated simply by using list(); however, especially for
# larger data sets, specifying the length of a list in R in advance speeds up
# the processing. The length of the list equals our 3 data types.
fb_ad_list <- vector(mode = "list", length = length(fields_vector))

# We will also name its three items with values from table_type_vector so we can
# refer to them further
names(fb_ad_list) <- table_type_vector

for (i in seq_along(fields_vector)) {
  print(paste("Extracting the", fields_vector[i]))
  
  query <- adlib_build_query(
    ad_reached_countries = "AU",
    ad_active_status = "ALL",
    search_terms = ".",
    ad_delivery_date_min = "2022-03-21",
    ad_delivery_date_max = "2022-05-20",
    ad_type = "POLITICAL_AND_ISSUE_ADS",
    publisher_platform = c("FACEBOOK", "INSTAGRAM", "MESSENGER", "WHATSAPP"),
    fields = fields_vector[i]
  )
  
  # The call is limited to 1000 results but pagination of overcomes it.
  # We pipe the output of the paginated call to the as_tibble function.
  fb_ad_list[[table_type_vector[i]]] <- adlib_get_paginated(query,
                                                            token = Sys.getenv("fb_token")
  ) %>%
    as_tibble(
      type = table_type_vector[i],
      censor_access_token = TRUE
    )
}

# The demographic & region datasets are in the "long" format (multiple
# rows of information for each ad), and we need a transformation to a "wide" 
# format (single row per ad) of the ad dataset using the tidyr package.

# fb_ad_list[["ad"]] <- fb_ad_list[["ad"]] %>% unnest(c(ad_creative_bodies,
#                                                       ad_creative_link_captions, 
#                                                       ad_creative_link_descriptions,
#                                                       ad_creative_link_titles,
#                                                       languages,
#                                                       publisher_platforms),
#                                                     keep_empty = TRUE)
                                                             

fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(ad_creative_bodies), keep_empty = TRUE)
fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(ad_creative_link_captions), keep_empty = TRUE)
fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(ad_creative_link_descriptions), keep_empty = TRUE)
fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(ad_creative_link_titles), keep_empty = TRUE)
fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(languages), keep_empty = TRUE)
fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% unnest (c(publisher_platforms), keep_empty = TRUE)

fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>%group_by(id)%>%
  mutate(ad_creative_bodies_u = paste(ad_creative_bodies, sep = " "),
         ad_creative_link_captions_u = paste(ad_creative_link_captions, sep = " "),
         ad_creative_link_descriptions_u = paste(ad_creative_link_descriptions, sep = " "),
         ad_creative_link_titles_u = paste(ad_creative_link_titles, sep = " "),
         languages_u = paste(languages, sep = " "),
         publisher_platforms_u = paste(publisher_platforms, sep = " "))

fb_ad_list[['ad']] <- fb_ad_list[['ad']] %>% distinct(id, .keep_all = TRUE)


fb_ad_list[["demographic"]] <- fb_ad_list[["demographic"]] %>% 
  unnest(demographic_distribution, keep_empty = TRUE)

fb_ad_list[["demographic"]] <- fb_ad_list[["demographic"]] %>%
  mutate(age_gender = paste(fb_ad_list[["demographic"]]$age, fb_ad_list[["demographic"]]$gender))

fb_ad_list[["demographic"]] <-fb_ad_list[["demographic"]]%>%
  select(id, percentage, age_gender)


fb_ad_list[["demographic"]]<- pivot_wider(fb_ad_list[["demographic"]],
            id_cols = id,
            names_from = age_gender,
            names_sort = TRUE,
            values_from = percentage)

fb_ad_list[["demographic"]][is.na(fb_ad_list[["demographic"]])] = 0



fb_ad_list[["region"]] <- fb_ad_list[["region"]] %>% 
  unnest(delivery_by_region, keep_empty = TRUE)


# fb_ad_list[["region"]] <- fb_ad_list[["region"]] %>% 
#   +     select("id",
#                +            "Australian Capital Territory", 
#                +            "New South Wales", 
#                +            "Victoria", 
#                +            "Queensland", 
#                +            "South Australia", 
#                +            "Jervis Bay Territory", 
#                +            "Western Australia", 
#                +            "Northern Territory",
#                +            "Tasmania")

fb_ad_list[["region"]] <- pivot_wider(fb_ad_list[["region"]],
                                      id_cols = id,
                                      names_from = region,
                                      names_sort = TRUE,
                                      values_from = percentage)
                                                     
                                      

fb_ad_list[["region"]] <- fb_ad_list[["region"]] %>%
  select("id",
         "Australian Capital Territory",
         "New South Wales",
         "Victoria",
         "Queensland",
         "South Australia",
         "Jervis Bay Territory",
         "Western Australia",
         "Northern Territory",
         "Tasmania")

fb_ad_list[["region"]] <-  unnest(fb_ad_list[["region"]], cols = -id, keep_empty = TRUE)

fb_ad_list[["region"]][is.na(fb_ad_list[["region"]])] = 0

                                     
                                          
# Performing a left join on the common id column across the 3 datasets, remove
# full duplicates and arrange by date.
merged_dataset <- fb_ad_list[["ad"]] %>%
  left_join(fb_ad_list[["demographic"]], by = "id") %>%
  left_join(fb_ad_list[["region"]], by = "id") %>%
  distinct() %>%
  arrange(desc(ad_creation_time))


write.csv(merged_dataset, "ads_raw.csv")
#---------------------------------------------------------




  
  
