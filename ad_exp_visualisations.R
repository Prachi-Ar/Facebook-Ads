rm(list=ls())
setwd("~/Desktop/fb_ads")

# install.packages('ggplot2')
# install.packages("ggrepel")
# install.packages("showtext")
# install.packages("MetBrewer")
# install.packages("scales")
# install.packages("magrittr")
# install.packages("operators")
# 
# library(knitr)
# library(tidyverse)
# library(showtext)
# library(MetBrewer)
# library(scales)

library(dplyr)

library(tidyverse)

# library(ggplot2)
library(ggrepel)


require(scales)


# font <- "Reem Kufi"
# font_add_google(family=font, font)

ads <- read_csv("ads_cleaned.csv")%>%
  select(-...1)
# calculate average values for impressions, spend, audience size 

# Ad data
ads <- ads %>% mutate(estimated_audience_size_mean = round(rowMeans(ads[,16:17]), -1),
                      impressions_mean = round(rowMeans(ads[,18:19]), -1),
                      spend_mean = round(rowMeans(ads[,20:21]), -1))

ads <- ads %>% mutate(group = ifelse(ads$party=="Liberal" | ads$party =="The Nationals" | ads$party=="Liberal National Party", "Coalition",
                                     ifelse(ads$party=="Labor", "Labor", 
                                            ifelse(ads$party == "Greens", "Greens",
                                                   ifelse(ads$party=="Independent", "Independent",
                                                          ifelse(is.na(ads$party), "Other", "Other"))))))

ads$party <- ifelse(ads$party=="Liberal National Party of Queensland", "Liberal National Party", ads$party)


ads$ad_run_time <- as.numeric(difftime(ads$ad_delivery_stop_time, ads$ad_delivery_start_time, units = "days"))
ads$per_day_spend <- ifelse(ads$ad_run_time > 0, ads$spend_mean/ads$ad_run_time, NA) 

txt_col <- "grey10"
bg <- "#FFFFFF"
grey = "#6a6a6b"


pages <- ads %>%
  filter(page_type == "Candidate",
         state == "ACT")%>%
  select(page_name, spend_mean) %>%
  group_by(page_name) %>%
  dplyr::summarise(page_spend_mean = sum(spend_mean), across()) %>%
  ungroup() %>%
  arrange(desc(page_spend_mean)) %>%
  distinct (page_name, page_spend_mean) %>%
  head(20)
  # pages[order(-pages$page_spend_mean),] %>%
  # distinct (page_name, page_spend_mean) %>%
  # slice(1:15)

pages %>%
  ggplot() +
  geom_bar(aes(x=page_spend_mean, y=reorder(page_name, page_spend_mean), fill="#6D80B0"), stat = "identity") +
  geom_text(aes(x = page_spend_mean, y = page_name, label=dollar(page_spend_mean)), hjust=-0.1, color = txt_col, size = 6)+
  scale_fill_manual(values = rep("#6a6a6b", 8))+
  # scale_fill_gradientn(colours = met.brewer("Demuth", type = "continuous")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,55000),
                     breaks = seq(0,55000,12500),
                     labels = scales::dollar_format()) +
  labs(title = "Top Political Pages on Facebook by Ad Spend: Party Pages",
       subtitle = "How party pages spent on social media advertising",
       x = "Political Ad Spend in AUD",
       y = "Party"
  ) +
  coord_cartesian(clip = "off")+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(), #element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_blank(), #element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none",
  ) 

# Party spend data

# party <- ads %>% select(spend_mean, group) %>%


# party <- ads %>% mutate(group_party = ifelse(ads$party=="Liberal" | ads$party =="The Nationals" | ads$party=="Liberal National Party", "Coalition",
#                                      ifelse(ads$party=="Labor", "Labor", 
#                                             ifelse(ads$party == "Greens", "Greens",
#                                                    ifelse(ads$party=="Independent", "Independent",
#                                                           ifelse(ads$party =="United Australia Party", "United Australia Party",
#                                                                  ifelse(ads$party == "Pauline Hanson's One Nation", "Pauline Hanson's One Nation",
#                                                                         ifelse(is.na(ads$party), "Other", "Other"))))))))

                                     
# party <- party %>% group_by(group_party) %>%
#   summarise(group_sum = sum(spend_mean), across())%>%
#   ungroup()%>%
#   distinct(group_party, group_sum)%>%
#   filter(!is.na(group_party))%>%
#   arrange(desc(group_sum))


party <- ads %>% select(party, spend_mean)%>%
  group_by(party) %>%
  summarise(party_spend = sum(spend_mean), across())%>%
  ungroup()%>%
  arrange(desc(party_spend))%>%
  distinct(party, party_spend) %>%
  head(15)

party %>%
  ggplot() +
  geom_bar(aes(x=party_spend, y=reorder(party, party_spend), fill="#6D80B0"), stat = "identity") +
  geom_text(aes(x = party_spend, y = party, label=dollar(party_spend)), hjust=-0.1, color = txt_col)+
  scale_fill_manual(values = rep("#ad1a27", 8))+
  # scale_fill_gradientn(colours = met.brewer("Demuth", type = "continuous")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,5250000),
                     breaks = seq(0,5250000,1000000),
                     labels = scales::dollar_format()) +
  labs(title = "Top Political Pages on Facebook by Ad Spend: Party Pages",
       subtitle = "How party pages spent on social media advertising",
       x = "Ad Spend by Party and Candidate Pages",
       y = "Page Name"
  ) +
  coord_cartesian(clip="off")+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(), #element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_blank(), #element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  ) 


# Demographic data

so_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(format(round(x/1e3,0), nsmall =0)), "K"),
    x < 1e9 ~ paste0(as.character(format(round(x/1e6,1), nsmall =1)), "M"),
    TRUE ~ "To be implemented..."
  )
}

demographic <- ads%>%
  # filter(group == "Greens") %>%
  select(c (matches("X|group"),id, impressions_mean, group)) %>%
  select(!matches("13")) %>%
  pivot_longer(cols = c(-group, -id, -impressions_mean),
               names_to = "dem_group",
               values_to = "reach") %>%
  mutate(group_impressions = reach*impressions_mean)



demographic <- demographic %>%
  mutate(gender = ifelse(grepl("female", demographic$dem_group), "Female",  ifelse(grepl("male", demographic$dem_group), "Male", "Other"))) %>%
  mutate(age_group = ifelse(grepl("18", demographic$dem_group), "18-24",
                            ifelse(grepl("25",demographic$dem_group), "25-34",
                                   ifelse(grepl("35", demographic$dem_group), "35-44",
                                          ifelse(grepl("45", demographic$dem_group),"45-54",
                                                 ifelse(grepl("55", demographic$dem_group), "55-64",
                                                        ifelse(grepl("65", demographic$dem_group), "65+", NA)
                                                        )
                                                 )
                                          )
                                   )
                            )
        )


# gender 

gender <- demographic %>% 
  group_by(group, gender) %>%
  summarise(gender_total = sum(group_impressions, na.rm = T), across())%>%
  ungroup()%>%
  mutate(group_name = factor(group, levels = c("Coalition", "Labor","Greens", "Independent", "Other")))
  

gender %>%
  filter (!gender =="Other")%>%
  filter(!is.na(group_name))%>%
  distinct(group_name, gender, gender_total)%>%
  ggplot(aes(x = reorder(gender,-gender_total), y = gender_total, fill=group_name, na.rm = T))+
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=so_formatter(gender_total)), position = position_dodge(0.9), size = 6, angle = 90, vjust = "centre", hjust = "bottom")+
  # geom_text(aes(label = so_formatter(persons_reached))) +
  scale_fill_manual(values = c("#002E6D","#ad1a27","#51AF00","#00ABFF","#E7E7E7"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,90000000),
                     breaks = seq(0,90000000,10000000),
                     labels = so_formatter)+
  coord_cartesian(clip = "off")+
  labs(x = "Gender")+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(), #element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_blank(), #element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 

#age group plot

age <- demographic %>% 
  group_by(group, age_group) %>%
  summarise(age_group_total = sum(group_impressions, na.rm = T), across())%>%
  ungroup()%>%
  mutate(group_name = factor(group, levels = c("Coalition", "Labor","Greens", "Independent", "Other")))

age %>%
  filter(!is.na(group_name))%>%
  distinct(group_name, age_group, age_group_total)%>%
  ggplot(aes(x = age_group, y = age_group_total, fill=group_name, na.rm = T))+
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=so_formatter(age_group_total)), position = position_dodge(0.9), size = 6, angle = 90, vjust = "centre", hjust = "bottom")+
  # geom_text(aes(label = so_formatter(persons_reached))) +
  scale_fill_manual(values = c("#002E6D","#ad1a27","#51AF00","#00ABFF","#E7E7E7"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,50000000),
                     breaks = seq(0,50000000,10000000),
                     labels = so_formatter) +
  labs(x = "Age group") +
  coord_cartesian()+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(), #element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_blank(), #element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 



#             
# 
# demographic %>%
#   group_by(gender, id)%>%
#   summarise(gender_reach = sum(reach))
# demographic <- demographic %>%
#   mutate(persons_reached = impressions_mean*reach)
# 
# gender <- demographic %>%
#   select(group, id, gender, persons_reached)%>%
#   group_by(id, gender)%>%
#   summarise(gender_reach = sum(persons_reached))




# gender <- demographic %>%
#   select(id, gender, persons_reached, group) %>%
#   pivot_wider(
#   names_from = gender,
#   values_from = persons_reached
# )

# demographic_average <- demographic %>% group_by(gender, age_group) %>%
#    dplyr::summarise(Mean = mean(reach, na.rm = TRUE))
# 
# demographic_average %>% mutate(
#   Mean = ifelse(gender=="Male", Mean*(-1),
#                       Mean*1),
#   Percent = paste0(abs(round(Mean*100, 1)),"%"))%>%
#   ggplot(aes(x = age_group,y = Mean, fill=gender, label= Percent)) + 
#   geom_bar(stat = "identity", width = 0.4) +
#   coord_flip()+
#   geom_text(hjust = ifelse(demographic_average$gender == "Female", 1.1, -0.1), color = "#FFFFFF") +
#   scale_y_continuous(labels = function(x) percent(abs(x)), limits = max(demographic_average$Mean) * c(-1,1)) +
#   scale_color_manual(values = c("#ad1a27","#002E6D"),
#                      aesthetics = c("color", "fill")) +
#   labs(title = "Demographic Distribution of Ad Reach Across Age and Gender: The Greens",
#        subtitle = "Distribution for all ads published by candidate and party pages of the Australian Greens",
#        x = "Age",
#        y = "Percentage Distribution")+
#   theme(
#     panel.grid = element_blank(),
#     axis.title.y  = element_text(color=txt_col, size=10, hjust=1),
#     axis.title.x  = element_text(color=txt_col, size=10, hjust=1),
#     axis.text = element_text(color=txt_col, size=8, hjust = 0.4),
#     axis.ticks = element_blank(),
#     panel.background = element_rect(fill = bg, color = bg),
#     plot.title = element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
#     plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
#     plot.title.position = "plot",
#     plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
#     plot.background = element_rect(color=bg, fill=bg),
#     plot.margin = margin(30,30,30,30),
#     legend.title = element_blank(),
#     legend.position = "top"
#   ) 

# Region Data 

region <- ads %>% select(id,
                         group,
                         impressions_mean,
                         New.South.Wales,
                         Australian.Capital.Territory,
                         Victoria, 
                         Western.Australia, 
                         South.Australia,
                         Northern.Territory,
                         Queensland,
                         Tasmania) %>%
  pivot_longer(cols =c(-group, -impressions_mean, -id),
               names_to = "state",
               values_to = "reach")
region <- region%>%
  mutate(state_name = ifelse(grepl("Capital", region$state), "Australian \n Capital \n Territory",
                             ifelse(grepl("Western", region$state), "Western \nAustralia",
                                    ifelse(grepl("Northern", region$state), "Northern Territory",
                                           ifelse(grepl("South.Australia", region$state), "South \nAustralia", 
                                                  ifelse(grepl("Wales", region$state), "New South \nWales", region$state))))))%>%
  mutate(state_impressions = reach*impressions_mean)

region <- region %>%
  filter(!is.na(group))%>%
  group_by(group, state_name)%>%
  summarise(state_total = sum(state_impressions, na.rm = T), across())%>%
  ungroup()%>%
  mutate(group_name = factor(group, levels = c("Coalition", "Labor","Greens", "Independent", "Other")))%>%
  distinct(group_name, state_name, state_total)

region %>%
  filter(!is.na(group_name))%>%
  distinct(group_name, state_name, state_total)%>%
  ggplot(aes(x = reorder(state_name,-state_total), y = state_total, fill=group_name, na.rm = T))+
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=so_formatter(state_total)), position = position_dodge(0.9), size = 6, angle = 90, vjust="centre", hjust = "bottom")+
  # geom_text(aes(label = so_formatter(persons_reached))) +
  scale_fill_manual(values = c("#002E6D","#ad1a27","#51AF00","#00ABFF","#E7E7E7"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,90000000),
                     breaks = seq(0,90000000,10000000),
                     labels = so_formatter) +
  labs(x = "States and Territories") +
  coord_cartesian(clip = "off")+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(), #element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_blank(), #element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 





# number_rows = nrow(region_average)


# region_average$hjust<-ifelse( region_average$angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# region_average$angle<-ifelse(region_average$angle < -90, region_average$angle+180, region_average$angle)
# 
# region_average %>% ggplot(aes(x = as.factor(id), y = value, label = label))+
#   geom_bar(stat = "identity", fill = alpha("red", 0.22), width = 0.95) + 
#   geom_text(angle = region_average$angle, hjust = region_average$hjust) +
#   ylim(-7, 30)+
#   theme_minimal()+
#   theme (
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
#   ) +
#   coord_polar(start = 0)
# 


# electorate

division <- ads %>%
  filter(house == "House",
         state == "ACT",
         division == "Bean") %>%
  select(division, spend_mean, fullName, group) %>%
  group_by(division, fullName) %>%
  dplyr::summarise(candidate_spend = sum(spend_mean), across())%>%
  ungroup()%>%
  group_by(division)%>%
  distinct(fullName, candidate_spend, division, group)%>%
  dplyr::summarise(division_spend = sum(candidate_spend), across())%>%
  ungroup()%>%
  group_by(division, group)%>%
  dplyr::summarise(group_spend = sum(candidate_spend), across())%>%
  ungroup()%>%
  arrange(desc(group_spend))%>%
  arrange(desc(division_spend))%>%
  distinct(division, group, group_spend, division_spend)%>%
  mutate(ordering = row_number()) %>%
  slice(1:56)

division <- division %>%
  mutate(group_name = factor(group, levels = c("Coalition", "Labor","Greens", "Independent", "Other")))
  
  

ggplot(division %>% group_by("division") %>% 
         arrange(group_spend, by_group = T),
       aes(x = reorder(division,division_spend), 
           y = group_spend, fill=group_name, 
           group = division)) +
  geom_bar(position = "stack", stat = "identity", width = 0.4) +
  geom_text(aes(y = division_spend, label=dollar(division_spend)), hjust=-0.1, color = txt_col, size = 6) +
  geom_text(aes(x =reorder(division, -group_spend), y =group_spend, label = ifelse(group_spend >=500, so_formatter(group_spend), "")), position = position_stack(vjust = 0.5), hjust = 0.5, size = 6, color = "#FFFFFF")+
  labs(title = "Top 15 Electorates by Candidate Ad Spend",
       subtitle = "Based on ad spend by pages of candidates contesting from Federal divisions",
       x = "",
       y = "Political Ad Spend in AUD"
  ) +
  scale_fill_manual(values = c("#002E6D","#ad1a27","#51AF00","#00ABFF","#E7E7E7"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,27500),
                     breaks = seq(0,27500,12500),
                     labels = scales::dollar_format())+
  coord_flip(clip = "off")+
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=14, hjust=1),
    axis.text = element_text(color=txt_col, size=18, hjust = 0.4),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = bg, color = bg),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 


# Per day spend 

len <- abs(as.numeric(difftime(as.Date("2022-03-21", origin = "yyyy-mm-dd"), as.Date("2022-05-21", origin="yyyy-mm-dd"))))
  
day_wise_spend <- data.frame(date = seq(as.Date("2022-03-21", origin = "yyyy-mm-dd"), by="day", length.out = len),
                             spend = 0)

# per_day = 0
# 
# 
# for (i in nrow(ads)){
#   if(day_wise_spend$date >= ads$ad_delivery_start_time[i] ){
#     day_wise_spend$spend <- day_wise_spend$spend + ads$spend_mean[i]
#   }else{
#     day_wise_spend$spend <- day_wise_spend$spend
#   }
# }
# 
# & day_wise_spend$date <= ads$ad_delivery_stop_time[i]

  