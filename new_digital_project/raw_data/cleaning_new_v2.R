library(dplyr) 
library(scales)
library(readr)
library(distill)
library(ggplot2)
library(ggExtra)
library(jpeg)
library(knitr)
library(pals)
library(grid)
library(tidyverse)
library(plotly)
library(RColorBrewer)


Hall_all_members<- read.csv("raw_data/HSall_members.csv")

Hall_votes<- read.csv("raw_data/HSall_votes.csv")

key_votes_79_88_list<- read.csv("clean_data/key_votes_79_90_list_v2.csv")

Hall_votes_period<- Hall_votes %>% 
  filter(congress > 78) %>% 
  filter(congress < 91)

Hall_all_members_period<- Hall_all_members %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>% 
  filter(chamber != "President")

Hall_votes_period_joined<- Hall_votes_period %>% 
  left_join(key_votes_79_88_list, by = c("congress", "chamber", "rollnumber")) %>% 
  drop_na(bill_no) %>% 
  distinct(congress, chamber, rollnumber, icpsr, cast_code, bill_no, type, reverse_vote, type, aid_type, bill_name, link_1)

write.csv(Hall_votes_period_joined, "Hall_votes_joined.csv")

key_votes_new<- Hall_votes_period_joined %>% 
  left_join(Hall_all_members_period, by = c("congress", "chamber", "icpsr")) %>% 
  drop_na(bioname)

write.csv(key_votes_new, "key_votes_new.csv")

