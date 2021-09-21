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

#key_votes_list<- read.csv("raw_data/rollcalls/rollcall_votes_79_101.csv")

#key_votes_list_1<- read.csv("raw_data/rollcalls/rollcall_votes_76_78.csv")

#key_votes_list_2<- read.csv("raw_data/rollcalls/rollcall_votes_75.csv")

#key_votes_list_3<- read.csv("raw_data/rollcalls/rollcall_votes_102.csv")

#key_votes_list_4<- read.csv("raw_data/rollcalls/rollcall_votes_74.csv")

#key_votes_list<- rbind(key_votes_list, key_votes_list_1, key_votes_list_2, key_votes_list_3, key_votes_list_4)

#write.csv(key_votes_list, "clean_data/key_votes_list.csv")

key_votes_list<- read.csv("raw_data/rollcalls/key_votes_list.csv")

Hall_votes_period<- Hall_votes %>% 
  filter(congress > 73) %>% 
  filter(congress < 103)

Hall_all_members_period<- Hall_all_members %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>% 
  filter(chamber != "President")

Hall_votes_period_joined<- Hall_votes_period %>% 
  left_join(key_votes_list, by = c("congress", "chamber", "rollnumber")) %>% 
  distinct(congress, congress_1, chamber, rollnumber, icpsr, cast_code, bill_number, 
           type_1, type_2, reverse_vote, type, aid_type, dtl_desc, region, prob)

key_votes_new<- Hall_votes_period_joined %>% 
  left_join(Hall_all_members_period, by = c("congress", "chamber", "icpsr")) %>% 
  drop_na(type_1)

key_votes_new_reverse_vote<- key_votes_new %>% 
  filter(reverse_vote == "Y")

key_votes_new_reverse_vote$cast_code <- gsub("1", "iso", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("2", "iso", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("3", "iso", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("4", "int", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("5", "int", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("6", "int", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("7", "nv", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("8", "nv", key_votes_new_reverse_vote$cast_code)
key_votes_new_reverse_vote$cast_code <- gsub("9", "nv", key_votes_new_reverse_vote$cast_code)

key_votes_new_no_reverse<- key_votes_new %>% 
  filter(reverse_vote == "N")

key_votes_new_no_reverse$cast_code <- gsub("1", "int", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("2", "int", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("3", "int", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("4", "iso", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("5", "iso", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("6", "iso", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("7", "nv", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("8", "nv", key_votes_new_no_reverse$cast_code)
key_votes_new_no_reverse$cast_code <- gsub("9", "nv", key_votes_new_no_reverse$cast_code)

key_votes<- rbind(key_votes_new_no_reverse, key_votes_new_reverse_vote)

write.csv(key_votes, "clean_data/key_votes.csv")

