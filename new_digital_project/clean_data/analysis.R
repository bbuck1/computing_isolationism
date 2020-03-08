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

key_votes<- read.csv("clean_data/key_votes_new.csv")

key_votes_79_88_list<- read.csv("clean_data/key_votes_79_88_list.csv")

Hall_all_members<- read.csv("raw_data/HSall_members.csv")

term_total<- Hall_all_members %>% 
  group_by(icpsr) %>% 
  summarize(term_total = n())

key_votes %>%
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(congress < 89) %>% 
  group_by(cohort, congress, party_code) %>% 
  summarize(num = n()) %>%
  ggplot(aes(x= congress, y= num, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) + facet_wrap(~party_code) +
  labs(title = "Foreign Policy Votes within the House of Representatives",
       subtitle = "79th through 88th Congresses, by Representative",
       color = "Party",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Number of Votes Per Cohort") +
  scale_color_manual(labels = c("Interventionist", "Noninterventionist", "Noncommitted"),
                     values = c("green", "orange", "yellow"))+
  theme_bw() 

label<- c(econ = "Foreign Aid", gov = "Foreign Policy", mil = "Military Policy and Appropriations")
bb <- c(1, 5, 10, 15, 20)
ll <- c("1", "5", "10", "15", "20+")
  
key_votes %>%
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(cohort == "iso") %>%
  group_by(cohort, party_code, nominate_dim1, nominate_dim2, type) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num, color = party_code)) + geom_point(alpha = .50) + 
  facet_wrap(~type, labeller=labeller(type = label)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "Foreign Policy Opposition, 85th through 88th Congresses",
       subtitle = "By Representative, Party, and Bill Type ",
       color = "Party",
       size = "Nay Votes",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Democrat", "Republican"),
                     values = c("blue", "red")) +
  theme_bw() 



key_votes %>%
  filter(congress > 84) %>% 
  filter(congress < 89) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(cohort == "int") %>% 
  group_by(cohort, party_code, nominate_dim1, nominate_dim2, type) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num, color = party_code)) + geom_point(alpha = .50) + 
  facet_wrap(~type, labeller=labeller(type = label)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
           alpha = .2) +
  labs(title = "Foreign Policy Support, 85th through 88th Congressess",
       subtitle = "By Representative, Party, and Bill Type ",
       color = "Party",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Democrat", "Republican"),
                     values = c("blue", "red")) +
  scale_size_continuous(name = "Yay Votes",
                        breaks = bb,
                        labels = ll) +
  theme_bw(base_size = 20) +
  guides(color = guide_legend(override.aes = list(size=5)))

key_votes %>%
  filter(congress > 84 |
           congress < 89) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(type == "econ") %>% 
  group_by(cohort, congress, party_code) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= congress, y= num, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) + 
  facet_wrap(~party_code)
  

#Right wing support for foreign aid
key_votes %>%
  filter(congress > 84 |
         congress < 89) %>% 
  filter(cohort == "int") %>%
  filter(type == "econ") %>% 
  filter(nominate_dim1 > 0.0) %>% 
  filter(nominate_dim2 > 0.0) %>% 
  group_by(bioname, party_code, icpsr) %>% 
  summarize(num = n()) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(per_term = num / term_total) %>% 
  arrange(desc(num))
  
  write.csv("right_wing_support_for_Aid.csv")

key_votes_79_88_list %>% 
  filter(congress > 84) %>%
  filter(reverse_vote == "N") %>% 
  group_by(congress, type) %>%
  filter(type == "econ") %>% 
  summarize(num = n())

bill_total<- key_votes_79_88_list %>% 
  group_by(congress, type) %>% 
  summarize(number = n())

key_votes %>%
  filter(party_code ==  "Republican") %>%
  filter(congress < 89) %>% 
  group_by(cohort, congress, party_code, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) + facet_wrap(~type) +
  labs(title = "Votes the House of Representatives",
       subtitle = "By type and vote",
       color = "Vote Type",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Yay", "Nay", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw() 

econ_year<- key_votes_79_88_list %>% 
  group_by(congress, type) %>% 
  filter(type == "econ") %>% 
  summarize(number = n())

key_votes %>%
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>%
  filter(type == "econ") %>% 
  group_by(cohort, congress, party_code) %>%
  summarize(vote = n()) %>% 
  left_join(econ_year, by = "congress") %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) + facet_wrap(~party_code) +
  labs(title = "Foreign Aid Votes the House of Representatives",
       subtitle = "By Party and Cohort",
       color = "Vote Type",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Yay", "Nay", "Noncommitted"),
                     values = c("green", "orange", "yellow"))+
  theme_bw() 

key_votes %>% 
  filter(cohort == "iso") %>% 
  filter(party_code == "Republican") %>% 
  group_by(congress, type) %>%
  summarize(num = n()) %>% 
  ggplot(aes(x= congress, y= num, color = type)) + geom_point(size= 2) + geom_line(size= 0.5) +
  labs(title = "Republican Nay on Foreign Policy Votes",
       subtitle = "By Type",
       color = "Type",
       x = "Gongress",
       y = "Number of Votes Per Type") +
  scale_color_manual(labels = c("Foreign Aid", "Foreign Policy Initiatives", "Military Policy and Appropriations"),
                     values = c("green", "orange", "blue"))+
  theme_bw() 

key_votes %>% 
  filter(congress > 79) %>% 
  filter(type == "econ") %>% 
  filter(cohort == "iso") %>% 
  filter(party_code == "Republican") %>% 
  group_by(congress) %>% 
  summarize(num = n())

term_after<- Hall_all_members %>%
  filter(congress > 84) %>% 
  group_by(icpsr) %>% 
  summarize(term_total_after= n())

holdouts<- key_votes %>% 
  filter(cohort == "iso") %>% 
  filter(party_code == "Republican") %>% 
  filter(congress > 84) %>% 
  group_by(icpsr, bioname) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no))

term_total<- Hall_all_members %>% 
  filter(congress > 78 |
         congress < 89) %>% 
  group_by(icpsr) %>% 
  summarize(term_total = n())

leaderboard<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "iso") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim2, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(nay_per = no / term_total)

leaderboard_wo_mil<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "iso") %>%
  filter(type != "mil") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim1, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(nay_per = no / term_total)

leaderboard_gov<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "iso") %>%
  filter(type == "gov") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim1, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(nay_per = no / term_total)

  
leaderboard_int_aid<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "int") %>% 
  filter(type == "econ") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim1, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(yay_per = no / term_total)

key_votes %>%
  filter(congress > 84) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  group_by(icpsr, bioname, party_code, nominate_dim1, nominate_dim2, cohort) %>% 
  summarize(num = n()) %>% 
  filter(cohort == "int") %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, shape = party_code, color = num)) + geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  scale_color_gradient(low="red", high="green")

key_votes %>% 
  filter(congress == "87") %>% 
  filter(bill_no == "HR8400") %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color = cohort, shape = party_code)) + geom_point(size = 3, alpha = .8) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "House Vote on Foreign Assistance Act of 1961 (87 H.R. 8400)",
       subtitle = "By Party and Vote",
       color = "Cohort",
       shape = "Party",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Yay", "Nay", "No Vote"),
                     values = c("green", "red", "orange"))+
  theme_bw() 


Hall_all_members_90th<- Hall_all_members %>% 
  filter(congress == "90")

bill_yay<- key_votes %>% 
  filter(congress == "87") %>% 
  filter(bill_no == "HR8400") %>% 
  filter(party_code == "Republican") %>% 
  filter(cohort == "int")

reccommit_gop_yay<- reccommit %>% 
  filter(V1 == 1) %>% 
  left_join(Hall_all_members_90th, by = "icpsr") %>% 
  filter(party_code == "200")

names(reccommit)[1] <- "icpsr"

the_flipped<- reccommit_gop_yay[match(bill_yay$icpsr, reccommit_gop_yay$icpsr, nomatch=0),] %>% 
  left_join(leaderboard_int_aid, by = "icpsr") %>% 
  arrange(desc(no))

#write.csv(the_flipped, "the_flipped_for_aid.csv")

ike_doc_nay<- key_votes %>% 
  filter(congress == "85") %>% 
  filter(rollnumber == "3") %>% 
  filter(cohort == "iso") %>% 
  filter(party_code == "Republican") %>% 
  left_join(leaderboard, by = "icpsr")

key_votes %>% 
  filter(congress == "85") %>% 
  filter(rollnumber == "3") %>%
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color = cohort, shape = party_code)) + 
  geom_point(size = 3, alpha = .8) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "House Vote on the Eisenhower Doctrine (85 H.J.Res. 117)",
       subtitle = "By Party and Vote",
       color = "Cohort",
       shape = "Party",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Yay", "Nay", "No Vote"),
                     values = c("green", "red", "orange"))+
  theme_bw() 

gulf_of_ton_yay<- key_votes %>% 
  filter(congress == "88") %>% 
  filter(rollnumber == "197") %>% 
  filter(cohort == "int") %>% 
  filter(party_code == "Republican")

the_flipped_gulf_of_ton<- gulf_of_ton_yay[match(ike_doc_nay$icpsr, gulf_of_ton_yay$icpsr, nomatch=0),] %>% 
  left_join(leaderboard, by = "icpsr")
#write.csv(the_flipped_gulf_of_ton, "the_flipped_gulf_of_ton.csv")

key_votes %>% 
  filter(congress == "88") %>% 
  filter(rollnumber == "197") %>%
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color = cohort, shape = party_code)) + 
  geom_point(size = 4, alpha = .6) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "House Vote on the Gulf of Tonkin Resolution (88 H.J. RES. 1145)",
       subtitle = "By Party and Vote",
       color = "Cohort",
       shape = "Party",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Yay", "Nay", "No Vote"),
                     values = c("green", "red", "orange")) +
  theme_bw() 

post_88_holdouts<- Hall_all_members %>% 
  filter(congress > 88) %>% 
  filter(icpsr == "93" |
         icpsr == "986" |
         icpsr == "1941" |
         icpsr == "3853" |
         icpsr == "4928" |
         icpsr == "7046" |
         icpsr == "9578" |
         icpsr == "9578")


