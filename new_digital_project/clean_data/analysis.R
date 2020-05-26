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

key_votes_79_90_list<- read.csv("clean_data/key_votes_79_90_list_v2.csv")

Hall_all_members<- read.csv("raw_data/HSall_members.csv")


term_total<- Hall_all_members %>% 
  group_by(icpsr) %>% 
  summarize(term_total = n())

total_bill<- key_votes_79_90_list %>% 
  group_by(congress) %>% 
  summarize(total = n())

key_votes %>%
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  group_by(cohort, congress, chamber) %>% 
  summarize(num = n()) %>% 
  left_join(total_bill, by = "congress") %>% 
  mutate(avg = num / total) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +
  facet_wrap(~chamber) +
  labs(title = "Foreign Policy Votes within the House of Representatives",
       subtitle = "79th through 88th Congresses, by Representative",
       color = "Party",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Number of Votes Per Cohort") +
  scale_color_manual(labels = c("Interventionist", "Noninterventionist", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw() 

key_votes %>%
  filter(party_code ==  "Republican") %>% 
  filter(reverse_vote == "N") %>% 
  group_by(cohort, congress, chamber) %>% 
  summarize(num = n()) %>% 
  left_join(total_bill, by = "congress") %>% 
  mutate(avg = num / total) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +
  facet_wrap(~chamber) +
  labs(title = "Foreign Policy Votes within the Republican Party",
       subtitle = "79th through 88th Congresses, by Representative",
       color = "Party",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Number of Votes Per Cohort") +
  scale_color_manual(labels = c("Interventionist", "Noninterventionist", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw() 

key_votes %>%
  filter(party_code ==  "Republican") %>% 
  filter(congress < 89) %>% 
  filter(type != "mil") %>% 
  group_by(cohort, congress, wing) %>% 
  summarize(num = n()) %>% 
  left_join(total_bill, by = "congress") %>% 
  mutate(avg = num / total) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +
  facet_wrap(~wing) 

label<- c(econ = "Foreign Aid", gov = "Foreign Policy", mil = "Military Policy and Appropriations")
bb <- c(1, 5, 10, 15, 20)
ll <- c("1", "5", "10", "15", "20+")
  
key_votes %>%
  filter(congress > 83) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(cohort == "int") %>% 
  group_by(cohort, party_code, nominate_dim1, nominate_dim2, type) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num, color = party_code)) + geom_point(alpha = .70) + 
  facet_wrap(~type, labeller=labeller(type = label)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
           alpha = .2) +
  labs(title = "Foreign Policy Support, 84th through 90th Congress ",
       subtitle = "By U.S. House Representative, Party, and Bill Type ",
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

#Right wing support for foreign aid
right_wing_support_for_Aid<- key_votes %>%
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
  
  #write.csv("right_wing_support_for_Aid.csv")

key_votes_79_90_list %>% 
  filter(congress > 84) %>%
  filter(reverse_vote == "N") %>% 
  group_by(congress, type) %>%
  filter(type == "econ") %>% 
  summarize(num = n())

bill_total<- key_votes_79_90_list %>% 
  group_by(congress, type) %>% 
  summarize(number = n())


econ_year<- key_votes_79_90_list %>% 
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
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + 
  geom_line(size= 0.5) + facet_wrap(~party_code) +
  labs(title = "Foreign Aid Votes the House of Representatives",
       subtitle = "By Party and Cohort",
       color = "Vote Type",
       shape = "Noninterventionist Votes",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
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


session_total<- Hall_all_members %>% 
  filter(party_code == "200") %>%
  group_by(icpsr) %>% 
  summarize(session_total = n())

sessions_after<- Hall_all_members %>%
  filter(party_code == "200") %>%
  filter(congress > 84) %>% 
  group_by(icpsr) %>% 
  summarize(sessions_after= n())


leaderboard<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "iso") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim2, state_abbrev, chamber) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(session_total, by = 'icpsr') %>% 
  mutate(nay_per = no / session_total) %>% 
  left_join(sessions_after, by = "icpsr")

p1<- leaderboard %>%
  drop_na(sessions_after) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = nay_per, label = bioname, color = chamber)) + geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1)

ggplotly(p1)

write.csv(leaderboard, "leaderboad.csv")

leaderboard_84_on<- key_votes %>% 
  filter(party_code == "Republican") %>%
  filter(type != "mil") %>% 
  filter(cohort == "iso") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim2, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  mutate(nay_per = no / term_total) %>% 
  left_join(term_after, by = "icpsr")

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
  left_join(leaderboard, by = "icpsr") %>% 
  left_join(gulf_of_ton_yay, by = "icpsr")

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

key_votes %>% 
  filter(congress == "90") %>% 
  filter(rollnumber == "419") %>%
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color = cohort, shape = party_code)) + 
  geom_point(size = 4, alpha = .6) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "House Vote on the  Foreign Military Sales Act (90 H.R. 15681)",
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

bill_total<- key_votes_79_90_list %>% 
  group_by(congress, type) %>% 
  summarize(number = n())


key_votes %>%
  filter(chamber == "House") %>% 
  filter(wing == "Right") %>% 
  filter(party_code == "Republican") %>% 
  group_by(cohort, congress, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +      
  facet_wrap(~type, labeller=labeller(type = label)) +
  labs(title = "Voting Patterns of Right-Wing Republicans",
       subtitle = "By Type and Vote",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw() 


key_votes %>%
  filter(wing == "Left" |
         wing == "Center") %>% 
  filter(chamber == "Senate") %>% 
  filter(party_code == "Republican") %>% 
  group_by(cohort, congress, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +      
  facet_wrap(~type, labeller=labeller(type = label)) +
  labs(title = "Voting Patterns of Moderate & Left-Wing Republicans",
       subtitle = "By Type and Vote",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw()

key_votes %>%
  group_by(congress, type, cohort) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +      
  facet_wrap(~type, labeller=labeller(type = label)) +
  labs(title = "Voting Patterns of the House of Representatives",
       subtitle = "By Type and Vote",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw()

key_votes %>%
  filter(nominate_dim1 > 0) %>% 
  filter(nominate_dim2 > 0) %>% 
  filter(party_code == "Democrat") %>% 
  group_by(cohort, congress, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +      
  facet_wrap(~type, labeller=labeller(type = label)) +
  labs(title = "Voting Patterns of Right-Wing Democrats",
       subtitle = "By Type and Vote",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw() 

key_votes %>%
  filter(nominate_dim1 > 0) %>% 
  filter(nominate_dim2 > 0) %>% 
  filter(party_code != 329) %>% 
  group_by(cohort, congress, type, party_code) %>%
  filter(type == "econ") %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +    
  facet_wrap(~party_code) +
  labs(title = "Voting Patterns of Right-Wing Representatives on Foreign Aid",
       subtitle = "By Party",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw()

key_votes %>%
  filter(nominate_dim1 < 0 |
         nominate_dim2 < 0) %>%
  filter(party_code == "Democrat" |
         party_code == "Republican") %>% 
  group_by(cohort, congress, type, party_code) %>%
  filter(type == "econ") %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 2) + geom_line(size= 0.5) +    
  facet_wrap(~party_code) +
  labs(title = "Voting Patterns of Left-Wing & Centrist Representatives on Foreign Aid",
       subtitle = "By Party",
       color = "Vote Type",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = c("green", "red", "orange"))+
  theme_bw()


key_votes %>%
  filter(congress < 84) %>% 
  filter(party_code == "Democrat" |
         party_code == "Republican") %>% 
  distinct(icpsr,congress, party_code, wing) %>% 
  group_by(congress, party_code, wing) %>% 
  summarize(number = n()) %>% 
  ggplot(aes(x= congress, y= number, color = wing)) + geom_point(size= 2) + geom_line(size= 0.5) +
  facet_wrap(~party_code) +
  labs(title = "Size of Cohorts within Republican and Democratic Parties",
       subtitle = "79 to 90 Congress",
       color = "Party",
       x = "Congress",
       y = "Number of Representatives") +
  theme_bw()

key_votes %>%
  filter(party_code ==  "Republican") %>% 
  filter(cohort == "iso") %>%
  filter(type == "econ") %>% 
  group_by(cohort, party_code, nominate_dim1, nominate_dim2) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num)) + geom_point(alpha = .50) + 
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1)

key_votes_tally<- key_votes %>%
  filter(congress == 84) %>% 
  filter(cohort == "iso") %>% 
  group_by(icpsr, bioname, state_abbrev, party_code, district_code, nominate_dim1, nominate_dim2) %>% 
  summarize(num = n())

write.csv(key_votes_tally, "key_votes_tally.csv")

key_votes_tally %>% 
  group_by(state_abbrev) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num))

key_votes_tally %>% 
  group_by(party_code) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num))


key_votes_tally %>%
  group_by(party_code) %>% 
  summarize(num = n()) %>% 
  mutate(avg = num / 12)

Hall_all_members %>% 
  filter(icpsr == 9578) %>% 
  group_by(icpsr) %>% 
  summarize(term_no = n())

for_mil_sales_act<- key_votes %>% 
  filter(congress == "90") %>% 
  filter(rollnumber == "419") %>%
  select(icpsr, cohort)

ike_me_dec<- key_votes %>% 
  filter(congress == "85") %>% 
  filter(rollnumber == "3") %>%
  select(icpsr, cohort)


old_iso<- key_votes %>% 
  filter(congress < 85) %>% 
  filter(party_code == "Republican") %>%
  filter(cohort == "iso") %>% 
  group_by(icpsr, bioname, nominate_dim1, nominate_dim2, state_abbrev) %>% 
  summarize(no = n()) %>% 
  arrange(desc(no)) %>% 
  left_join(term_total, by = 'icpsr') %>% 
  left_join(term_after, by = "icpsr") %>% 
  drop_na(term_total_after) %>% 
  left_join(for_mil_sales_act, by = "icpsr") %>% 
  left_join(ike_me_dec, by = "icpsr") %>% 
  filter(no > 7)

gross_vote_record<- Hall_votes %>% 
  filter(icpsr == 3853) %>% 
  group_by(cast_code) %>% 
  summarize(num = n())

label<- c(econ = "Foreign Aid", gov = "Foreign Policy", mil = "Military Policy and Appropriations")
bb <- c(1, 5, 10, 15, 20)
ll <- c("1", "5", "10", "15", "20+")

key_votes %>%
  filter(congress > 84) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(cohort == "int") %>% 
  group_by(cohort, party_code, nominate_dim1, nominate_dim2, type) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num, color = party_code)) +
  facet_wrap(~type, labeller=labeller(type = label)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_point(alpha = .70) + 
  annotate("rect", xmin = 0.35, xmax = 1, ymin = 0, ymax = 0.5,
           alpha = .3) +
  labs(title = "Cold War Policy Support, 79th through 83rd Congress ",
       subtitle = "By U.S. House Representative, Party, and Bill Type ",
       color = "Party",
       x = "← Liberal      Economic/Redistributive      Conservative →",
       y = "← Liberal      Social Policy      Conservative →") +
  scale_color_manual(labels = c("Democrat", "Republican"),
                     values = c("blue", "red")) +
  scale_size_continuous(name = "Yay Votes",
                        breaks = bb,
                        labels = ll) +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) 

plot<- HS_all_members_1 %>% 
  filter(congress > 85) %>% 
  filter(congress < 91) %>% 
  filter(party_code == 100 |
         party_code == 200) %>% 
  drop_na(nominate_dim1) %>% 
  drop_na(nominate_dim2) %>% 
  group_by(party_code, chamber) %>% 
  summarise(mean(nominate_dim1), mean(nominate_dim2))

plot %>% 
  ggplot(aes(x = `mean(nominate_dim1)`, y = `mean(nominate_dim2)`, shape = chamber)) + geom_point() +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_point(alpha = .70)


label<- c(econ = "Foreign Aid", gov = "Foreign Policy", mil = "Military Policy and Appropriations")
bb <- c(1, 5, 10, 15, 20)
ll <- c("1", "5", "10", "15", "20+")

p<- key_votes %>%
  filter(congress > 84 ) %>% 
  filter(party_code == "Democrat" |
           party_code ==  "Republican") %>% 
  filter(cohort == "int") %>% 
  group_by(bioname, cohort, party_code, nominate_dim1, nominate_dim2, type) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size = num, color = party_code, label = bioname)) +
  facet_wrap(~type, labeller=labeller(type = label)) +
  xlim(-1,1) + ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_point(alpha = .3) + 
  labs(title = "Cold War Policy Support, 79th through 88rd Congress ",
       subtitle = "By U.S. House Representative, Party, and Bill Type ",
       color = "Party",
       x = "← Liberal      Economic/Redistributive      Conservative →",
       y = "← Liberal      Social Policy      Conservative →") +
  scale_color_manual(labels = c("Democrat", "Republican"),
                     values = c("blue", "red")) +
  scale_size_continuous(name = "Yay Votes",
                        breaks = bb,
                        labels = ll) +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=4.5)))

ggplotly(p)

econ_year<- key_votes_79_90_list %>% 
  group_by(congress, type) %>% 
  filter(type == "econ") %>% 
  summarize(number = n())

key_votes %>%
  filter(party_code ==  "Republican") %>%
  filter(type == "econ") %>% 
  group_by(cohort, congress, wing) %>%
  summarize(vote = n()) %>% 
  left_join(econ_year, by = "congress") %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 3) + geom_line(size= 1) +
  facet_wrap(~wing) +
  labs(title = "House Republican Intraparty Voting Trends on Foreign Aid",
       subtitle = "79th to 90th Congresses by Party and Vote",
       color = "Cohort",
       x = "Congress",
       y = "Average Vote Total Per Bill") +
  scale_color_manual(labels = c("Support", "Opposition", "Noncommitted"),
                     values = pal)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=1.5)))


bill_total<- key_votes_79_90_list %>% 
  group_by(congress, type) %>% 
  summarize(number = n())

pal<- c("#33a532", "#cf142b", "#f7b500")

p1<- key_votes %>%
  filter(party_code == "Republican") %>% 
  filter(type == "econ") %>% 
  group_by(cohort, congress, party_code, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 3) + geom_line(size= 1) +      
  scale_color_manual(values = pal,
                     labels = c("Support", "Opposition", "Noncommitted")) +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=1.5)))

ggplotly(p1)

key_votes %>% 
  filter(party_code == "Republican") %>% 
  filter(wing == "Right") %>% 
  group_by(cohort, congress, party_code, type) %>%
  summarize(vote = n()) %>% 
  left_join(bill_total, by = c("congress", "type")) %>% 
  mutate(avg = vote / number) %>% 
  ggplot(aes(x= congress, y= avg, color = cohort)) + geom_point(size= 3) + geom_line(size= 1) +      
  facet_wrap(~type, labeller=labeller(type = label)) +  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=1.5))) +
  scale_color_manual(values = pal,
                     labels = c("Support", "Opposition", "Noncommitted")) +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=1.5)))


p2<- key_votes %>% 
  filter(wing == "Right") %>% 
  filter(cohort == "int") %>% 
  filter(type == "econ") %>% 
  group_by(bioname, nominate_dim1, nominate_dim2, party_code) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num)) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, label =  bioname, size = num, color = party_code)) +
  geom_point() +
  scale_color_manual(labels = c("Democrat", "Republican"),
                     values = c("blue", "red"))

ggplotly(p2)

gop_num<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  distinct(bioname, congress, chamber) %>% 
  group_by(congress, chamber) %>% 
  summarize(total = n())

key_votes %>% 
  filter(party_code == "Republican") %>% 
  distinct(bioname, congress, wing, chamber) %>% 
  group_by(congress, wing, chamber) %>% 
  summarize(num = n()) %>% 
  left_join(gop_num, by = c("congress", "chamber")) %>% 
  mutate(percent = num / total) %>% 
  ggplot(aes(x= congress, y= percent, color = wing)) + geom_point(size= 3) + geom_line(size= 1) +
  facet_wrap(~chamber) +
  labs(title = "Republican Senatorial Ideological Composition, 79th to 90th Congresses",
       subtitle = "Percentage of Wings per Session",
       color = "Wing",
       x = "Congress",
       y = "Percentage of Total GOP Body") +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=1.5)))


sen_total_iso<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  filter(chamber == "Senate") %>% 
  filter(type != "mil") %>% 
  filter(cohort == "iso") %>% 
  group_by(bioname, cohort) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

sen_total_int<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  filter(chamber == "Senate") %>% 
  filter(type != "mil") %>% 
  filter(cohort == "int") %>% 
  group_by(bioname, cohort) %>% 
  summarize(int_num = n()) %>% 
  select(bioname, int_num)

sen_wing<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  select(bioname, wing, nominate_dim1, nominate_dim2) %>% 
  distinct(bioname, wing, nominate_dim1, nominate_dim2)

session_num<- Hall_all_members %>% 
  filter(party_code == "200") %>% 
  filter(chamber == "Senate") %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(bioname, congress) %>% 
  group_by(bioname) %>% 
  summarize(session_no = n())
  
gop_sen<- Hall_all_members %>% 
  filter(party_code == "200") %>% 
  filter(chamber == "Senate") %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(icpsr, bioname, congress) %>% 
  spread(congress, icpsr) %>% 
  left_join(sen_total_iso, by = "bioname") %>% 
  left_join(sen_total_int, by = "bioname") %>% 
  drop_na(iso_num) %>% 
  drop_na(int_num) %>% 
  mutate(total_vote = iso_num + int_num) %>% 
  mutate(iso_per = iso_num / total_vote) %>% 
  left_join(sen_wing, by = "bioname") %>% 
  left_join(session_num, by = "bioname")


  bb <- c(0.1, 0.2, 0.4, 0.6, 0.8)
ll <- c("10%", "20%", "40%", "60%", "80%")

p1<- gop_sen %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size= session_no, 
             color = iso_per, label = bioname)) + geom_point() +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1) +
  labs(title = "Senate Republican Noninterventionist Voting Patters",
       subtitle = "79th to 90th Congresses",
       size = "Number of Sessions Served",
       x = "← Liberal      Economic/Redistributive      Conservative →",
       y = "← Liberal      Social Policy      Conservative →") +
  theme_bw(base_size = 20) +
  scale_color_continuous(name = "Noninterventionist Voting Percentage",
                         breaks = bb,
                         labels = ll,
                         low = "blue",
                         high = "red") +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size=15, color = "Black", face="bold")) +
  guides(color = guide_legend(override.aes = list(size=8)))

ggplotly(p1)

hou_total_iso<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  filter(chamber == "House") %>% 
  filter(type != "mil") %>% 
  filter(cohort == "iso") %>% 
  group_by(bioname, cohort) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

hou_total_int<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  filter(chamber == "House") %>% 
  filter(type != "mil") %>% 
  filter(cohort == "int") %>% 
  group_by(bioname, cohort) %>% 
  summarize(int_num = n()) %>% 
  select(bioname, int_num)

hou_wing<- key_votes %>% 
  filter(party_code == "Republican") %>% 
  select(bioname, wing, nominate_dim1, nominate_dim2) %>% 
  distinct(bioname, wing, nominate_dim1, nominate_dim2)

session_num<- Hall_all_members %>% 
  filter(party_code == "200") %>% 
  filter(chamber == "House") %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(bioname, congress) %>% 
  group_by(bioname) %>% 
  summarize(session_no = n())

gop_hou<- Hall_all_members %>% 
  filter(party_code == "200") %>% 
  filter(chamber == "House") %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(icpsr, bioname, congress) %>% 
  spread(congress, icpsr) %>% 
  left_join(hou_total_iso, by = "bioname") %>% 
  left_join(hou_total_int, by = "bioname") %>% 
  drop_na(iso_num) %>% 
  drop_na(int_num) %>% 
  mutate(total_vote = iso_num + int_num) %>% 
  mutate(iso_per = iso_num / total_vote) %>% 
  left_join(sen_wing, by = "bioname") %>% 
  left_join(session_num, by = "bioname")

gop_hou %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, size= session_no, 
             color = iso_per, label = bioname)) + geom_point() +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey", size = 1) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size = 1)

gop_sen %>% 
  group_by(wing) %>% 
  summarize(mean(iso_per))

gop_hou %>% 
  group_by(wing) %>% 
  summarize(mean(iso_per))

ggplotly(p1)

write.csv(gop_sen, "gop_sen_leaderboard.csv")

gop_num<- Hall_all_members %>% 
  filter(party_code == "200") %>% 
  group_by(congress, chamber) %>% 
  summarize(total = n())

Hall_all_members %>% 
  filter(congress > 71) %>% 
  filter(chamber != "President") %>% 
  filter(wing != "UNK") %>% 
  filter(party_code == 200) %>% 
  group_by(congress, chamber, wing) %>% 
  summarize(num = n()) %>% 
  left_join(gop_num, by = c("congress", "chamber")) %>% 
  mutate(percent = num / total) %>% 
  ggplot(aes(x= congress, y= percent, color = wing)) + geom_point() + geom_line() + facet_wrap(~chamber)


key_votes %>% 
  ggplot(aes(x=nominate_dim1, y=nominate_dim2, color =  wing)) + geom_point()

HSall_parties %>% 
  filter(party_name == "Republican") %>% 
  filter(congress > 78) %>% 
  filter(chamber != "President") %>%
  group_by(congress, chamber) %>% 
  ggplot(aes(x= congress, y= nominate_dim1_median, color = chamber)) + geom_line()
