library(tyverse)
library(dplyr)
library(lubrate)
library(scales)
library(readr)
library(leaflet)
library(sf)
library(shiny)
library(distill)
library(ggplot2)
library(ggExtra)

install.packages("lattice")

district_codes<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/districts/districts.csv", colClasses = "character")
names(district_codes)[1]<-"congress"


voting_data_map<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/new_digital_project/clean_data/key_votes_tally.csv", colClasses = "character")


state_abbr<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/state_abbr.csv", colClasses = "character")


district_codes_joined<- district_codes %>% 
  left_join(state_abbr, by = 'state') %>% 
  filter(congress == 84)

str(district_codes_joined)

str(key_votes_79_84)

voting_data_map_joined<- voting_data_map %>% 
  left_join(district_codes_joined, by = c("district_code", "state_abbrev"))
  
write.csv(voting_data_map_joined, "voting_data_map.csv")  

#names(district_codes)[1]<-"congress"

#isolationists<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/isolationists.csv")

#key_votes<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/079_087_key_no_votes.csv")

#names(key_votes)[1]<-"congress"

#names(isolationists)[1]<-"icpsr"

Hall_all_members<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/vote__view_data/79_90_congress/HSall_members_79_90.csv")

Hall_votes<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/vote__view_data/79_90_congress/Hall_votes_79_90.csv")

isolationists_1<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/isolationists_1.csv")

#Hall_all_members_joined_1<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/Hall_all_members_joined_1.csv")

key_votes_79_84<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/key_votes_79_84.csv", colClasses = "character")

key_yay_votes_79_84<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/key_yay_votes_79_84.csv", colClasses = "character")

Hall_all_members_joined_2<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/Hall_all_members_joined_2.csv")

#isolationists_1<- key_votes_79_84 %>% 
  group_by(icpsr, bioname, party_code, state_abbrev, nominate_dim1, nominate_dim2) %>% 
  summarize(total_nay = sum(nay_no))%>% 
  filter(party_code == "Republican") %>% 
  left_join(isolationists_fate_notes, by = 'icpsr') %>% 
  left_join(interventionists_1, by = 'icpsr')

write.csv(isolationists_1, "isolationists_1.csv")

key_votes_79_84 %>% 
  filter(party_code != '370') %>% 
  filter(congress.x < 85) %>% 
  group_by(party_code, congress.x) %>%
  summarize(total = sum(nay_no)) %>% 
  ggplot(aes(x = congress.x, y= total, color= party_code)) + geom_line(size= 1) + geom_point(size= 2) +
  labs(title = "Decline of Isolationism in the House of Representatives",
       subtitle = "79th through 84th Congresses",
       color = "Party:",
       x = "Congress",
       y = "Number of Nays on Key Foreign Policy Votes") +
  scale_color_manual(labels = c("American Labor Party", "Democrat", "Republican"),
                     values = c("green", "blue", "red")) +
  theme_bw()

isolationists_1 %>% 
  ggplot(aes(x= nominate_dim1.x, nominate_dim2.x, color= total_nay)) + geom_point(size= 3) + 
  scale_colour_gradient(low = "yellow", high = "red") + theme_dark()

isolationists_1 %>% 
  ggplot(aes(x= nominate_dim1.x, nominate_dim2.x, color= fate)) + geom_point(size= 4) + 
  theme_dark()

isolationists_1 %>% 
  group_by(fate) %>% 
  summarize(no = n()) %>%
  arrange(desc(no))

Hall_all_members_joined_2 %>%
  filter(party_code == "Republican") %>% 
  ggplot(aes(x= nominate_dim1, nominate_dim2, color= faction)) + facet_wrap(~congress) + geom_point(size= 3) +
  labs(title = "Foreign Policy Divide within Republican House Representation",
       subtitle = "79 through 84 Congresses",
       color = "Faction",
       caption = "Dots represent Republican representatives. Data available from UCLA's political science's voteview.com project",
       x = "Economic Liberalism-Conservatism",
       y = "Social Liberalism-Conservatism") +
  scale_color_manual(labels = c("Noncommitted", "Committed Interventionst", "Committed Isolationist"),
                     values = c("orange", "purple", "red"))+
  theme_bw() 

#Hall_all_members_joined_2<- Hall_all_members%>% 
  left_join(key_votes_79_84, by = c('congress', 'icpsr')) %>% 
  left_join(key_yay_votes_79_84, by = c('congress', 'icpsr')) %>% 
  left_join(isolationists_fate_notes, by = 'icpsr')

interventionists_1<- key_yay_votes_79_84 %>% 
    filter(congress.x < 85) %>% 
    group_by(icpsr, bioname, party_code, state_abbrev, nominate_dim1, nominate_dim2) %>% 
    summarize(total_yay = sum(yay_no))%>% 
    filter(party_code == "Republican")

key_yay_votes_79_84 %>% 
  filter(party_code == "Republican" |
           party_code == "Democrat") %>% 
  filter(congress.x < 85) %>% 
  group_by(party_code, congress.x) %>%
  summarize(total = sum(yay_no),
            total_members = n()) %>% 
  ggplot(aes(x = congress.x, y= total_members, color= party_code)) + geom_line(size= 1) + geom_point(size= 2) +
  labs(title = "Growth of Interventionism in the House of Representatives",
       subtitle = "79th through 84th Congresses",
       color = "Party:",
       x = "Congress",
       y = "Number of Yays on Key Foreign Policy Votes") + 
  theme_bw()  

Hall_all_members_joined_2 %>% 
  group_by(party_code, congress, faction) %>%
  summarize(total_members = n()) %>% 
  ggplot(aes(x = congress, y= total_members, color= faction)) + geom_point(size= 2) + geom_line(size= 1) +
  labs(title = "Foreign Policy Cohorts within the Republican Party",
       subtitle = "79th through 84th Congresses",
       color = "Party:",
       x = "Congress",
       y = "Number of Representatives") + 
  scale_color_manual(labels = c("Noncommitted", "Committed Interventionst", "Committed Isolationist"),
                     values = c("orange", "purple", "red"))
  theme_bw() 
  
isolationists_1_term_total<- isolationists_1 %>% 
  left_join(term_total, by = 'icpsr')

write.csv(isolationists_1_term_total, "isolationists_1_term_total.csv")

isolationists_1_term_total %>% 
  filter(fate)

isolationists_1_term_total %>% 
  filter(fate != "Died in Office") %>% 
  ggplot(aes(x= total_nay, y= term_total, color= fate)) + geom_point(size= 3, alpha= 0.75) + 
  theme_dark()

names(key_votes_79_84)[6]<-"congress"
names(key_yay_votes_79_84)[6]<-"congress"

gop<- key_votes_79_84 %>% 
  left_join(Hall_all_members_79_84, by = c('icpsr', 'congress'))

gop1<- key_yay_votes_79_84 %>% 
  left_join(Hall_all_members_79_84, by = c('icpsr', 'congress'))
  

#write.csv(gop_join, "gop_join.csv")

gop_join %>% 
  distinct(icpsr)

write.csv(key_votes_79_84, "key_votes_79_84.csv")

write.csv(key_yay_votes_79_84, "key_yay_votes_79_84.csv")

key_votes_79_84 %>% 
  ggplot(aes(x= nominate_dim1, nominate_dim2, color= faction)) + geom_point(size = 3) + facet_wrap(~congress)

fence_sitter<- Hall_all_members_79_84 %>% 
  anti_join(key_votes_79_84, by = 'icpsr') %>% 
  anti_join(key_yay_votes_79_84, by = 'icpsr')

fence_sitter %>% 
  group_by(party_code) %>% 
  summarize(no= n())

write.csv(Hall_all_votes_79_84_fence_sitter, "Hall_all_votes_79_84_fence_sitter.csv")

key_votes_all<- rbind(key_votes_79_84, Hall_all_votes_79_84_fence_sitter)

key_votes_all_joined<- key_votes_all %>% 
  left_join(isolationists_fate_notes, by = 'icpsr')

write.csv(key_votes_all_joined, "key_votes_all.csv")

key_votes_all %>% 
  filter(party_code == 'Republican') %>% 
  filter(faction == 'Isolationist') %>% 
  distinct(icpsr, fate) %>% 
  group_by(fate) %>% 
  summarize(num= n())

test<- key_votes_all %>%
  filter(party_code == "Republican") %>%
  anti_join(the_flipped, by = 'icpsr') %>% 
  anti_join(the_loyal, by = 'icpsr')

fence_sitters_79_84<- Hall_all_members_79_84 %>% 
  anti_join(key_votes_79_84, by = 'icpsr') %>% 
  anti_join(key_yay_votes_79_84, by = 'icpsr') %>% 
  select(icpsr, bioname, state_abbrev, district_code, party_code, congress, nominate_dim1, nominate_dim2)

write_csv(fence_sitters_79_84, "fence_sitters_79_84.csv")
write.csv(key_votes_79_84, "key_votes_79_84.csv")
write.csv(key_yay_votes_79_84, "key_yay_votes_79_84.csv")

key_votes_all_fate<- key_votes_all_1 %>% 
  left_join(isolationists_fate_notes, by = 'icpsr')

write.csv(key_votes_all_fate, "key_votes_all_fate.csv")

test<- key_votes_all_fate %>% 
  group_by(bioname, icpsr, party_code) %>% 
  summarize(yay_total = sum(yay_no),
            nay_total = sum(nay_no))

install.packages("pals")


write.csv(Hall_all_members_79_84_vote_join, "Hall_all_members_79_84_vote_join.csv")

isolationists_fate<- isolationists_1 %>% 
  left_join(isolationists_fate_notes, by ='icpsr')

write.csv(isolationists_fate, "isolationists_fate.csv")


key_votes_all %>%
  filter(party_code == '100') %>% 
  ggplot(aes(x= nominate_dim1, nominate_dim2, color= faction)) + facet_wrap(~congress) + geom_point(size= 5, alpha= 0.5) +
  labs(title = "Foreign Policy Divide within Republican House Representation",
       subtitle = "79 through 84 Congresses, by Representative",
       color = "Faction",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Noncommitted", "Committed Interventionst", "Committed Isolationist"),
                     values = c("orange", "purple", "red"))+
  theme_bw(base_size = 30) 

isolationists_final %>% 
  filter(term_total > 2) %>% 
  filter(fate != "Softened") %>% 
  filter(fate != "Flipped")

isolationists_final<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/map/isolationists_final.csv")

plot1<- ggplot(isolationists_final,aes(x= nominate_dim1, y= nominate_dim2, color= fate, size = total_nay)) + geom_point() + 

  theme_dark()

plot1


interventionists_2 %>%
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color= former_iso, size = total_yay)) + geom_point(alpha= 0.70) + 
  facet_wrap(~former_iso) +
  scale_color_brewer(palette="Set1") +
  labs(title = "Interventionists and their Defectors",
       subtitle = "79 through 84 Congresses, by Representative",
       color = "Former Isolationist?",
       size = "Total Yay Votes",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  theme_bw()

summary(isolationists$nominate_dim2)


str(Hall_all_members_79_84)



key_votes_all %>%
  ggplot(aes(x= nominate_dim1, nominate_dim2, color= faction)) + geom_point(size= 3, alpha= 0.5) +
  labs(title = "Foreign Policy Divide within Republican House Representation",
       subtitle = "82th through 84th Congresses, by Representative",
       color = "Faction",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  scale_color_manual(labels = c("Noncommitted", "Committed Interventionst", "Committed Isolationist"),
                     values = c("orange", "purple", "red"))+
  theme_bw() 


isolationists_1<- isolationists %>% 
  left_join(isolationists_fate_notes, by = 'icpsr') %>% 
  left_join(interventionists_1, by = 'icpsr')

write.csv(isolationists_1, "isolationists_1.csv")


isolationists_final %>% 
  filter(fate == "Flipped or Moderated") %>% 
  filter(nominate_dim1 > 0.75)

Hall_all_members_79_84_codes<-Hall_all_members_79_84 %>% 
  select(icpsr, district_code)

isolationists_final_1<- isolationists_final %>%
  left_join(Hall_all_members_79_84_codes)

write.csv(isolationists_final_1, "isolationists_final.csv")

isolationists_final %>%
  filter(term_total_after > 2) %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color= fate, size = total_nay)) + geom_point(alpha= 0.70) + 
  scale_color_brewer(palette="Set1") +
  labs(title = "Fate of 'Isolationist' Republican Representatives",
       subtitle = "79 through 84 Congresses, by Representative",
       color = "Fate",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  theme_bw() + scale_size_continuous(name = "Total Nay Votes",
                                     breaks = c(4,10,15,20),
                                     labels = c("4","10","15","20+"))

isolationists_final %>%
  filter(fate == "Holdout") %>% 
  filter(nominate_dim1 < 0.15)


left_join()
Hall_votes_84_joined %>% 
  group_by(party_code) %>% 
  summarize(no = n())

key_votes_all %>% 
  distinct(congress, party_code, icpsr, bioname, nay_no, yay_no, faction) %>% 
  group_by(congress, faction) %>% 
  summarize(num= n()) %>% 
  ggplot(aes(x = congress, y= num, color= faction)) + geom_point(size= 2) + geom_line(size= 0.5) +
  labs(title = "Foreign Policy Cohorts within the House of Representatives",
       subtitle = "79th through 84th Congresses, by party",
       color = "Cohort:",
       x = "Congress",
       y = "Number of Representatives in Cohort") + 
  scale_color_manual(labels = c("Noncommitted", "Committed Interventionst", "Committed Isolationist"),
                     values = c("orange", "purple", "red")) +
  theme_classic()

Hall_all_members_years <- Hall_all_members %>% 
  select(icpsr, born, died)

isolationists_final_year<- isolationists_final %>% 
  left_join(Hall_all_members_years, by = 'icpsr')

write.csv(isolationists_final_year, "isolationists_final_year.csv")

inter_spread_1<- spread(inter_spread, congress, faction)

interventionists_final<- interventionists_1 %>% 
  left_join(inter_spread_1, by = "icpsr") %>% 
  left_join(term_total, by = "icpsr") %>% 
  left_join(term_study, by = 'icpsr') %>% 
  left_join(term_after, by = 'icpsr') 

write_csv(interventionists_final, "interventionists_final.csv")

isolationists_final_1<-isolationists_final %>% 
  mutate(age_1945 = X79_year - born)

isolationists_loyal<- isolationists_final_1 %>% 
  filter(loyal == "Y")

summary(isolationists_loyal$age_1945)


interventionists_2<-interventionists_final %>%
  left_join(Hall_all_members_years, by = 'icpsr') %>% 
  mutate(age_1945 = X79_year - born) %>% 
  distinct(icpsr, bioname, born, age_1945, loyal)

isolationists_loyal<- isolationists_final_1 %>% 
  filter(loyal == "Y")

summary(isolationists_loyal$age_1945)

isolationists_decline<- isolationists_final %>% 
  filter(korea_surge == "N") %>% 
  filter(fate == "Flipped or moderated") %>% 
  filter(Faction_81 == "Isolationist") %>% 
  filter(Faction_82 != "Not in Congress")

isolationists_final_plot<- isolationists_final %>%
  filter(korea_surge == "N") 

library(plotly)
  
p<- isolationists_final %>%
  filter(korea_surge == "N") %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color= fate, size = total_nay)) + geom_point(alpha= 0.70) + 
  scale_color_brewer(palette="Set1") +
  labs(title = "Fate of 'Isolationist' Republican Representatives",
       subtitle = "79 through 84 Congresses, by Representative",
       color = "",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  theme_bw() + scale_size_continuous(name = "",
                                     breaks = c(4,10,15,20),
                                     labels = c("4","10","15","20+"))


ggplotly(p)


p<- isolationists_final %>%
  filter(korea_surge == "N") %>% 
  ggplot(aes(x= nominate_dim1, y= nominate_dim2, color= fate, size = total_nay, label = bioname)) + geom_point(alpha= 0.70) + 
  scale_color_brewer(palette="Set1") +
  labs(title = "Fate of 'Isolationist' Republican Representatives",
       subtitle = "79 through 84 Congresses, by Representative",
       caption = "Data available from UCLA's political science's voteview.com project",
       x = "<- Liberal      Economic/Redistributive      Conservative->",
       y = "<- Liberal      Social Policy      Conservative->") +
  theme_bw()

ggplotly(p)


key_votes_all %>%
  filter(party_code == "Democrat" |
         party_code == "Republican") %>% 
  group_by(congress, party_code) %>%
  drop_na(nominate_dim1) %>% 
  drop_na(nominate_dim2) %>% 
  summarize(econ_avg = mean(nominate_dim1),  
            social_avg = mean(nominate_dim2)) %>% 
  ggplot(aes(x= congress, y= econ_avg, color= party_code)) + geom_line() + geom_point()

party_ideo_shift<- Hall_all_members %>% 
  filter(party_code == "200") %>%
  filter(congress == 70:90) %>% 
  group_by(congress) %>%
  drop_na(nominate_dim1) %>% 
  drop_na(nominate_dim2) %>% 
  summarize(econ_avg = mean(nominate_dim1),  
            social_avg = mean(nominate_dim2))

isolationists_final %>%
