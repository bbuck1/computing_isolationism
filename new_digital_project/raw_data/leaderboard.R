total_iso<- key_votes %>% 
  filter(cohort == "iso") %>% 
  group_by(icpsr, chamber, party_code) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, iso_num, chamber, party_code)

total_int<- key_votes %>% 
  filter(cohort == "int") %>% 
  group_by(icpsr, chamber, party_code) %>% 
  summarize(int_num = n()) %>% 
  select(icpsr, int_num, chamber, party_code)

total_vote<- key_votes %>% 
  filter(cohort != "nv") %>% 
  group_by(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, total_vote, chamber, party_code, nominate_dim1, nominate_dim2)

congress_num<- Hall_all_members %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(icpsr, congress, chamber, party_code) %>% 
  group_by(icpsr, chamber) %>% 
  summarize(congress_no = n())

total_iso_session<- key_votes %>% 
  filter(cohort == "iso") %>% 
  group_by(icpsr, congress, chamber, party_code) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, iso_num, congress, chamber, party_code)

total_vote_session<- key_votes %>% 
  filter(cohort != "nv") %>% 
  group_by(icpsr, chamber, congress, party_code) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, total_vote, chamber, congress, party_code) %>% 
  left_join(total_iso_session, by = c ("icpsr", "chamber", "congress", "party_code")) %>% 
  mutate(iso_sess = iso_num / total_vote)

total_vote_session[is.na(total_vote_session)] <- 0

congress_num<- Hall_all_members %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  select(bioname, congress, chamber, party_code) %>% 
  group_by(bioname, chamber, party_code) %>% 
  summarize(congress_no = n())

leaderboard<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(chamber != "President") %>% 
  filter(congress > 78) %>% 
  filter(congress < 91) %>%
  left_join(total_vote_session, by = c ("icpsr", "chamber", "congress", "party_code")) %>% 
  select(icpsr, chamber, party_code, congress, iso_sess) %>% 
  spread(congress, iso_sess) %>% 
  left_join(total_iso, by = c ("icpsr", "chamber", "party_code")) %>% 
  left_join(total_int, by = c ("icpsr", "chamber", "party_code")) %>% 
  left_join(total_vote, by = c ("icpsr", "chamber", "party_code")) %>% 
  left_join(congress_num, by = c ("bioname", "chamber", "party_code")) %>% 
  mutate(oppo_per = iso_num / total_vote) %>% 
  mutate(supp_per = int_num / total_vote) %>% 
  mutate(id_score = nominate_dim1 + nominate_dim2)


flipped<- leaderboard %>% 
  filter(congress_no > 1) %>% 
  drop_na(79)

write.csv(leaderboard, "leaderboard.csv")
