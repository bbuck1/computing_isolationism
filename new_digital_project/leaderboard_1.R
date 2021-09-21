key_votes<- read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/new_digital_project/clean_data/key_votes.csv")

Hall_all_members<- read.csv("raw_data/HSall_members.csv")

regions<-read.csv("C:/Users/bpbuc/Documents/Brandan's Stuff/_PhD Stuff/Dissertation Research/new_digital_project/clean_data/region.csv")

gen<- Hall_all_members %>% 
  select(icpsr, gen)

#individual leaderboard

total_iso<- key_votes %>%  #Total number of votes in opposition to rollcalls
  filter(wing != "UNK") %>% 
  filter(cast_code == "iso") %>% 
  group_by(bioname) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

total_vote<- key_votes %>%  #Total number of votes to rollcalls, excluding abstentions, votes of present, etc. 
  filter(wing != "UNK") %>% 
  filter(cast_code != "nv") %>% 
  group_by(bioname) %>% 
  summarize(total_vote = n()) %>% 
  left_join(total_iso, by =c("bioname"))

total_vote[is.na(total_vote)] <- "0"

congress_num<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  select(icpsr) %>% 
  group_by(icpsr) %>% 
  summarize(congress_num = n())

total_vote$total_vote <- as.numeric(as.character(total_vote$total_vote))
total_vote$iso_num <- as.numeric(as.character(total_vote$iso_num))

last_cong<- Hall_all_members %>%  #The last congress served by each individual
  group_by(icpsr) %>% 
  do(tail(., 1)) %>% 
  filter(congress > 73) %>% 
  select(icpsr, congress)

first_cong<- Hall_all_members %>% #The first congress served by each individual
  group_by(icpsr) %>% 
  do(head(., 1)) %>% 
  filter(congress < 103) %>% 
  select(icpsr, congress)

names(first_cong)[2]<-"first_cong"

names(last_cong)[2]<-"last_cong"

total_iso_session<- key_votes %>% 
  filter(cast_code == "iso") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, party_code, chamber, congress, iso_num)

total_vote_session<- key_votes %>% 
  filter(cast_code != "nv") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, party_code,chamber, total_vote, congress) %>% 
  left_join(total_iso_session, by = c("icpsr", "congress", "chamber", "party_code"))

total_vote_session[is.na(total_vote_session)] <- "0"

total_vote_session$total_vote <- as.numeric(as.character(total_vote_session$total_vote))
total_vote_session$iso_num <- as.numeric(as.character(total_vote_session$iso_num))


total_vote_session_1<- total_vote_session %>% 
  mutate(opp_sess = iso_num / total_vote) %>% 
  select(icpsr, congress, party_code, chamber, opp_sess) %>% 
  spread(congress, opp_sess)

total_vote_session_1[is.na(total_vote_session_1)] <- "X"

leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(oppo_per = iso_num / total_vote) %>% 
  arrange(desc(oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber"))

write.csv(leaderboard, "clean_data/leaderboard.csv")

gop_leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  filter(party_code == "Republican") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(oppo_per = iso_num / total_vote) %>% 
  arrange(desc(oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber")) %>% 
  filter(congress_num > 2)

write.csv(gop_leaderboard, "clean_data/gop_leaderboard.csv")

# Mil Scores

total_iso<- key_votes %>%  #Total number of votes in opposition to rollcalls
  filter(type_1 == "mil") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code == "iso") %>% 
  group_by(bioname) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

total_vote<- key_votes %>%  #Total number of votes to rollcalls, excluding abstentions, votes of present, etc. 
  filter(type_1 == "mil") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code != "nv") %>% 
  group_by(bioname) %>% 
  summarize(total_vote = n()) %>% 
  left_join(total_iso, by =c("bioname"))

total_vote[is.na(total_vote)] <- "0"

congress_num<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  select(icpsr) %>% 
  group_by(icpsr) %>% 
  summarize(congress_num = n())

total_vote$total_vote <- as.numeric(as.character(total_vote$total_vote))
total_vote$iso_num <- as.numeric(as.character(total_vote$iso_num))

last_cong<- Hall_all_members %>%  #The last congress served by each individual
  group_by(icpsr) %>% 
  do(tail(., 1)) %>% 
  filter(congress > 73) %>% 
  select(icpsr, congress)

first_cong<- Hall_all_members %>% #The first congress served by each individual
  group_by(icpsr) %>% 
  do(head(., 1)) %>% 
  filter(congress < 103) %>% 
  select(icpsr, congress)

names(first_cong)[2]<-"first_cong"

names(last_cong)[2]<-"last_cong"

total_iso_session<- key_votes %>% 
  filter(type_1 == "mil") %>% 
  filter(cast_code == "iso") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, party_code, chamber, congress, iso_num)

total_vote_session<- key_votes %>% 
  filter(type_1 == "mil") %>% 
  filter(cast_code != "nv") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, party_code,chamber, total_vote, congress) %>% 
  left_join(total_iso_session, by = c("icpsr", "congress", "chamber", "party_code"))

total_vote_session[is.na(total_vote_session)] <- "0"

total_vote_session$total_vote <- as.numeric(as.character(total_vote_session$total_vote))
total_vote_session$iso_num <- as.numeric(as.character(total_vote_session$iso_num))


total_vote_session_1<- total_vote_session %>% 
  mutate(opp_sess = iso_num / total_vote) %>% 
  select(icpsr, congress, party_code, chamber, opp_sess) %>% 
  spread(congress, opp_sess)

total_vote_session_1[is.na(total_vote_session_1)] <- "X"

mil_leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(mil_oppo_per = iso_num / total_vote) %>% 
  arrange(desc(mil_oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(mil_oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber"))

write.csv(leaderboard, "clean_data/mil_leaderboard.csv")

#aid scores

total_iso<- key_votes %>%  #Total number of votes in opposition to rollcalls
  filter(type_1 == "econ") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code == "iso") %>% 
  group_by(bioname) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

total_vote<- key_votes %>%  #Total number of votes to rollcalls, excluding abstentions, votes of present, etc. 
  filter(type_1 == "econ") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code != "nv") %>% 
  group_by(bioname) %>% 
  summarize(total_vote = n()) %>% 
  left_join(total_iso, by =c("bioname"))

total_vote[is.na(total_vote)] <- "0"

congress_num<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  select(icpsr) %>% 
  group_by(icpsr) %>% 
  summarize(congress_num = n())

total_vote$total_vote <- as.numeric(as.character(total_vote$total_vote))
total_vote$iso_num <- as.numeric(as.character(total_vote$iso_num))

last_cong<- Hall_all_members %>%  #The last congress served by each individual
  group_by(icpsr) %>% 
  do(tail(., 1)) %>% 
  filter(congress > 73) %>% 
  select(icpsr, congress)

first_cong<- Hall_all_members %>% #The first congress served by each individual
  group_by(icpsr) %>% 
  do(head(., 1)) %>% 
  filter(congress < 103) %>% 
  select(icpsr, congress)

names(first_cong)[2]<-"first_cong"

names(last_cong)[2]<-"last_cong"

total_iso_session<- key_votes %>% 
  filter(type_1 == "econ") %>% 
  filter(cast_code == "iso") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, party_code, chamber, congress, iso_num)

total_vote_session<- key_votes %>% 
  filter(type_1 == "econ") %>% 
  filter(cast_code != "nv") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, party_code,chamber, total_vote, congress) %>% 
  left_join(total_iso_session, by = c("icpsr", "congress", "chamber", "party_code"))

total_vote_session[is.na(total_vote_session)] <- "0"

total_vote_session$total_vote <- as.numeric(as.character(total_vote_session$total_vote))
total_vote_session$iso_num <- as.numeric(as.character(total_vote_session$iso_num))


total_vote_session_1<- total_vote_session %>% 
  mutate(opp_sess = iso_num / total_vote) %>% 
  select(icpsr, congress, party_code, chamber, opp_sess) %>% 
  spread(congress, opp_sess)

total_vote_session_1[is.na(total_vote_session_1)] <- "X"

econ_leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(econ_oppo_per = iso_num / total_vote) %>% 
  arrange(desc(econ_oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(econ_oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber"))

write.csv(leaderboard, "clean_data/econ_leaderboard.csv")

#gov scores
total_iso<- key_votes %>%  #Total number of votes in opposition to rollcalls
  filter(type_1 == "gov") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code == "iso") %>% 
  group_by(bioname) %>% 
  summarize(iso_num = n()) %>% 
  select(bioname, iso_num)

total_vote<- key_votes %>%  #Total number of votes to rollcalls, excluding abstentions, votes of present, etc. 
  filter(type_1 == "gov") %>% 
  filter(wing != "UNK") %>% 
  filter(cast_code != "nv") %>% 
  group_by(bioname) %>% 
  summarize(total_vote = n()) %>% 
  left_join(total_iso, by =c("bioname"))

total_vote[is.na(total_vote)] <- "0"

congress_num<- Hall_all_members %>% 
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  select(icpsr) %>% 
  group_by(icpsr) %>% 
  summarize(congress_num = n())

total_vote$total_vote <- as.numeric(as.character(total_vote$total_vote))
total_vote$iso_num <- as.numeric(as.character(total_vote$iso_num))

last_cong<- Hall_all_members %>%  #The last congress served by each individual
  group_by(icpsr) %>% 
  do(tail(., 1)) %>% 
  filter(congress > 73) %>% 
  select(icpsr, congress)

first_cong<- Hall_all_members %>% #The first congress served by each individual
  group_by(icpsr) %>% 
  do(head(., 1)) %>% 
  filter(congress < 103) %>% 
  select(icpsr, congress)

names(first_cong)[2]<-"first_cong"

names(last_cong)[2]<-"last_cong"

total_iso_session<- key_votes %>% 
  filter(type_1 == "gov") %>% 
  filter(cast_code == "iso") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(iso_num = n()) %>% 
  select(icpsr, party_code, chamber, congress, iso_num)

total_vote_session<- key_votes %>% 
  filter(type_1 == "gov") %>% 
  filter(cast_code != "nv") %>% 
  group_by(icpsr, party_code, chamber, congress) %>% 
  summarize(total_vote = n()) %>% 
  select(icpsr, party_code,chamber, total_vote, congress) %>% 
  left_join(total_iso_session, by = c("icpsr", "congress", "chamber", "party_code"))

total_vote_session[is.na(total_vote_session)] <- "0"

total_vote_session$total_vote <- as.numeric(as.character(total_vote_session$total_vote))
total_vote_session$iso_num <- as.numeric(as.character(total_vote_session$iso_num))


total_vote_session_1<- total_vote_session %>% 
  mutate(opp_sess = iso_num / total_vote) %>% 
  select(icpsr, congress, party_code, chamber, opp_sess) %>% 
  spread(congress, opp_sess)

total_vote_session_1[is.na(total_vote_session_1)] <- "X"

gov_leaderboard<- Hall_all_members %>%
  filter(wing != "UNK") %>% 
  filter(congress > 73) %>% 
  filter(congress < 103) %>%
  filter(chamber != "President") %>% 
  left_join(regions, by = "state_abbrev") %>% 
  distinct(icpsr, bioname, chamber, party_code, wing, nominate_dim1, nominate_dim2, state_abbrev, born, died, gen, region) %>% 
  left_join(total_vote, by = c("bioname")) %>% 
  mutate(gov_oppo_per = iso_num / total_vote) %>% 
  arrange(desc(gov_oppo_per)) %>% 
  left_join(first_cong, by = "icpsr") %>% 
  left_join(last_cong, by = "icpsr") %>% 
  left_join(congress_num, by = "icpsr") %>% 
  drop_na(gov_oppo_per) %>% 
  left_join(total_vote_session_1, by = c("icpsr", "party_code", "chamber"))

write.csv(leaderboard, "clean_data/gov_leaderboard.csv")

test<- leaderboard %>% 
  filter(oppo_per > .5) %>% 
  filter(congress_num > 1) %>% 
  filter(last_cong > 78) %>% 
  filter(party_code == "Republican")
