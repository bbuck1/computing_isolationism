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

Hall_votes<- read.csv("raw_data/Hall_votes.csv")

notes<- read.csv("isolationists_fate_notes.csv")

Hall_members_79<- Hall_all_members %>%
  filter(congress == "79")

Hall_votes_79<- Hall_votes %>% 
  filter(congress == "79")

Hall_votes_79_joined<- Hall_members_79 %>%
  left_join(Hall_votes_79, by = 'icpsr') %>% 
  filter( rollnumber == '94'|
           rollnumber == '96'|
           rollnumber == '195'|
           rollnumber == '162'|
           rollnumber == '176'|
           rollnumber == '155'|
           rollnumber == '5'|
           rollnumber == '17'|
           rollnumber == '31'|
           rollnumber == '47'|
           rollnumber == '100'|
           rollnumber == '194') %>%
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_80<- Hall_all_members %>% 
  filter(congress == "80")

Hall_votes_80<- Hall_votes %>% 
  filter(congress == "80")

Hall_votes_80_joined<- Hall_members_80 %>%
  left_join(Hall_votes_80, by = 'icpsr') %>%
  filter(rollnumber == '34'|
           rollnumber == '137'|
           rollnumber == '112'|
           rollnumber == '37'|
           rollnumber == '51'|
           rollnumber == '82'|
           rollnumber == '109'|
           rollnumber == '147'|
           rollnumber == '154'|
           rollnumber == '158'|
           rollnumber == '159') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_81<- Hall_all_members %>% 
  filter(congress == "81")

Hall_votes_81<- Hall_votes %>% 
  filter(congress == "81")

Hall_votes_81_joined<- Hall_members_81 %>%
  left_join(Hall_votes_81, by = 'icpsr') %>%
  filter(rollnumber == '208'|
           rollnumber == '43'|
           rollnumber == '105'|
           rollnumber == '201'|
           rollnumber == '214'|
           rollnumber == '248'|
           rollnumber == '7'|
           rollnumber == '43'|
           rollnumber == '98'|
           rollnumber == '123'|
           rollnumber == '131'|
           rollnumber == '196'|
           rollnumber == '190'|
           rollnumber == '269') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)


Hall_members_82<- Hall_all_members %>% 
  filter(congress == "82")

Hall_votes_82<- Hall_votes %>% 
  filter(congress == "82")

Hall_votes_82_joined<- Hall_members_82 %>%
  left_join(Hall_votes_82, by = 'icpsr') %>%
  filter(rollnumber == '88'|
           rollnumber == '99'|
           rollnumber == '100'|
           rollnumber == '30'|
           rollnumber == '4'|
           rollnumber == '35'|
           rollnumber == '37'|
           rollnumber == '85'|
           rollnumber == '88'|
           rollnumber == '100'|
           rollnumber == '138'|
           rollnumber == '153'|
           rollnumber == '155') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_83<- Hall_all_members %>% 
  filter(congress == "83")

Hall_votes_83<- Hall_votes %>% 
  filter(congress == "83")

Hall_votes_83_joined<- Hall_members_83 %>%
  left_join(Hall_votes_83, by = 'icpsr') %>%
  filter(rollnumber == '66'|
           rollnumber == '117'|
           rollnumber == '107'|
           rollnumber == '43'|
           rollnumber == '141'|
           rollnumber == '99'|
           rollnumber == '38'|
           rollnumber == '50'|
           rollnumber == '67'|
           rollnumber == '90'|
           rollnumber == '109'|
           rollnumber == '144') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)


Hall_members_84<- Hall_all_members %>% 
  filter(congress == "84")

Hall_votes_84<- Hall_votes %>% 
  filter(congress == "84")

Hall_votes_84_joined<- Hall_members_84 %>%
  left_join(Hall_votes_84, by = 'icpsr') %>%
  filter(rollnumber == '60'|
           rollnumber == '79'|
           rollnumber == '91'|
           rollnumber == '101'|
           rollnumber == '2'|
           rollnumber == '15'|
           rollnumber == '34'|
           rollnumber == '47'|
           rollnumber == '55'|
           rollnumber == '59'|
           rollnumber == '108'|
           rollnumber == '128') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)


Hall_members_85<- Hall_all_members %>% 
  filter(congress == "85")

Hall_votes_85<- Hall_votes %>% 
  filter(congress == "85")

Hall_votes_85_joined<- Hall_members_85 %>%
  left_join(Hall_votes_85, by = 'icpsr') %>%
  filter(rollnumber == '30'|
           rollnumber == '53'|
           rollnumber == '70'|
           rollnumber == '79'|
           rollnumber == '104'|
           rollnumber == '189'|
           rollnumber == '192'|
           rollnumber == '28'|
           rollnumber == '3'|
           rollnumber == '100'|
           rollnumber == '156'|
           rollnumber == '65'|
           rollnumber == '131'|
           rollnumber == '158'|
           rollnumber == '104'|
           rollnumber == '39'|
           rollnumber == '139') %>% 
  
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_86<- Hall_all_members %>% 
  filter(congress == "86")

Hall_votes_86<- Hall_votes %>% 
  filter(congress == "86")

Hall_votes_86_joined<- Hall_members_86 %>%
  left_join(Hall_votes_86, by = 'icpsr') %>%
  filter(rollnumber == '4'|
           rollnumber == '13'|
           rollnumber == '15'|
           rollnumber == '63'|
           rollnumber == '97'|
           rollnumber == '179'|
           rollnumber == '159'|
           rollnumber == '53'|
           rollnumber == '159'|
           rollnumber == '149'|
           rollnumber == '127'|
           rollnumber == '86'|
           rollnumber == '55'|
           rollnumber == '28'|
           rollnumber == '161') %>% 
  
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)


Hall_members_87<- Hall_all_members %>% 
  filter(congress == "87")

Hall_votes_87<- Hall_votes %>% 
  filter(congress == "87")

Hall_votes_87_joined<- Hall_members_87 %>%
  left_join(Hall_votes_87, by = 'icpsr') %>%
  filter(rollnumber == '21'|
           rollnumber == '29'|
           rollnumber == '30'|
           rollnumber == '32'|
           rollnumber == '63'|
           rollnumber == '75'|
           rollnumber == '92'|
           rollnumber == '116'|
           rollnumber == '132'|
           rollnumber == '181'|
           rollnumber == '323'|
           rollnumber == '234'|
           rollnumber == '87'|
           rollnumber == '107'|
           rollnumber == '113'|
           rollnumber == '173'|
           rollnumber == '200'|
           rollnumber == '138'|
           rollnumber == '43'|
           rollnumber == '163'|
           rollnumber == '74'|
           rollnumber == '147'|
           rollnumber == '108'|
           rollnumber == '141'|
           rollnumber == '218'|
           rollnumber == '215') %>% 
group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)



Hall_members_88<- Hall_all_members %>% 
  filter(congress == "88")

Hall_votes_88<- Hall_votes %>% 
  filter(congress == "88")

Hall_votes_88_joined<- Hall_members_88 %>%
  left_join(Hall_votes_88, by = 'icpsr') %>%
  filter(rollnumber == '132'|
           rollnumber == '197'|
           rollnumber == '180'|
           rollnumber == '171'|
           rollnumber == '164'|
           rollnumber == '134'|
           rollnumber == '130'|
           rollnumber == '116'|
           rollnumber == '110'|
           rollnumber == '100'|
           rollnumber == '95'|
           rollnumber == '73'|
           rollnumber == '62'|
           rollnumber == '8'|
           rollnumber == '6'|
           rollnumber == '61'|
           rollnumber == '170') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_89<- Hall_all_members %>% 
  filter(congress == "89")

Hall_votes_89<- Hall_votes %>% 
  filter(congress == "89")

Hall_votes_89_joined<- Hall_members_89 %>%
  left_join(Hall_votes_89, by = 'icpsr') %>%
  filter(rollnumber == '120'|
           rollnumber == '214'|
           rollnumber == '215'|
           rollnumber == '284'|
           rollnumber == '285') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

Hall_members_90<- Hall_all_members %>% 
  filter(congress == "90")

Hall_votes_90<- Hall_votes %>% 
  filter(congress == "90")

Hall_votes_90_joined<- Hall_members_90 %>%
  left_join(Hall_votes_90, by = 'icpsr') %>%
  filter(rollnumber == '201'|
           rollnumber == '202'|
           rollnumber == '390'|
           rollnumber == '442') %>% 
  group_by(icpsr, bioname, state_abbrev, district_code, party_code, congress.x, nominate_dim1, nominate_dim2)

key_votes_79_90<- rbind(Hall_votes_79_joined, Hall_votes_80_joined, Hall_votes_81_joined, 
                        Hall_votes_82_joined, Hall_votes_83_joined, Hall_votes_84_joined,
                        Hall_votes_85_joined, Hall_votes_86_joined, Hall_votes_87_joined,
                        Hall_votes_88_joined, Hall_votes_88_joined, Hall_votes_88_joined,
                        Hall_votes_89_joined, Hall_votes_90_joined)


key_votes_79_88_new_joined<- key_votes_79_90_new %>% 
left_join(key_votes_79_88_list, by = c('congress', "rollnumber")) %>% 
  left_join(notes, by = 'icpsr')

#write.csv(key_votes_79_88_new_joined, "key_votes_79_88_new_joined.csv")

#csv is then edited in OpenRefine. "Cohort" column is added. For reverse votes cast codes 1-3, “iso” 
#is added to the new column, 4-6, “int,” 7-9, 
#“NV.” For non-reverse votes, 1-3, “int,”, 4-6, “iso.” Non votes are coded the same.

key_votes_distinct<- key_votes_79_88_new_joined %>% 
  left_join(key_votes_79_88_list, by = c('congress', "rollnumber")) %>% 
  left_join(notes, by = 'icpsr') %>% 
distinct(icpsr, bioname, congress, chamber, state_abbrev, party_code, district_code, nominate_dim1, 
         nominate_dim2, rollnumber, bill_no, bill_name, reverse_vote, cohort, cast_code, type, aid_type) %>% 
  filter(state_abbrev != "USA")

#write.csv(key_votes_distinct, "key_votes_new.csv")


