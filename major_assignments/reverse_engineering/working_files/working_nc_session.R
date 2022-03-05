library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)

## data prep

# read/store data
nc_session_donations <- read.csv("nc_session_donations.csv")

# fix amount column
nc_session_donations <- nc_session_donations %>% 
  mutate(amount=as.numeric(parse_number(amount)))

# fix date column
nc_session_donations <- nc_session_donations %>%
  mutate(date = mdy(date)) 

# check data
View(nc_session_donations)

## end

# R.H. Barringer's board chair, Jasie Barringer, donated another $8,200 in-session, and Teresa Craig kicked in 
# another $5,200 to Berger. The North Carolina Beer & Wine Wholesalers Association PAC gave $18,400 during the week 
# before session and on opening day.

# Jasie Barringer -> $8,200
nc_session_donations %>%
  filter(name.clean == "JASIE S BARRINGER") %>%
  filter(date >= "2018-05-16" & date <= "2018-06-30") %>%
  group_by(name.clean) %>%
  summarize(total_amount = sum(amount))

# Teresa Craig -> $5,200
nc_session_donations %>%
  filter(name.clean == "TERESA S CRAIG") %>%
  filter(date >= "2018-05-16" & date <= "2018-06-30") %>%
  group_by(name.clean) %>%
  summarize(total_amount = sum(amount))

# North Carolina Beer & Wine Wholesalers Association PAC -> $18,400
nc_session_donations %>%
  filter(name.clean == "NC BEER & WINE WHOLESALERS ASSOC PAC") %>%
  filter(date >= "2018-05-09" & date <= "2018-05-16") %>%
  group_by(name.clean) %>%
  summarize(total_amount = sum(amount))

# Smithfield Foods' PAC gave another $10,000 to various legislators and the state Republican and Democratic parties, 
# some just before, some just after the session began. The North Carolina Pork Council gave $10,000 during session to the 
# state Republican Party and $5,000 right at the session's start to the Democrats.

nc_session_donations %>%
  filter(name.clean == "SMITHFIELD FOODS PAC")

# Senate President Pro Tem Phil Berger's campaign alone raised nearly $222,000 during session. Twenty-three people gave him 
# the maximum allowed: $5,200.

nc_session_donations %>%
  filter(committee_name == "PHILIP E BERGER COMM") %>%
  filter(date > "2018-05-16" & date < "2018-06-30") %>%
  group_by(committee_name) %>%
  summarize(total_amount = sum(amount)) # confirms berger received almost $222,000

berger_max_donors <- nc_session_donations %>%
  filter(committee_name == "PHILIP E BERGER COMM") %>%
  filter(date > "2018-05-16" & date < "2018-06-30") %>% 
  filter(amount == 5200) # confirms 23 people donated max amount to berger

# graf 4

mark_craig_donations <- nc_session_donations %>%
  filter(name.clean == "MARK R CRAIG") %>%
  filter(date > "2018-05-16" & date < "2018-06-30") 

sum(mark_craig_donations$amount) # confirms donation total over $30,000

individual_top_donors <- nc_session_donations %>%
  filter(date > "2018-05-16" & date < "2018-06-30") %>%
  filter(transaction_type == "Individual") %>%
  group_by(name.clean) %>%
  summarise(total_donation_amount = sum(amount)) %>%
  arrange(desc(total_donation_amount))

# graf 5
  
first_wk_top_donors <- nc_session_donations %>%
  filter(date >= "2018-05-16" & date <= "2018-05-23") %>%
  group_by(name.clean) %>%
  summarise(total_donation_amount = sum(amount)) %>%
  arrange(desc(total_donation_amount)) # confirms Lindberg, Hoffman, and Eli Research LLC as top donors during first week
