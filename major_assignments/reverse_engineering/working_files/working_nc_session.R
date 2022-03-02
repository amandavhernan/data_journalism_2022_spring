library(dplyr)
library(tidyverse)

# read/store data
nc_session_ban <- read.csv("nc_session_donations.csv")

# fix amount column
nc_session_ban <- nc_session_ban %>% 
  mutate(amount=as.numeric(parse_number(amount)))

View(nc_session_ban)

# fix date column
