library(tidyverse)
library(lubridate)

options(scipen=9999)

#1. Read in data----
files <- list.files("Data", full.names = TRUE)
raw <- purrr::map(.x=files, ~read.csv(.x)) %>% 
  data.table::rbindlist()

#2. Wrangle----
#Fix NA clients
#Format date & time columns
#Filter to period of interest (Feb 2020 - Jan 2023)
dat <- raw %>% 
  mutate(date = ymd(Start.date),
         year = year(date),
         month = month(date),
         minutes = as.numeric(str_sub(Duration, 1, 2))*60 + as.numeric(str_sub(Duration, 4, 5)) + as.numeric(str_sub(Duration, 7, 8))/60,
         days = minutes/60/7.5) %>% 
  dplyr::mutate(client = case_when(Client=="" & Project %in% c("AB Coordination - Advertising", "Annual Report", "Nightjar News", "Program Transfer", "Program Management") ~ "Side Project",
                                   Client=="" & Project %in% c("Applications", "Proposal") ~ "Job Searching",
                                   Client=="" & Project %in% c("Time Management") ~ "PhD",
                                   Client=="" & Project=="Emails" & date < ymd("2021-09-01") ~ "PhD",
                                   Client=="" & Project=="Emails" & date >= ymd("2021-09-01") ~ "SMBC",
                                   Client=="" & Project=="ARU models" ~ "ABMI",
                                   Client=="" & Project=="" & Description=="Emails" ~ "PhD",
                                   Client=="" & Project=="" & Description=="ABMI application" ~ "Job Searching",
                                   Client=="" & Project=="" & Description %in% c("Defense presentation", "Trying to fix github") ~ "PhD",
                                   !is.na(Client) ~ Client)) %>% 
  dplyr::select(client, year, month, date, days, minutes) %>% 
  dplyr::filter(client!="",
                date >= ymd("2020-02-01"),
                date <= ymd("2023-01-31"))
  
#3. Group by month & client----
dat.month <- dat %>% 
  group_by(year, client, month) %>% 
  summarize(days = sum(days)) %>% 
  ungroup() %>% 
  arrange(year, month, client) %>% 
  mutate(days.round = ceiling(days))

#4. Calculate number of off days----
