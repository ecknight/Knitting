library(tidyverse)
library(lubridate)
library(lares)

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
                                   Client=="SMBC" & date <= ymd("2021-09-01") ~ "PhD",
                                   Client=="BAM" & date <= ymd("2022-06-01") ~ "PhD",
                                   Client=="GNN" ~ "Side Project",
                                   !is.na(Client) ~ Client)) %>% 
  dplyr::select(client, year, month, date, days, minutes) %>% 
  dplyr::filter(client!="",
                date >= ymd("2020-02-01"),
                date <= ymd("2023-01-31"))

#3. Holidays----
holidaydates <- holidays("Canada", years=c(2020, 2021, 2022, 2023)) %>% 
  dplyr::filter(holidayname %in% c("Family Day", "Good Friday", "Easter Monday", "Victoria Day", "Canada Day", "Labour Day", "Thanksgiving", "Remembrance Day", "National Day for Truth and Reconciliation", "Christmas") | holidaytype %in% c("Statutory Holiday", "Federal Employees Holiday")) %>% 
  dplyr::select(holiday) %>% 
  unique()
  
#4. Make sequences of all days----
alldates <- data.frame(date = seq(ymd("2020-02-01"), ymd("2023-01-31"), by="days")) %>% 
  mutate(day = weekdays(date),
         month = month(date),
         year = year(date),
         dayclass = ifelse(day %in% c("Saturday", "Sunday", holidaydates$holiday, ymd("2021-01-01", "2022-01-01", "2023-01-01")), "weekend", "weekday")) 

#5. Group by month, client, & dayclass----
#split ABMI/BAM time in two
dat.month <- dat %>% 
  left_join(alldates) %>% 
  group_by(year, client, month, dayclass) %>% 
  summarize(days = sum(days)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=client, values_from=days, values_fill=0) %>% 
  rename(ABMIBAM = 'ABMI/BAM') %>% 
  mutate(ABMI = ifelse(ABMIBAM > 0, ABMI + ABMIBAM/2, ABMI),
         BAM = ifelse(ABMIBAM > 0, BAM + ABMIBAM/2, BAM)) %>% 
  dplyr::select(-ABMIBAM) %>% 
  pivot_longer(Consulting:BAM, names_to="client", values_to="days") %>% 
  dplyr::filter(days > 0) %>% 
  group_by(year, month, dayclass) %>% 
  mutate(totaldays=sum(days)) %>% 
  ungroup() %>% 
  mutate(totaldaysround = ceiling(totaldays),
         daysround = ceiling(days)) %>% 
  arrange(year, month, dayclass, -days) %>% 
  group_by(year, month, dayclass) %>% 
  mutate(daysum = cumsum(daysround)) %>% 
  ungroup() %>% 
  mutate(use = ifelse(daysum <= totaldaysround, 1, 0),
         use = ifelse(client=="Side Project" & days > 0.5, 1, use))

#6. Compare number of days----
dat.month.use <- dat.month %>% 
  dplyr::filter(use==1)
sum(dat.month$days)
sum(dat.month.use$daysround)
#Pretty good

#7. Compare proportion of time----
dat.month %>% 
  group_by(client) %>% 
  summarize(sum=sum(days))
dat.month.use %>% 
  group_by(client) %>% 
  summarize(sum=sum(daysround))
#Looks good

#8. Calculate number of off days----
totaltime <- alldates %>% 
  group_by(year, month, dayclass) %>% 
  summarize(totaldays=n()) %>% 
  ungroup()

dat.off <- dat.month %>% 
  group_by(year, month, dayclass) %>% 
  summarize(ondays = sum(daysround)) %>% 
  ungroup() %>% 
  left_join(totaltime) %>% 
  mutate(offdays = totaldays - ondays) %>% 
  dplyr::select(-totaldays, -ondays) %>% 
  pivot_wider(names_from=dayclass, values_from=offdays, values_fill=0) %>% 
  mutate(weekday = ifelse(weekend < 0, weekday + weekend, weekday),
         weekend = ifelse(weekend < 0, 0, weekend),
         weekend = ifelse(weekday < 0, weekend + weekday, weekend),
         weekday = ifelse(weekday < 0, 0, weekday),
         weekend = ifelse(weekend < 0, 0, weekend)) %>% 
  pivot_longer(weekday:weekend, values_to="offdays", names_to="dayclass")

#9. Put it all together----
dat.all <- dat.off %>% 
  rename(days = offdays) %>% 
  mutate(client = "Off") %>% 
  rbind(dat.month %>% 
          dplyr::filter(use==1) %>% 
          dplyr::select(year, month, dayclass, daysround, client) %>% 
          rename(days = daysround)) %>% 
  arrange(year, month, dayclass, client) %>% 
  mutate(panel = case_when(year==2020 | (year==2021 & month==1) ~ 1,
                           (year==2021 & month!=1) | (year==2022 & month==1) ~ 2,
                           (year==2022 & month!=1) | year==2023 ~ 3),
         month = ifelse(month==1, 13, month),
         client = factor(client, levels=c("Off", "PhD", "Side Project", "Job Searching", "Consulting", "SMBC", "ABMI", "BAM")),
         dayclass = factor(dayclass, levels=c("weekend", "weekday")))

#10. Check number of days----
dat.all %>% 
  group_by(panel) %>% 
  summarize(days = sum(days)) %>% 
  ungroup()
#something has gone sideways...

#11. Add colours----
clrs <- data.frame(unique(dat.all$client),
                   colour = c("cornsilk2", "darkgoldenrod3", "purple4", "firebrick4", "olivedrab4", "sienna", "dodgerblue3", "darkgreen"))

#12. Try plotting----
ggplot(dat.all) +
  geom_bar(aes(x=month, y=days, fill=client), colour="black", stat="identity") +
  scale_fill_manual(values=clrs$colour) +
  facet_wrap(~panel) +
  theme_bw()

#13. Calculate rows per client----
clientrows <- dat.all %>% 
  mutate(rows = ifelse(dayclass=="weekend", days*2, days)) %>% 
  group_by(client) %>% 
  summarize(rows = sum(rows)) %>% 
  ungroup()
clientrows

#14. Calculate rows per panel----
panelrows <- dat.all %>% 
  group_by(panel, dayclass) %>% 
  summarize(rows = sum(days)) %>% 
  ungroup()
panelrows