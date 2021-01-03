# Part 03: descriptive analyses


# information content per session -----------------------------------------
run_data %>%
  filter(details & !map) %>% nrow # number of times ran on treadmill with watch/hr_monitor
run_data %>%
  filter(!map) %>% nrow # number of times ran on treadmill
run_data %>%
  filter(map) %>% nrow # number of times ran outside
run_data %>%
  filter(details) %>% nrow # number of times ran with details
run_data %>%
  filter(details) %>% nrow # number of times ran with details
run_data %>%
  filter(map & !hr_details) %>% nrow # number of times ran on map but without hr

# yearly stats ------------------------------------------------------------

run_data %>% mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n())

run_data %>% filter(Distance > 10 & EleUp < 100 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))

run_data %>% filter(Distance > 25 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))

run_data %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), DIST_SUM = sum(Distance), sum(EleUp), sum(Duration/60))

run_data %>%
  mutate(Year = year(Date), Week = isoweek(Date)) %>% group_by(Year, Week) %>% 
  summarise(N = n(), Week_Dist = sum(Distance)) %>% 
  filter(Week_Dist > 60) %>% 
  group_by(Year) %>% summarise(N = n())

run_data %>%
  mutate(Year = year(Date), Month = month(Date)) %>% group_by(Year, Month) %>% 
  summarise(N = n(), Month_Dist = sum(Distance)) %>% 
  filter(Month_Dist > 250) %>% 
  group_by(Year) %>% summarise(N = n())


# hr analysis -------------------------------------------------------------
run_data %>% filter(Pace > (5 - 10/60) & Pace < (6 + 10/60) & Distance > 10 & Distance < 20 & EleUp < 100 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))
run_data %>% filter(Pace > (6 - 10/60) & Pace < (7 + 10/60) & Distance > 10 & Distance < 20 & EleUp < 100 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))
run_data %>% filter(Pace > (6 - 10/60) & Pace < (6 + 10/60) & Distance > 10 & Distance < 20 & EleUp < 100 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))
run_data %>% filter(Pace > (6.5 - 10/60) & Pace < (6.5 + 10/60) & Distance > 10 & Distance < 20 & EleUp < 100 & !is.na(HRavg)) %>% 
  mutate(Year = year(Date)) %>% group_by(Year) %>% 
  summarise(N = n(), HR_avg = mean(HRavg))


# fastest activties -------------------------------------------------------
run_data %>% arrange(BestPace1000)
run_data %>% arrange(BestPace5000)
run_data %>% arrange(BestPace10000)
run_data %>% arrange(BestPace21097)

run_data %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  arrange(BestPace1000) %>% 
  slice(1) %>% 
  summarise(BestTime = seconds.to.hms(60*BestPace1000))

run_data %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  arrange(BestPace5000) %>% 
  slice(1) %>%
  summarise(BestTime = seconds.to.hms(60*BestPace5000*5), Date = Date, Session)

run_data %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  arrange(BestPace10000) %>% 
  slice(1) %>% 
  summarise(BestTime = seconds.to.hms(60*BestPace10000*10), Date = Date)

run_data %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  arrange(BestPace21097) %>% 
  slice(1) %>% 
  summarise(BestTime = seconds.to.hms(60*BestPace21097*21.097), Date = Date)


