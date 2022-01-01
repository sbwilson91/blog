## read csv

library(tidyverse)

times <- read_csv("Strava_activity_log.csv", col_names = FALSE)

# appropriately label columns

head(times)
colnames(times) <- c("Date", "Advanced_Activity", "Activity", "Metres", "Long_time", "Seconds", "Info_1", "Info_2")

times %>% filter(Activity == "Run") %>% ggplot() +
  geom_point(aes(Metres, Seconds))

times$KPH <- (times$Metres/1000)/(times$Seconds/3600)

times %>% filter(Activity == "Run") %>% ggplot() +
  geom_point(aes(Metres, KPH))

times$MPK <- (times$Seconds/60)/(times$Metres/1000)

times %>% filter(Activity == "Run", MPK < 10, Metres > 2000, Metres < 2300) %>% ggplot() +
  geom_point(aes(Metres, MPK))

times$MDY <- str_split(times$Date, pattern = " at ", simplify = T)[,1]
times$MDY <- lubridate::mdy(times$MDY)

times %>% filter(Activity == "Run", MDY > "2021-06-01") %>% ggplot() +
  geom_point(aes(MDY, KPH, colour = Metres, size = 5)) +
  theme_classic()

times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres < 2200, Metres > 2000) %>% ggplot() +
  geom_point(aes(MDY, KPH, size = 5)) +
  theme_classic()

times %>% filter(Activity == "Run", MDY > "2021-06-01") %>%
  mutate(Cumulative = cumsum(Metres)/1000) %>%  ggplot() +
  geom_point(aes(MDY, Cumulative)) +
  geom_line(aes(MDY, Cumulative)) +
  theme_classic()

  
times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres < 2200, Metres > 2000)  %>%
  mutate(CumulativeSum = cumsum(Metres)/1000,
         CumulativeMean = cummean(Metres)/1000,
         MPM = Metres/(Seconds/60)) %>%  ggplot() +
  geom_point(aes(MDY, MPM)) +
  geom_line(aes(MDY, MPM)) +
  theme_classic()


times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres < 2200, Metres > 2000)  %>%
  mutate(MPM = Metres/(Seconds/60),
         CumulativeSum = cumsum(Metres)/1000,
         Mean_MPM = cummean(MPM)) %>% ggplot() +
  geom_point(aes(MDY, MPM)) +
  geom_line(aes(MDY, Mean_MPM)) +
  theme_classic()

times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres>1000)  %>%
  mutate(MPM = Metres/(Seconds/60),
         CumulativeSum = cumsum(Metres)/1000,
         Mean_MPM = cummean(MPM)) %>% ggplot() +
  geom_point(aes(MDY, MPM)) +
  geom_line(aes(MDY, Mean_MPM)) +
  theme_classic()

times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres>2000)  %>%
  mutate(MPM = Metres/(Seconds/60),
         CumulativeSum = cumsum(Metres)/1000,
         Mean_MPM = cummean(MPM),
         Group = cut(Metres, c(0, 2200, 5000, 5200, 10000, Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k"))) %>% ggplot() +
  geom_point(aes(MDY, MPM)) +
  #geom_line(aes(MDY, Mean_MPM)) +
  facet_wrap(facets = "Group", scales = "free_y") +
  theme_bw()

times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres>2000)  %>%
  mutate(MPK = (Seconds/60)/(Metres/1000),
         CumulativeSum = cumsum(Metres)/1000,
         Mean_MPK = cummean(MPK),
         Group = cut(Metres, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(MDY, MPK)) +
  geom_smooth(aes(MDY, MPK), se = F, method = "lm") +
  facet_wrap(facets = "Group", scales = "free_y") +
  theme_bw()

times %>% filter(Activity == "Run", MDY > "2021-06-01", Metres>2000)  %>%
  mutate(MPK = (Seconds/60)/(Metres/1000),
         CumulativeSum = cumsum(Metres)/1000,
         Mean_MPK = cummean(MPK),
         Group = cut(Metres, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(MDY, Seconds)) +
  #geom_line(aes(MDY, Mean_MPM)) +
  facet_wrap(facets = "Group", scales = "free_y") +
  theme_bw()

times %>% filter(Activity == "Run", MDY > "2021-06-01") %>%
  mutate(Cumulative = cumsum(Metres)/1000) %>%  ggplot() +
  geom_point(aes(MDY, (Metres/1000))) +
  geom_line(aes(MDY, Cumulative)) +
  theme_classic()


# summary of all activity in 2021
map(c("Run" , "Ride"), 
    ~times %>% filter(Activity == .x, MDY > "2021-01-01") %>%
      mutate(Cumulative = cumsum(Metres)/1000) %>%  ggplot() +
      geom_point(aes(MDY, (Metres/1000))) +
      geom_line(aes(MDY, Cumulative)) +
      ggtitle(.x) +
      theme_classic()) %>% patchwork::wrap_plots()



