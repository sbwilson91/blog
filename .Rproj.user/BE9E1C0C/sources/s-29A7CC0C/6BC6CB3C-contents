# strava download

strava <- read_csv("~/export_40721577/activities.csv")
colnames(strava)

strava$MDYHMS <- lubridate::mdy_hms(strava$`Activity Date`, tz = "Australia/Sydney") +
  lubridate::hours(11)# need to add 11 hours
strava$MDY <- str_split(strava$`Activity Date`, pattern = ", ") lubridate::mdy(strava$`Activity Date`)
strava$HMS <- format(strava$MDYHMS, format = "%H:%M:%S")

# we want moving time in minutes, we also want a distance in km as well as m

strava$`Moving Time (Min)` <- strava$`Moving Time`/60
strava$`Distance (M)` <- strava$Distance...17
strava$`Distance (KM)` <- strava$Distance...7

# clean up some other values

strava$MaxHR <- strava$`Max Heart Rate...8`

# filter out any activity under 1km (almost certain these are unwanted or mistaken events)

strava <- strava %>% filter(`Distance (M)` > 999)

strava.run <- strava %>% filter(`Activity Type` == "Run")
strava.ride <- strava %>% filter(`Activity Type` == "Ride")

## simple vis of rides

strava.ride %>% ggplot() +
  geom_point(aes(`Distance (KM)`, `Moving Time (Min)`))

# simple vis of runs
strava.run %>% ggplot() +
  geom_point(aes(`Distance (KM)`, `Moving Time (Min)`))

# plot max HR compared to moving time and distance

strava.run %>% ggplot() +
  geom_point(aes(`Distance (KM)`, MaxHR))

strava.run %>% ggplot() +
  geom_point(aes(`Moving Time (Min)`, MaxHR))

ggpairs(strava.run[, c("Moving Time (Min)", "Distance (KM)", "MaxHR", "Max Speed", "Relative Effort...9")])


strava.run %>% filter(MDYHMS > "2021-06-01", `Distance (M)`>2000)  %>%
          mutate(Group = cut(`Distance (M)`, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(MDYHMS, `Average Speed`)) +
  #geom_line(aes(MDYHMS, Mean_MPM)) +
  geom_smooth(aes(MDYHMS, `Average Speed`), se = F, method = "lm") +
  facet_wrap(facets = "Group") +
  theme_bw()


a <- strava.run %>% filter(MDYHMS > "2021-06-01", `Distance (M)`>2000)   %>%
  mutate(Group = cut(`Distance (M)`, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(MDYHMS, `Average Speed`, colour = Group), size = 5, alpha = 0.6) +
  #geom_line(aes(MDYHMS, Mean_MPM)) +
  geom_smooth(aes(MDYHMS, `Average Speed`, group = Group, colour = Group), se = F, method = "lm") +
  xlab("Date") + ylab("Average Speed (KM/H)") +
  #ggtitle("2021 Runs", "Average speed (KM/H) with linear regression per group")
  theme_bw()

# linear model of speed change across all runs, by distance
b <- strava.run %>% filter(MDYHMS > "2021-06-01", `Distance (M)`>2000)   %>%
  mutate(Group = cut(`Distance (M)`, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(MDYHMS, `Average Speed`, colour = Group, fill = Group), alpha = 0.6) +
  #geom_line(aes(MDYHMS, Mean_MPM)) +
  geom_smooth(aes(MDYHMS, `Average Speed`, group = Group, colour = Group), se = F, method = "lm") +
  facet_wrap(facets = "Group", scales = "free_y", ncol = 2) +
  xlab("Date") + ylab("Average Speed (KM/H)") +
  theme_bw()

# time of runs and rides
strava.run %>% filter(MDYHMS > "2021-06-01", `Distance (M)`>2000)   %>%
  mutate(Group = cut(`Distance (M)`, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+"))) %>% ggplot() +
  geom_point(aes(HMS, `Distance (M)`, colour = Group)) +
  #scale_y_time(name = "Time of Day") +
  #geom_line(aes(MDYHMS, Mean_MPM)) +
  facet_wrap(facets = "Group") +
  theme_bw()
  
a + b + patchwork::plot_annotation(title = "2021 Runs (Aug-Dec)", theme = theme(plot.title = element_text(hjust = 0.5),
                                                                                plot.subtitle = element_text(hjust = 0.5)),
                                   subtitle = "Took up running at the beginning of Aug. Charts show Average Speed (KM/H) with Linear Regression per group; 2k, 5k and 10k are usually effort runs") 

bbrun <- (strava.run %>% filter(`Activity ID` == 6343787920))$MDYHMS
c <- strava.run %>% filter(MDYHMS > "2021-06-01", `Distance (M)`>1999)   %>%
  mutate(Group = cut(`Distance (M)`, c(0, 2200, 4999, 5200, 9999, 10500,  Inf),
                     labels = c("2k", "2k-5k", "5k", "5k-10k", "10k", "10k+")),
         TotalDist = cumsum(`Distance (KM)`)) %>% ggplot() +
  #geom_text(
  #  data = (strava.run %>% filter(`Activity ID` == 6343787920)), aes(MDYHMS, `Distance (KM)`),nudge_y = 5
  #  colour = "red",
  #  size = 7
  #) +
  #annotate(
  #  geom = "curve", x = "2021-08-01", y = 15, xend = "2021-12-05", yend = 11, 
  #  curvature = .3, arrow = arrow(length = unit(2, "mm"))
  #) +
  #annotate(geom='text', x=lastDate,y=lastDateY, vjust=-2, label="China")
  geom_point(aes(MDYHMS, `Distance (KM)`, colour = Group), size = 5, alpha = 0.6) +
  #geom_line(aes(MDYHMS, Mean_MPM)) +
  #geom_smooth(aes(MDYHMS, `Average Speed`), se = F, method = "lm") +
  geom_line(aes(MDYHMS, TotalDist/30), colour = "green", size = 2) +
  scale_y_continuous(
    # Features of the first axis
    name = "Run Distance (KM)",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*30, name="Total Distance (KM)")
  ) +
  xlab("Date") + ylab("Distance (KM)") +
  coord_cartesian(ylim = c(0, 15)) +
  #ggtitle("2021 Runs", "Average speed (KM/H) with linear regression per group")
  theme_bw()

c + b + patchwork::plot_annotation(title = "2021 Runs (Aug-Dec)", theme = theme(plot.title = element_text(hjust = 0.5),
                                                                                plot.subtitle = element_text(hjust = 0.5)),
                                   subtitle = "Took up running at the beginning of Aug; 2k, 5k and 10k are usually effort runs") 


