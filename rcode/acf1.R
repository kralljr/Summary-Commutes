# check autocorrelation
# number of commutes, number of points/commute


# load libraries
library(tidyverse)
library(lubridate)
library(purrr)
library(here)

# load PM/commute data
load(here("data/roadiness_commutes.Rdata"))

# function to find acfs
getacf <- function(dat) {
  acf1 <- with(dat, acf(PM, plot = F))
  acf1 <- data.frame(lag = acf1$lag, acf = acf1$acf)
  acf1 <- dplyr::filter(acf1, abs(acf) > 0.3)
}


# find acfs for each
rcomm1 <- select(rcomm, id2, rdatetime, PM) %>%
  nest(data = c(rdatetime, PM)) %>%
  slice(1 : 69) %>%
  mutate(acfg= purrr::map(data, ~getacf(dat =.))) %>%
  select(-data) %>%
  unnest(acfg) %>%
  filter(lag != 0)


c1 <- count(rcomm1, lag)
group_by(rcomm1, lag) %>%
  summarize(mean(acf), max(acf), min(acf), median(acf)) %>%
  full_join(c1)

sort(unique(rcomm$obs))
max1 <- filter(rcomm, obs == 59)
max1 <- filter(rcomm, obs == 58)
max1 <- filter(rcomm, obs == 45)
max1 <- filter(rcomm, obs == 44)

ggplot(max1, aes(x = row, y= PM)) + geom_point()+ geom_smooth(se = F) +facet_wrap(~id2)

max1 <- filter(max1, id2 == max1$id2[1])
acf(max1$PM)

StructTS(max1$PM)
ts1 <- StructTS(x = max1$PM)
acf(ts1$residuals)

