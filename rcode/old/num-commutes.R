# number of commutes, number of points/commute


# load libraries
library(tidyverse)
library(lubridate)

# load PM/commute data
load(here("data/roadiness_commutes.Rdata"))



# histogram of obs
rcomm2 <- select(rcomm, date, ID, cum1, obs) %>% unique()
ggplot(rcomm2, aes(x = obs)) + geom_histogram()
summary(rcomm2$obs)


# plot of select
unid <- unique(rcomm$id2)
# number of unique commutes
length(unid)


sels <- sample(unid, 4)
rcommsel <- filter(rcomm, id2 %in% sels)
ggplot(rcommsel, aes(x = row, y= PM)) + geom_point()+ geom_smooth(se = F) +facet_wrap(~id2)
