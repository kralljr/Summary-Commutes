# check lpm
library(tidyverse)

load(here("data/rcomm2.RData"))

dat <- dplyr::select(rcommLM, ID, id3, lPM, PM) %>%
  mutate(lPM1 = log(PM + 0.01), lPM2 = ifelse(PM == 0, 0.09, PM),
         lPM2 = log(lPM2)) %>%
  pivot_longer(-c(ID, id3))



ggplot(dat, aes(x = value)) + geom_histogram() +
  facet_wrap(~name, scales = "free")
