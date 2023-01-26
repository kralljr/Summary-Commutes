# in-vehicle humidity

library(here)
library(tidyverse)

# load RTI data
load(here("data/pm-cont-data-1minrti.RData"))

# save RH
inrh <- dplyr::select(datall, rdatetime, ID, rtiRH) %>%
  unique()


save(inrh, file = here("data/inrh.RData"))
