library(tidyverse)
library(here)


load(here("data/rcomm2.RData"))
rcomm1 <- rcommLM


iqrs <- dplyr::select(rcomm1, -any_of(c("PM", "lPM", "ID", "date_local", "rdatetime",
                                        "group",   "prcpbin", "snowbin", "rtype", "id3", "cat5sm"))) %>%
  pivot_longer(srness : RH) %>%
  group_by(name)

iqrs <- iqrs %>%
  summarize(IQR = IQR(value)) %>%
  mutate(IQR = ifelse(name %in% c("snowbin", "prcpbin"), 1, IQR),
         # fix speed
         IQR = ifelse(name == "mph", 10, IQR)) %>%
  rename(term = name)


save(iqrs, file = here("data/iqrs.RData"))
