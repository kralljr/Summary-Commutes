# check commutes
load(here("data/rcomm2.RData"))
load(here("data/pm-final.RData"))


# dat <- rcomm2
dat <- pmf

group_by(pmf, ID, date) %>%
  arrange(ID, date, rdatetime) %>%
  mutate(lag1 = lag(rdatetime),
         diff = as.numeric(rdatetime - lag1),
         diff = ifelse(is.na(lag1), 0, diff - 1),
         cum1 = cumsum(diff)) %>%
  ungroup() %>%
  group_by(ID, date, cum1) %>%
  mutate(obs = n()) %>%
  dplyr::select(-c(diff, lag1)) %>%
  mutate(id2 = paste(ID, date, cum1), row = row_number()) %>%
  # commutes of at least 15 minutes
  filter(obs <15)
