# road types
library(here)
library(readxl)
library(tidyverse)

dat <- read_excel(here("data/universal_frc_v1.0.xlsx"), sheet = 3)

dat <- data.frame(dat) %>% dplyr::select(1 : 2) %>%
  unique() %>% na.omit()
